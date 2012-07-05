{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, Rank2Types, FlexibleInstances, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Prim
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Prim
  ( Parser(..)
  , why
  , stepParser
  , manyAccum
  ) where


import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Writer.Class
import Control.Monad.Cont.Class
import Control.Monad
import Control.Comonad
import qualified Data.Functor.Plus as Plus
import Data.Functor.Plus hiding (some, many)
import Data.Function
import Data.Semigroup
import Data.Foldable
import qualified Data.List as List
import Data.Functor.Bind (Bind((>>-)))
import qualified Text.Trifecta.IntervalMap as IntervalMap
import Data.Set as Set hiding (empty, toList)
import Data.ByteString as Strict hiding (empty)
import Data.Sequence as Seq hiding (empty)
import Data.ByteString.UTF8 as UTF8
import Text.PrettyPrint.Free hiding (line)
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Err
import Text.Trifecta.Diagnostic.Err.State
import Text.Trifecta.Diagnostic.Err.Log
import Text.Trifecta.Diagnostic.Rendering.Caret
import Text.Trifecta.Highlight.Class
import Text.Trifecta.Highlight.Prim
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.It
import Text.Trifecta.Parser.Mark
import Text.Trifecta.Parser.Step
import Text.Trifecta.Parser.Result
import Text.Trifecta.Rope.Delta as Delta
import Text.Trifecta.Rope.Prim
import Text.Trifecta.Rope.Bytes

data Parser r e a = Parser
  { unparser ::
    (a -> ErrState e -> ErrLog e -> Bool -> Delta -> ByteString -> It Rope r) -> -- uncommitted ok
    (     ErrState e -> ErrLog e -> Bool -> Delta -> ByteString -> It Rope r) -> -- uncommitted err
    (a -> ErrState e -> ErrLog e -> Bool -> Delta -> ByteString -> It Rope r) -> -- committed ok
    (     ErrState e -> ErrLog e -> Bool -> Delta -> ByteString -> It Rope r) -> -- committed err
                        ErrLog e -> Bool -> Delta -> ByteString -> It Rope r
  }

instance Functor (Parser r e) where
  fmap f (Parser m) = Parser $ \ eo ee co -> m (eo . f) ee (co . f)
  {-# INLINE fmap #-}
  a <$ Parser m = Parser $ \ eo ee co -> m (\_ -> eo a) ee (\_ -> co a)
  {-# INLINE (<$) #-}

instance Apply (Parser r e) where (<.>) = (<*>)
instance Applicative (Parser r e) where
  pure a = Parser $ \ eo _ _ _ -> eo a mempty
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
{-
  Parser m <*> Parser n = Parser $ \ eo ee co ce ->
    m (\f e -> n (\a e' -> eo (f a) (e <> e')) ee (\a e' -> co (f a) (e <> e')) ce) ee
      (\f e -> n (\a e' -> co (f a) (e <> e')) ce (\a e' -> co (f a) (e <> e')) ce) ce
  {-# INLINE (<*>) #-}
  Parser m <* Parser n = Parser $ \ eo ee co ce ->
    m (\a e -> n (\_ e' -> eo a (e <> e')) ee (\_ e' -> co a (e <> e')) ce) ee
      (\a e -> n (\_ e' -> co a (e <> e')) ce (\_ e' -> co a (e <> e')) ce) ce
  {-# INLINE (<*) #-}
  Parser m *> Parser n = Parser $ \ eo ee co ce ->
    m (\_ e -> n (\a e' -> eo a (e <> e')) ee (\a e' -> co a (e <> e')) ce) ee
      (\_ e -> n (\a e' -> co a (e <> e')) ce (\a e' -> co a (e <> e')) ce) ce
  {-# INLINE (*>) #-}
-}

instance Alt (Parser r e) where
  (<!>) = (<|>)
  many p = Prelude.reverse <$> manyAccum (:) p
  some p = p *> many p
instance Plus (Parser r e) where zero = empty
instance Alternative (Parser r e) where
  empty = Parser $ \_ ee _ _ -> ee mempty
  {-# INLINE empty #-}
  Parser m <|> Parser n = Parser $ \ eo ee co ce ->
    m eo (\e -> n (\a e'-> eo a (e <> e')) (\e' -> ee (e <> e')) co ce)
      co ce
  {-# INLINE (<|>) #-}
  many p = Prelude.reverse <$> manyAccum (:) p
  {-# INLINE many #-}
  some p = (:) <$> p <*> many p

instance Semigroup (Parser r e a) where
  (<>) = (<|>)

instance Monoid (Parser r e a) where
  mappend = (<|>)
  mempty = empty

instance Bind (Parser r e) where (>>-) = (>>=)
instance Monad (Parser r e) where
  return a = Parser $ \ eo _ _ _ -> eo a mempty
  {-# INLINE return #-}
  Parser m >>= k = Parser $ \ eo ee co ce ->
    m (\a e -> unparser (k a) (\b e' -> eo b (e <> e')) (\e' -> ee (e <> e')) co ce) ee
      (\a e -> unparser (k a) (\b e' -> co b (e <> e')) (\e' -> ce (e <> e')) co ce) ce
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  fail s = Parser $ \ _ ee _ _ l b8 d bs -> ee mempty { errMessage = FailErr (renderingCaret d bs) s } l b8 d bs
  {-# INLINE fail #-}


instance MonadPlus (Parser r e) where
  mzero = empty
  mplus = (<|>)

instance MonadWriter (ErrLog e) (Parser r e) where
  tell w = Parser $ \eo _ _ _ l -> eo () mempty (l <> w)
  {-# INLINE tell #-}
  listen (Parser m) = Parser $ \eo ee co ce l ->
    m (\ a e' l' -> eo (a,l') e' (l <> l'))
      (\   e' l' -> ee        e' (l <> l'))
      (\ a e' l' -> co (a,l') e' (l <> l'))
      (\   e' l' -> ce        e' (l <> l'))
      mempty
  {-# INLINE listen #-}
  pass (Parser m) = Parser $ \eo ee co ce l ->
    m (\(a,p) e' l' -> eo a e' (l <> p l'))
      (\      e' l' -> ee   e' (l <>   l'))
      (\(a,p) e' l' -> co a e' (l <> p l'))
      (\      e' l' -> ce   e' (l <>   l'))
      mempty
  {-# INLINE pass #-}

manyAccum :: (a -> [a] -> [a]) -> Parser r e a -> Parser r e [a]
manyAccum acc (Parser p) = Parser $ \eo _ co ce ->
  let walk xs x _ = p manyErr (\_ -> co (acc x xs) mempty) (walk (acc x xs)) ce
      manyErr _ e l b8 d bs = ce e { errMessage = PanicErr (renderingCaret d bs) "'many' applied to a parser that accepted an empty string" } l b8 d bs
  in p manyErr (eo []) (walk []) ce

instance MonadDiagnostic e (Parser r e) where
  throwDiagnostic e@(Diagnostic _ l _ _)
    | l == Fatal || l == Panic = Parser $ \_ _ _ ce -> ce mempty { errMessage = Err e }
    | otherwise                = Parser $ \_ ee _ _ -> ee mempty { errMessage = Err e }
  logDiagnostic d = Parser $ \eo _ _ _ l -> eo () mempty l { errLog = errLog l |> d }

instance MonadError (ErrState e) (Parser r e) where
  throwError m = Parser $ \_ ee _ _ -> ee m
  {-# INLINE throwError #-}
  catchError (Parser m) k = Parser $ \ eo ee co ce ->
    m eo (\e -> unparser (k e) eo ee co ce) co ce
  {-# INLINE catchError #-}

ascii :: ByteString -> Bool
ascii = Strict.all (<=0x7f)

liftIt :: It Rope a -> Parser r e a
liftIt m = Parser $ \ eo _ _ _ l b8 d bs -> do
  a <- m
  eo a mempty l b8 d bs
{-# INLINE liftIt #-}

instance MonadParser (Parser r e) where
  try (Parser m) = Parser $ \ eo ee co ce l b8 d bs -> m eo ee co (\e l' _ _ _ ->
     if fatalErr (errMessage e)
     then ce e (l <> l') b8 d bs
     else ee e (l <> l') b8 d bs
     ) l b8 d bs
  {-# INLINE try #-}
  highlightInterval h s e = Parser $ \eo _ _ _ l -> eo () mempty l { errHighlights = IntervalMap.insert s e h (errHighlights l) }
  {-# INLINE highlightInterval #-}

  skipping d = do
    m <- mark
    release $ m <> d
  {-# INLINE skipping #-}

  unexpected s = Parser $ \ _ ee _ _ l b8 d bs -> ee mempty { errMessage = FailErr (renderingCaret d bs) $  "unexpected " ++ s } l b8 d bs
  {-# INLINE unexpected #-}

  labels (Parser p) msgs = Parser $ \ eo ee -> p
     (\a e l b8 d bs ->
       eo a (if knownErr (errMessage e)
             then e { errExpected = Set.fromList (Prelude.map (:^ Caret d bs) msgs) `union` errExpected e }
             else e) l b8 d bs)
     (\e l b8 d bs -> ee e { errExpected = Set.fromList $ Prelude.map (:^ Caret d bs) msgs } l b8 d bs)
  {-# INLINE labels #-}
  line = Parser $ \eo _ _ _ l b8 d bs -> eo bs mempty l b8 d bs
  {-# INLINE line #-}
  skipMany p = () <$ manyAccum (\_ _ -> []) p
  {-# INLINE skipMany #-}
  satisfy f = Parser $ \ _ ee co _ l b8 d bs ->
    if b8 -- fast path
    then let b = columnByte d in (
         if b >= 0 && b < fromIntegral (Strict.length bs)
         then case toEnum $ fromEnum $ Strict.index bs (fromIntegral b) of
           c | not (f c)                 -> ee mempty l b8 d bs
             | b == fromIntegral (Strict.length bs) - 1 -> let !ddc = d <> delta c
                                            in join $ fillIt ( if c == '\n'
                                                               then co c mempty l True ddc mempty
                                                               else co c mempty l b8 ddc bs )
                                                             (\d' bs' -> co c mempty l (ascii bs') d' bs')
                                                             ddc
             | otherwise                 -> co c mempty l b8 (d <> delta c) bs
         else ee mempty { errMessage = FailErr (renderingCaret d bs) "unexpected EOF" } l b8 d bs)
    else case UTF8.uncons $ Strict.drop (fromIntegral (columnByte d)) bs of
      Nothing             -> ee mempty { errMessage = FailErr (renderingCaret d bs) "unexpected EOF" } l b8 d bs
      Just (c, xs)
        | not (f c)       -> ee mempty l b8 d bs
        | Strict.null xs  -> let !ddc = d <> delta c
                             in join $ fillIt ( if c == '\n'
                                                then co c mempty l True ddc mempty
                                                else co c mempty l b8 ddc bs)
                                              (\d' bs' -> co c mempty l (ascii bs') d' bs')
                                              ddc
        | otherwise       -> co c mempty l b8 (d <> delta c) bs
  satisfy8 f = Parser $ \ _ ee co _ l b8 d bs ->
    let b = columnByte d in
    if b >= 0 && b < fromIntegral (Strict.length bs)
    then case toEnum $ fromEnum $ Strict.index bs (fromIntegral b) of
      c | not (f c)                 -> ee mempty l b8 d bs
        | b == fromIntegral (Strict.length bs - 1) -> let !ddc = d <> delta c
                                       in join $ fillIt ( if c == 10
                                                          then co c mempty l True ddc mempty
                                                          else co c mempty l b8 ddc bs )
                                                        (\d' bs' -> co c mempty l (ascii bs') d' bs')
                                                        ddc
        | otherwise                 -> co c mempty l b8 (d <> delta c) bs
    else ee mempty { errMessage = FailErr (renderingCaret d bs) "unexpected EOF" } l b8 d bs
  position = Parser $ \eo _ _ _ l b8 d -> eo d mempty l b8 d
  {-# INLINE position #-}
  slicedWith f p = do
    m <- position
    a <- p
    r <- position
    f a <$> liftIt (sliceIt m r)
  {-# INLINE slicedWith #-}
  lookAhead (Parser m) = Parser $ \eo ee _ ce l b8 d bs ->
    m eo ee (\a e l' _ _ _ -> eo a e (l <> l') b8 d bs) ce l b8 d bs
  {-# INLINE lookAhead #-}

instance MonadCont (Parser r e) where
  callCC f = Parser $ \ eo ee co ce l b8 d bs -> unparser (f (\a -> Parser $ \_ _ _ _ l' _ _ _ -> eo a mempty l' b8 d bs)) eo ee co ce l b8 d bs

instance MonadMark Delta (Parser r e) where
  mark = position
  {-# INLINE mark #-}
  release d' = Parser $ \_ ee co _ l b8 d bs -> do
    mbs <- rewindIt d'
    case mbs of
      Just bs' -> co () mempty l (ascii bs') d' bs'
      Nothing
        | bytes d' == bytes (rewind d) + fromIntegral (Strict.length bs) -> if near d d'
            then co () mempty l (ascii bs) d' bs
            else co () mempty l True d' mempty
        | otherwise -> ee mempty l b8 d bs

data St e a = JuSt a !(ErrState e) !(ErrLog e) !Bool !Delta !ByteString
            | NoSt !(ErrState e) !(ErrLog e) !Bool !Delta !ByteString

stepParser :: (Diagnostic e -> Diagnostic t) ->
              (ErrState e -> Highlights -> Bool -> Delta -> ByteString -> Diagnostic t) ->
              (forall r. Parser r e a) -> ErrLog e -> Bool -> Delta -> ByteString -> Step t a
stepParser yl y (Parser p) l0 b80 d0 bs0 =
  go mempty $ p ju no ju no l0 b80 d0 bs0
  where
    ju a e l b8 d bs = Pure (JuSt a e l b8 d bs)
    no e l b8 d bs   = Pure (NoSt e l b8 d bs)
    go r (Pure (JuSt a _ l _ _ _)) = StepDone r (yl . addHighlights (errHighlights l) <$> errLog l) a
    go r (Pure (NoSt e l b8 d bs)) = StepFail r ((yl . addHighlights (errHighlights l) <$> errLog l) |> y e (errHighlights l) b8 d bs)
    go r (It ma k) = StepCont r (case ma of
                                   JuSt a _ l _ _ _  -> Success (yl . addHighlights (errHighlights l) <$> errLog l) a
                                   NoSt e l b8 d bs  -> Failure ((yl . addHighlights (errHighlights l) <$> errLog l) |> y e (errHighlights l) b8 d bs))
                                (go <*> k)

why :: Pretty e => (e -> Doc t) -> ErrState e -> Highlights -> Bool -> Delta -> ByteString -> Diagnostic (Doc t)
why pp (ErrState ss m) hs _ d bs
  | Prelude.null now = explicateWith empty m
  | knownErr m       = explicateWith (char ',' <+> ex) m
  | otherwise        = Diagnostic rightHere Error ex notes
  where
    ex = expect now
    ignoreBlanks = go . List.nub . List.sort where
      go []   = []
      go [""] = ["space"]
      go xs   = List.filter (/= "") xs
    expect xs = text "expected:" <+> fillSep (punctuate (char ',') (Prelude.map text $ ignoreBlanks $ Prelude.map extract xs))
    (now,later) = List.partition (\x -> errLoc m == Just (delta x)) $ toList ss
    clusters = List.groupBy ((==) `on` delta) $ List.sortBy (compare `on` delta) later
    diagnoseCluster c = Diagnostic (Right $ addHighlights hs $ renderingCaret dc bsc) Note (expect c) [] where
      _ :^ Caret dc bsc = Prelude.head c
    notes = Prelude.map diagnoseCluster clusters
    rightHere = Right $ addHighlights hs $ renderingCaret d bs

    explicateWith x EmptyErr          = Diagnostic rightHere Error ((text "unspecified error") <> x)  notes
    explicateWith x (FailErr r s)     = Diagnostic (Right $ addHighlights hs r) Error ((fillSep $ text <$> words s) <> x) notes
    explicateWith x (PanicErr r s)    = Diagnostic (Right $ addHighlights hs r) Panic ((fillSep $ text <$> words s) <> x) notes
    explicateWith x (Err (Diagnostic r l e es)) = Diagnostic (addHighlights hs <$> r) l (pp e <> x) (notes ++ fmap (addHighlights hs . fmap pp) es)

    errLoc EmptyErr = Just d
    errLoc (FailErr r _) = Just $ delta r
    errLoc (PanicErr r _) = Just $ delta r
    errLoc (Err (Diagnostic (Left _)  _ _ _)) = Nothing
    errLoc (Err (Diagnostic (Right r)  _ _ _)) =  Just $ delta r
