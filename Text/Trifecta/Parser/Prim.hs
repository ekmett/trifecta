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
  , parseTest
  , manyAccum
  ) where


import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Writer.Class
import Control.Monad
import qualified Data.Functor.Plus as Plus
import Data.Functor.Plus hiding (some, many)
import Data.Semigroup
import Data.Foldable
import Data.Functor.Bind (Apply(..), Bind((>>-)))
import Data.IntervalMap.FingerTree (Interval(..))
import qualified Data.IntervalMap.FingerTree as IntervalMap
import Data.Set as Set hiding (empty, toList)
import Data.ByteString as Strict hiding (empty)
import Data.Sequence as Seq hiding (empty)
import Data.ByteString.UTF8 as UTF8
import Text.PrettyPrint.Free hiding (line)
import Text.Trifecta.Rope.Delta as Delta
import Text.Trifecta.Rope.Prim
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Err
import Text.Trifecta.Diagnostic.Err.State
import Text.Trifecta.Diagnostic.Err.Log
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.Trifecta.Diagnostic.Rendering.Caret
import Text.Trifecta.Highlighter.Class
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.It
import Text.Trifecta.Parser.Step
import Text.Trifecta.Parser.Result
import System.Console.Terminfo.PrettyPrint

data Parser e a = Parser 
  { unparser :: forall r.
    (a -> ErrState e -> ErrLog e -> Bool -> Delta -> ByteString -> It Rope r) -> -- uncommitted ok
    (     ErrState e -> ErrLog e -> Bool -> Delta -> ByteString -> It Rope r) -> -- uncommitted err
    (a -> ErrState e -> ErrLog e -> Bool -> Delta -> ByteString -> It Rope r) -> -- committed ok
    (     ErrState e -> ErrLog e -> Bool -> Delta -> ByteString -> It Rope r) -> -- committed err
                        ErrLog e -> Bool -> Delta -> ByteString -> It Rope r
  } 

instance Functor (Parser e) where
  fmap f (Parser m) = Parser $ \ eo ee co -> m (eo . f) ee (co . f)
  {-# INLINE fmap #-}
  a <$ Parser m = Parser $ \ eo ee co -> m (\_ -> eo a) ee (\_ -> co a)
  {-# INLINE (<$) #-}

instance Apply (Parser e) where (<.>) = (<*>)
instance Applicative (Parser e) where
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

instance Alt (Parser e) where 
  (<!>) = (<|>)
  many p = Prelude.reverse <$> manyAccum (:) p
  some p = p *> many p
instance Plus (Parser e) where zero = empty
instance Alternative (Parser e) where
  empty = Parser $ \_ ee _ _ -> ee mempty
  {-# INLINE empty #-}
  -- Parser m <|> Parser n = Parser $ \ eo ee co ce -> m eo (n eo ee co ce) co ce
  Parser m <|> Parser n = Parser $ \ eo ee co ce -> 
    m eo (\e -> n (\a e'-> eo a (e <> e')) (\e' -> ee (e <> e')) co ce) 
      co ce
  {-# INLINE (<|>) #-}
  many p = Prelude.reverse <$> manyAccum (:) p
  {-# INLINE many #-}
  some p = (:) <$> p <*> many p

instance Semigroup (Parser e a) where
  (<>) = (<|>) 

instance Monoid (Parser e a) where
  mappend = (<|>)
  mempty = empty

instance Bind (Parser e) where (>>-) = (>>=)
instance Monad (Parser e) where
  return a = Parser $ \ eo _ _ _ -> eo a mempty
  {-# INLINE return #-}
  Parser m >>= k = Parser $ \ eo ee co ce -> 
    m (\a e -> unparser (k a) (\b e' -> eo b (e <> e')) (\e' -> ee (e <> e')) co ce) ee 
      (\a e -> unparser (k a) (\b e' -> co b (e <> e')) (\e' -> ce (e <> e')) co ce) ce
  {-# INLINE (>>=) #-}
  (>>) = (*>) 
  {-# INLINE (>>) #-}
  fail s = Parser $ \ _ ee _ _ -> ee mempty { errMessage = FailErr s }
  {-# INLINE fail #-}


instance MonadPlus (Parser e) where
  mzero = empty
  mplus = (<|>) 

instance MonadWriter (ErrLog e) (Parser e) where
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
manyAccum :: (a -> [a] -> [a]) -> Parser e a -> Parser e [a]
manyAccum acc (Parser p) = Parser $ \eo _ co ce -> 
  let walk xs x _ = p manyErr (\_ -> co (acc x xs) mempty) (walk (acc x xs)) ce
      -- NB: to squelch this error you can change your grammar to add a commit, if you are progressing
      -- in some other fashion
      manyErr _ e = ce e { errMessage = PanicErr "'many' applied to a parser that accepted an empty string" }
  in p manyErr (eo []) (walk []) ce
   
instance MonadDiagnostic e (Parser e) where
  fatalWith ds rs e = Parser $ \_ _ _ ce -> 
    ce mempty { errMessage = Err rs Fatal e ds }
  errWith ds rs e = Parser $ \_ ee _ _ -> 
    ee mempty { errMessage = Err rs Error e ds }
  logWith v ds rs e = Parser $ \eo _ _ _ l b8 d bs -> 
    eo () mempty l { errLog = errLog l |> Diagnostic (Right $ addCaret d $ Prelude.foldr (<>) (rendering d bs) rs) v e ds } b8 d bs
    
instance MonadError (ErrState e) (Parser e) where
  throwError m = Parser $ \_ ee _ _ -> ee m
  {-# INLINE throwError #-}
  catchError (Parser m) k = Parser $ \ eo ee co ce -> 
    m eo (\e -> unparser (k e) eo ee co ce) co ce
  {-# INLINE catchError #-}

ascii :: ByteString -> Bool
ascii = Strict.all (<=0x7f) 

instance MonadParser (Parser e) where
  -- commit (Parser m) = Parser $ \ _ _ co ce -> m co ce co ce
  try (Parser m) = Parser $ \ eo ee co ce -> m eo ee co $ 
    \e -> if fatalErr (errMessage e) then ce e else ee e
  {-# INLINE try #-}
  highlightToken t (Parser m) = Parser $ \eo ee co ce l b8 d bs -> 
    m eo ee (\a e l' b8' d' -> co a e l' { errHighlights = IntervalMap.insert (Interval d d') t (errHighlights l') } b8' d') ce l b8 d bs

  unexpected s = Parser $ \ _ ee _ _ -> ee mempty { errMessage = FailErr $ "unexpected " ++ s }

  labels (Parser p) msgs = Parser $ \ eo ee -> p
     (\a e -> eo a $ if knownErr (errMessage e)
                     then e { errExpected = msgs `union` errExpected e }
                     else e)
     (\e -> ee e { errExpected = msgs })
  {-# INLINE labels #-}
  liftIt m = Parser $ \ eo _ _ _ l b8 d bs -> do 
     a <- m
     eo a mempty l b8 d bs
  {-# INLINE liftIt #-}
  mark = Parser $ \eo _ _ _ l b8 d -> eo d mempty l b8 d
  {-# INLINE mark #-}
  release d' = Parser $ \_ ee co _ l b8 d bs -> do
    mbs <- rewindIt d'
    case mbs of
      Just bs' -> co () mempty l (ascii bs') d' bs'
      Nothing 
        | bytes d' == bytes (rewind d) + Strict.length bs -> if near d d' 
            then co () mempty l (ascii bs) d' bs
            else co () mempty l True d' mempty
        | otherwise -> ee mempty l b8 d bs
  line = Parser $ \eo _ _ _ l b8 d bs -> eo bs mempty l b8 d bs
  {-# INLINE line #-}
  skipMany p = () <$ manyAccum (\_ _ -> []) p
  {-# INLINE skipMany #-}
  satisfy f = Parser $ \ _ ee co _ l b8 d bs ->
    if b8 -- fast path
    then let b = columnByte d in (
         if b >= 0 && b < Strict.length bs 
         then case toEnum $ fromEnum $ Strict.index bs b of
           c | not (f c)                 -> ee mempty l b8 d bs
             | b == Strict.length bs - 1 -> let !ddc = d <> delta c
                                            in join $ fillIt ( if c == '\n'
                                                               then co c mempty l True ddc mempty 
                                                               else co c mempty l b8 ddc bs )
                                                             (\d' bs' -> co c mempty l (ascii bs') d' bs') 
                                                             ddc
             | otherwise                 -> co c mempty l b8 (d <> delta c) bs
         else ee mempty { errMessage = FailErr "unexpected EOF" } l b8 d bs)
    else case UTF8.uncons $ Strict.drop (columnByte d) bs of
      Nothing             -> ee mempty { errMessage = FailErr "unexpected EOF" } l b8 d bs
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
    if b >= 0 && b < Strict.length bs 
    then case toEnum $ fromEnum $ Strict.index bs b of
      c | not (f c)                 -> ee mempty l b8 d bs
        | b == Strict.length bs - 1 -> let !ddc = d <> delta c
                                       in join $ fillIt ( if c == 10
                                                          then co c mempty l True ddc mempty
                                                          else co c mempty l b8 ddc bs )
                                                        (\d' bs' -> co c mempty l (ascii bs') d' bs') 
                                                        ddc
        | otherwise                 -> co c mempty l b8 (d <> delta c) bs
    else ee mempty { errMessage = FailErr "unexpected EOF" } l b8 d bs

instance MonadHighlighter (Parser e) where
  highlights = Parser $ \eo _ _ _ l -> eo (errHighlights l) mempty l

data St e a = JuSt a !(ErrState e) !(ErrLog e) !Bool !Delta !ByteString
            | NoSt !(ErrState e) !(ErrLog e) !Bool !Delta !ByteString

stepParser :: (Diagnostic e -> Diagnostic t) -> 
              (ErrState e -> Bool -> Delta -> ByteString -> Diagnostic t) ->
              Parser e a -> ErrLog e -> Bool -> Delta -> ByteString -> Step t a
stepParser yl y (Parser p) l0 b80 d0 bs0 = 
  go mempty $ p ju no ju no l0 b80 d0 bs0
  where
    ju a e l b8 d bs = Pure (JuSt a e l b8 d bs)
    no e l b8 d bs   = Pure (NoSt e l b8 d bs)
    go r (Pure (JuSt a _ l _ _ _)) = StepDone r (yl <$> errLog l) a
    go r (Pure (NoSt e l b8 d bs)) = StepFail r ((yl <$> errLog l) |> y e b8 d bs)
    go r (It ma k) = StepCont r (case ma of
                                   JuSt a _ l _ _ _  -> Success (yl <$> errLog l) a
                                   NoSt e l b8 d bs  -> Failure ((yl <$> errLog l) |> y e b8 d bs)) 
                                (go <*> k)

why :: Pretty e => (e -> Doc t) -> ErrState e -> Bool -> Delta -> ByteString -> Diagnostic (Doc t)
why pp (ErrState ss m) _ d bs 
  | Set.null ss = explicateWith empty m 
  | knownErr m  = explicateWith (char ',' <+> ex) m
  | otherwise   = Diagnostic r Error ex []
  where
    ex = text "expected:" <+> fillSep (punctuate (char ',') $ text <$> toList ss) -- TODO: oxford comma, "or" etc...
    r = Right $ addCaret d $ rendering d bs
    explicateWith x EmptyErr        = Diagnostic r  Error ((text "unspecified error") <> x)  []
    explicateWith x (FailErr s)     = Diagnostic r  Error ((fillSep $ text <$> words s) <> x) []
    explicateWith x (PanicErr s)    = Diagnostic r  Panic ((fillSep $ text <$> words s) <> x) []
    explicateWith x (Err rs l e es) = Diagnostic r' l (pp e <> x) (fmap (fmap pp) es)
      where r' = Right $ addCaret d $ Prelude.foldr (<>) (rendering d bs) rs

parseTest :: Show a => Parser String a -> String -> IO ()
parseTest p s = case starve 
                   $ feed (UTF8.fromString s) 
                   $ stepParser (fmap prettyTerm) (why prettyTerm) (release mempty *> p) mempty True mempty mempty of
  Failure xs -> displayLn $ toList xs
  Success xs a -> do
    unless (Seq.null xs) $ displayLn $ toList xs
    print a
