{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2011-2015
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Text.Trifecta.Parser
  ( Parser(..)
  , manyAccum
  , race
  -- * Feeding a parser more more input
  , Step(..)
  , feed
  , starve
  , stepParser
  , stepResult
  , stepIt
  -- * Parsing
  , parseFromFile
  , parseFromFileEx
  , parseString
  , parseByteString
  , parseTest
  ) where

import Control.Applicative as Alternative
import Control.Lens ((%~), (&))
import Control.Monad (MonadPlus(..), ap, join)
import Control.Monad.IO.Class
import Data.ByteString as Strict hiding (empty, snoc)
import Data.ByteString.UTF8 as UTF8
import Data.Maybe (isJust)
import Data.Semigroup
import Data.Semigroup.Reducer
-- import Data.Sequence as Seq hiding (empty)
import Data.Set as Set hiding (empty, toList)
import System.IO
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Parser.Token
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty)
import Text.Trifecta.Combinators
import Text.Trifecta.Instances ()
import Text.Trifecta.Rendering
import Text.Trifecta.Result
import Text.Trifecta.Rope
import Text.Trifecta.Delta as Delta
import Text.Trifecta.Util.It

newtype Parser a = Parser
  { unparser :: forall r.
    (a -> Err -> It Rope r) ->
    (Err -> It Rope r) ->
    (a -> Set String -> Delta -> ByteString -> It Rope r) -> -- committed success
    (Delta -> ErrInfo -> Err -> It Rope r) ->                -- committed err
    Delta -> ByteString -> It Rope r
  }

instance Functor Parser where
  fmap f (Parser m) = Parser $ \ eo ee co -> m (eo . f) ee (co . f)
  {-# INLINE fmap #-}
  a <$ Parser m = Parser $ \ eo ee co -> m (\_ -> eo a) ee (\_ -> co a)
  {-# INLINE (<$) #-}

instance Applicative Parser where
  pure a = Parser $ \ eo _ _ _ _ _ -> eo a mempty
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty = Parser $ \_ ee _ _ _ _ -> ee mempty
  {-# INLINE empty #-}
  Parser m <|> Parser n = Parser $ \ eo ee co ce d bs ->
    m eo (\e -> n (\a e' -> eo a (e <> e')) (\e' -> ee (e <> e')) co ce d bs) co ce d bs
  {-# INLINE (<|>) #-}
  many p = Prelude.reverse <$> manyAccum (:) p
  {-# INLINE many #-}
  some p = (:) <$> p <*> Alternative.many p

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Parser a) where
  mappend = liftA2 mappend
  {-# INLINE mappend #-}
  mempty = pure mempty
  {-# INLINE mempty #-}

instance Monad Parser where
  return a = Parser $ \ eo _ _ _ _ _ -> eo a mempty
  {-# INLINE return #-}
  Parser m >>= k = Parser $ \ eo ee co ce d bs ->
    m (\a e -> unparser (k a) (\b e' -> eo b (e <> e')) (\e' -> ee (e <> e')) co ce d bs) ee
      (\a es d' bs' -> unparser (k a)
         (\b e' -> co b (es <> _expected e') d' bs')
         (\e ->
           let errDoc = explain (renderingCaret d' bs') e { _expected = _expected e <> es }
               errDelta = _finalDeltas e
           in  ce d' (ErrInfo errDoc (d' : errDelta)) e
         )
         co ce d' bs') ce d bs
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  fail s = Parser $ \ _ ee _ _ _ _ -> ee (failed s)
  {-# INLINE fail #-}

instance MonadPlus Parser where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

manyAccum :: (a -> [a] -> [a]) -> Parser a -> Parser [a]
manyAccum f (Parser p) = Parser $ \eo _ co ce d bs ->
  let walk xs x es d' bs' = p (manyErr d' bs') (\e -> co (f x xs) (_expected e <> es) d' bs') (walk (f x xs)) ce d' bs'
      manyErr d' bs' _ e  = ce d' (ErrInfo errDoc [d']) e
        where errDoc = explain (renderingCaret d' bs') (e <> failed "'many' applied to a parser that accepted an empty string")
  in p (manyErr d bs) (eo []) (walk []) ce d bs

liftIt :: It Rope a -> Parser a
liftIt m = Parser $ \ eo _ _ _ _ _ -> do
  a <- m
  eo a mempty
{-# INLINE liftIt #-}

instance Parsing Parser where
  try (Parser m) = Parser $ \ eo ee co _ -> m eo ee co (\_ _ _ -> ee mempty)
  {-# INLINE try #-}
  Parser m <?> nm = Parser $ \ eo ee -> m
     (\a e -> eo a (if isJust (_reason e) then e { _expected = Set.singleton nm } else e))
     (\e -> ee e { _expected = Set.singleton nm })
  {-# INLINE (<?>) #-}
  skipMany p = () <$ manyAccum (\_ _ -> []) p
  {-# INLINE skipMany #-}
  unexpected s = Parser $ \ _ ee _ _ _ _ -> ee $ failed $ "unexpected " ++ s
  {-# INLINE unexpected #-}
  eof = notFollowedBy anyChar <?> "end of input"
  {-# INLINE eof #-}
  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))
  {-# INLINE notFollowedBy #-}

race :: Parser a -> Parser a
race (Parser m) = Parser $ \eo ee co ce -> m eo ee co $ \d ei err ->
  ee $ mempty
     & expected    %~ Set.union (_expected err)
     & finalDeltas %~ (++) (_errDeltas ei)
{-# INLINE race #-}

instance Errable Parser where
  raiseErr e = Parser $ \ _ ee _ _ _ _ -> ee e
  {-# INLINE raiseErr #-}

instance LookAheadParsing Parser where
  lookAhead (Parser m) = Parser $ \eo ee _ -> m eo ee (\a _ _ _ -> eo a mempty)
  {-# INLINE lookAhead #-}

instance CharParsing Parser where
  satisfy f = Parser $ \ _ ee co _ d bs ->
    case UTF8.uncons $ Strict.drop (fromIntegral (columnByte d)) bs of
      Nothing        -> ee (failed "unexpected EOF")
      Just (c, xs)
        | not (f c)       -> ee mempty
        | Strict.null xs  -> let !ddc = d <> delta c
                             in join $ fillIt (co c mempty ddc (if c == '\n' then mempty else bs))
                                              (co c mempty)
                                              ddc
        | otherwise       -> co c mempty (d <> delta c) bs
  {-# INLINE satisfy #-}

instance TokenParsing Parser

instance DeltaParsing Parser where
  line = Parser $ \eo _ _ _ _ bs -> eo bs mempty
  {-# INLINE line #-}
  position = Parser $ \eo _ _ _ d _ -> eo d mempty
  {-# INLINE position #-}
  rend = Parser $ \eo _ _ _ d bs -> eo (rendered d bs) mempty
  {-# INLINE rend #-}
  slicedWith f p = do
    m <- position
    a <- p
    r <- position
    f a <$> liftIt (sliceIt m r)
  {-# INLINE slicedWith #-}

instance MarkParsing Delta Parser where
  mark = position
  {-# INLINE mark #-}
  release d' = Parser $ \_ ee co _ d bs -> do
    mbs <- rewindIt d'
    case mbs of
      Just bs' -> co () mempty d' bs'
      Nothing
        | bytes d' == bytes (rewind d) + fromIntegral (Strict.length bs) -> if near d d'
            then co () mempty d' bs
            else co () mempty d' mempty
        | otherwise -> ee mempty

data Step a
  = StepDone !Rope a
  | StepFail !Rope ErrInfo
  | StepCont !Rope (Result a) (Rope -> Step a)

instance Show a => Show (Step a) where
  showsPrec d (StepDone r a) = showParen (d > 10) $
    showString "StepDone " . showsPrec 11 r . showChar ' ' . showsPrec 11 a
  showsPrec d (StepFail r xs) = showParen (d > 10) $
    showString "StepFail " . showsPrec 11 r . showChar ' ' . showsPrec 11 xs
  showsPrec d (StepCont r fin _) = showParen (d > 10) $
    showString "StepCont " . showsPrec 11 r . showChar ' ' . showsPrec 11 fin . showString " ..."

instance Functor Step where
  fmap f (StepDone r a)    = StepDone r (f a)
  fmap _ (StepFail r xs)   = StepFail r xs
  fmap f (StepCont r z k)  = StepCont r (fmap f z) (fmap f . k)

feed :: Reducer t Rope => t -> Step r -> Step r
feed t (StepDone r a)    = StepDone (snoc r t) a
feed t (StepFail r xs)   = StepFail (snoc r t) xs
feed t (StepCont r _ k)  = k (snoc r t)
{-# INLINE feed #-}

starve :: Step a -> Result a
starve (StepDone _ a)    = Success a
starve (StepFail _ xs)   = Failure xs
starve (StepCont _ z _)  = z
{-# INLINE starve #-}

stepResult :: Rope -> Result a -> Step a
stepResult r (Success a)  = StepDone r a
stepResult r (Failure xs) = StepFail r xs
{-# INLINE stepResult #-}

stepIt :: It Rope a -> Step a
stepIt = go mempty where
  go r (Pure a) = StepDone r a
  go r (It a k) = StepCont r (pure a) $ \s -> go s (k s)
{-# INLINE stepIt #-}

data Stepping a
  = EO a Err
  | EE Err
  | CO a (Set String) Delta ByteString
  | CE Delta ErrInfo Err

stepParser :: Parser a -> Delta -> ByteString -> Step a
stepParser (Parser p) d0 bs0 = go mempty $ p eo ee co ce d0 bs0 where
  eo a e       = Pure (EO a e)
  ee e         = Pure (EE e)
  co a es d bs = Pure (CO a es d bs)
  ce d errInf e = Pure (CE d errInf e)
  go r (Pure (EO a _))     = StepDone r a
  go r (Pure (EE e))       = StepFail r $
                              let errDoc = explain (renderingCaret d0 bs0) e
                              in  ErrInfo errDoc (_finalDeltas e)
  go r (Pure (CO a _ _ _)) = StepDone r a
  go r (Pure (CE _ ei _))  = StepFail r ei
  go r (It ma k)           = StepCont r (case ma of
                                EO a _     -> Success a
                                EE e       -> Failure $
                                  ErrInfo (explain (renderingCaret d0 bs0) e) (d0 : _finalDeltas e)
                                CO a _ _ _ -> Success a
                                CE _ ei _  -> Failure ei
                              ) (go <*> k)
{-# INLINE stepParser #-}

-- | @parseFromFile p filePath@ runs a parser @p@ on the
-- input read from @filePath@ using 'ByteString.readFile'. All diagnostic messages
-- emitted over the course of the parse attempt are shown to the user on the console.
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Nothing -> return ()
-- >     Just a  -> print $ sum a
parseFromFile :: MonadIO m => Parser a -> String -> m (Maybe a)
parseFromFile p fn = do
  result <- parseFromFileEx p fn
  case result of
   Success a  -> return (Just a)
   Failure xs -> do
     liftIO $ displayIO stdout $ renderPretty 0.8 80 $ (_errDoc xs) <> linebreak
     return Nothing

-- | @parseFromFileEx p filePath@ runs a parser @p@ on the
-- input read from @filePath@ using 'ByteString.readFile'. Returns all diagnostic messages
-- emitted over the course of the parse and the answer if the parse was successful.
--
-- > main = do
-- >   result <- parseFromFileEx (many number) "digits.txt"
-- >   case result of
-- >     Failure xs -> displayLn xs
-- >     Success a  -> print (sum a)
-- >

parseFromFileEx :: MonadIO m => Parser a -> String -> m (Result a)
parseFromFileEx p fn = do
  s <- liftIO $ Strict.readFile fn
  return $ parseByteString p (Directed (UTF8.fromString fn) 0 0 0 0) s

-- | @parseByteString p delta i@ runs a parser @p@ on @i@.

parseByteString :: Parser a -> Delta -> UTF8.ByteString -> Result a
parseByteString p d inp = starve $ feed inp $ stepParser (release d *> p) mempty mempty

parseString :: Parser a -> Delta -> String -> Result a
parseString p d inp = starve $ feed inp $ stepParser (release d *> p) mempty mempty

parseTest :: (MonadIO m, Show a) => Parser a -> String -> m ()
parseTest p s = case parseByteString p mempty (UTF8.fromString s) of
  Failure xs -> liftIO $ displayIO stdout $ renderPretty 0.8 80 $ (_errDoc xs) <> linebreak -- TODO: retrieve columns
  Success a  -> liftIO (print a)
