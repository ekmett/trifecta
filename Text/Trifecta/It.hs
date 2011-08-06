{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Text.Trifecta.It 
  ( It(..)
  , getRope
  , getMeasure
  , getEof
  , getWord8
  , sliceIt
  , slice
  , sliced
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Semigroup
import Data.Monoid
import Data.Word
import Data.Interned
import Data.Foldable (toList)
import Data.FingerTree as FingerTree
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Functor.Bind
import Data.Functor.Plus
import Text.Trifecta.Cursor
import Text.Trifecta.Hunk
import Text.Trifecta.Rope as Rope
import Text.Trifecta.Delta
import Text.Parsec.Prim hiding ((<|>))

data It a 
  = Done !Rope !Bool a
  | Fail !Rope !Bool String
  | Cont (Rope -> Bool -> It a)
  
instance Show a => Show (It a) where
  showsPrec d (Done r b a) = showParen (d > 10) $ 
    showString "Done " . showsPrec 11 r . showChar ' ' . showsPrec 11 b . showChar ' ' . showsPrec 11 a
  showsPrec d (Fail r b s) = showParen (d > 10) $
    showString "Fail " . showsPrec 11 r . showChar ' ' . showsPrec 11 b . showChar ' ' . showsPrec 11 s
  showsPrec d (Cont _) = showParen (d > 10) $ showString "Cont ..."

instance Functor It where
  fmap f (Done r b a) = Done r b (f a)
  fmap _ (Fail r b s) = Fail r b s
  fmap f (Cont k) = Cont (\r b -> fmap f (k r b))

instance Apply It where
  (<.>) = (<*>) 

instance Applicative It where
  pure = Done mempty False
  (<*>) = ap

instance Alt It where
  Fail r b _ <!> Cont k = k r b
  Fail r b _ <!> Done _ _ a = Done r b a
  Fail r b _ <!> Fail _ _ s = Fail r b s
  m <!> _ = m

instance Alternative It where
  (<|>) = (<!>)
  empty = fail "empty"

instance Bind It where
  (>>-) = (>>=)

instance Monad It where
  return = Done mempty False
  Done (Rope _ t) False a >>= f | FingerTree.null t = f a
  Done h e a >>= f = case f a of
    Done _ _ b -> Done h e b
    Fail _ _ s -> Fail h e s
    Cont k     -> k h e
  Fail r b s >>= _ = Fail r b s
  Cont k >>= f     = Cont $ \h e -> k h e >>= f
  fail = Fail mempty False

instance MonadPlus It where
  mplus = (<!>) 
  mzero = fail "mzero"

getRope :: It Rope
getRope = Cont $ \r e -> Done r e r

getMeasure :: It Cursor
getMeasure = Cont $ \r e -> Done r e (measure r)

getEof :: It Bool
getEof = Cont $ \ r e -> Done r e e 

instance Stream Cursor It Char where
  uncons cur = (getStrand cur >>= go) 
           <|> return Nothing 
    where
      go (Cursor b d i,bs,t) 
        | c <= 0x7f = do
           return $ Just (toEnum (fromEnum c), Cursor (b + 1) (d <> delta c) i) -- TODO: add in PathHunks
      | otherwise = error "TODO"
      where c = head bs
    -- TODO: finish these

getStrand :: Int -> It (Cursor, Strict.ByteString, FingerTree Cursor Strand)
getStrand n = Cont go where
  go h eof 
    | n < lastNewline h eof = dropRope n h (\c bs t -> Done h eof (c, bs, t)) 
                                           (Fail h eof "getStrand: dropRope failed")
    | eof                   = Fail h True "Unexpected EOF"
    | otherwise             = Cont $ \h' -> go (h <> h') -- h' <> h

sliceIt :: Int -> Int -> It Strict.ByteString
sliceIt !i !j = Cont go where
  go :: Rope -> Bool -> It Strict.ByteString
  go t@(Rope _ h) eof
    | j <= cursorBytes (measure h) || eof = Done t eof (sliceStrands h)
    | otherwise = Cont $ \t' eof' -> go (t <> t') eof'
  sliceStrands :: FingerTree Cursor Strand -> Strict.ByteString
  sliceStrands !t
    | req <= rmn = Strict.take req first -- yay! sharing
    | otherwise = Strict.concat 
                $ Lazy.toChunks 
                $ Lazy.take (fromIntegral req) 
                $ Lazy.fromChunks 
                $ first : Prelude.foldr iter [] (toList r')
    where iter (HunkStrand h) b = unintern h : b
          iter (PathStrand _) b = b
          (l,r) = FingerTree.split (\m -> cursorBytes m > i) t
          HunkStrand (Hunk _ _ a) :< r' = FingerTree.viewl r
          first = Strict.drop (i - cursorBytes (measure l)) a
          req = j - i
          rmn = Strict.length first

slice :: Cursor -> Cursor -> ParsecT Cursor u It Strict.ByteString
slice mark release = lift $ sliceIt (cursorBytes mark) (cursorBytes release)

sliced :: ParsecT Cursor u It a -> ParsecT Cursor u It Strict.ByteString
sliced p = do
  mark <- getInput
  _ <- p
  release <- getInput
  slice mark release

-- sliceDelta :: Int -> Int -> It Delta

