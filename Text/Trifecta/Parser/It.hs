{-# LANGUAGE MultiParamTypeClasses, BangPatterns, MagicHash, UnboxedTuples, TypeFamilies #-}
-- | harder, better, faster, stronger...
module Text.Trifecta.Parser.It 
  ( It(Pure, It)
  , needIt
  , wantIt
  , simplifyIt
  , runIt
  , fillIt
  , rewindIt
  , sliceIt
  , stepIt
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Data.Monoid
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Profunctor
import Data.Key as Key
import Text.Trifecta.Rope as Rope
import Text.Trifecta.Delta
import Text.Trifecta.Bytes
import Text.Trifecta.Util as Util
import Text.Trifecta.Util.MaybePair
import Text.Trifecta.Parser.Step

data It r a
  = Pure a 
  | It a (r -> It r a)

instance Show a => Show (It r a) where
  showsPrec d (Pure a) = showParen (d > 10) $ showString "Pure " . showsPrec 11 a
  showsPrec d (It a _) = showParen (d > 10) $ showString "It " . showsPrec 11 a . showString " ..."

instance Functor (It r) where
  fmap f (Pure a) = Pure $ f a
  fmap f (It a k) = It (f a) $ fmap f . k

type instance Key (It r) = r

instance Profunctor It where
  lmap _ (Pure a) = Pure a
  lmap f (It a k) = It a (lmap f . k . f)

  rmap g (Pure a) = Pure (g a)
  rmap g (It a k) = It (g a) (rmap g . k)

instance Applicative (It r) where
  pure = Pure
  Pure f  <*> Pure a  = Pure $ f a
  Pure f  <*> It a ka = It (f a) $ fmap f . ka
  It f kf <*> Pure a  = It (f a) $ fmap ($a) . kf
  It f kf <*> It a ka = It (f a) $ \r -> kf r <*> ka r

instance Indexable (It r) where
  index (Pure a) _ = a
  index (It _ k) r = extract (k r)

instance Lookup (It r) where
  lookup = lookupDefault

instance Zip (It r) where
  zipWith = liftA2

simplifyIt :: It r a -> r -> It r a
simplifyIt (It _ k) r = k r
simplifyIt pa _       = pa

instance Monad (It r) where
  return = Pure
  Pure a >>= f = f a
  It a k >>= f = It (extract (f a)) $ \r -> case k r of 
    It a' k' -> It (Key.index (f a') r) $ k' >=> f
    Pure a' -> simplifyIt (f a') r

instance Apply (It r) where (<.>) = (<*>) 
instance Bind (It r) where (>>-) = (>>=) 

instance Extend (It r) where
  duplicate p@Pure{} = Pure p
  duplicate p@(It _ k) = It p (duplicate . k)

  extend f p@Pure{} = Pure (f p)
  extend f p@(It _ k) = It (f p) (extend f . k)

-- | It is a cofree comonad
instance Comonad (It r) where
  extract (Pure a) = a
  extract (It a _) = a

needIt :: a -> (r -> Maybe a) -> It r a
needIt z f = k where 
  k = It z $ \r -> case f r of 
    Just a -> Pure a
    Nothing -> k

wantIt :: a -> (r -> (# Bool, a #)) -> It r a
wantIt z f = It z k where 
  k r = case f r of
    (# False, a #) -> It a k
    (# True,  a #) -> Pure a

-- scott decoding
runIt :: (a -> o) -> (a -> (r -> It r a) -> o) -> It r a -> o
runIt p _ (Pure a) = p a
runIt _ i (It a k) = i a k

-- * Rope specifics

-- | Given a position, go there, and grab the text forward from that point
fillIt :: Delta -> It Rope (MaybePair Delta Strict.ByteString)
fillIt n = wantIt NothingPair $ \r -> 
  (# bytes n < bytes (rewind (delta r))
  ,  grabLine n r NothingPair JustPair #) 

stepIt :: It Rope a -> Step e a
stepIt = go mempty where
  go r (Pure a) = StepDone r mempty a
  go r (It a k) = StepCont r (pure a) $ \s -> go s (k s)
                                       
-- | Return the text of the line that contains a given position
rewindIt :: Delta -> It Rope (Maybe Strict.ByteString)
rewindIt n = wantIt Nothing $ \r -> 
  (# bytes n < bytes (rewind (delta r))
  ,  grabLine (rewind n) r Nothing $ const Just #)

sliceIt :: Delta -> Delta -> It Rope Strict.ByteString
sliceIt !i !j = wantIt mempty $ \r -> 
  (# bj < bytes (rewind (delta r))
  ,  grabRest i r mempty $ const $ Util.fromLazy . Lazy.take (fromIntegral (bj - bi)) #)
  where
    bi = bytes i
    bj = bytes j

