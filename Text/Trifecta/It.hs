{-# LANGUAGE MultiParamTypeClasses, BangPatterns, MagicHash, UnboxedTuples #-}
module Text.Trifecta.It 
  ( It(Pure, It, result)
  , needIt
  , wantIt
  , fillIt
  , lineIt
  , sliceIt
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Monoid
import Data.FingerTree as FingerTree
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Functor.Bind
import Data.Functor.Plus
import Text.Trifecta.Rope as Rope
import Text.Trifecta.Delta
import Text.Trifecta.Bytes
import Text.Trifecta.Util
import Text.Trifecta.Parser.Step

data It a
  = Pure { result :: a } 
  | It { result :: a, _it :: Rope -> It a }

instance Functor It where
  fmap f (Pure a) = Pure (f a)
  fmap f (It a k) = It (f a) (fmap f . k)

instance Applicative It where
  pure = Pure
  Pure f  <*> Pure a  = Pure (f a)
  Pure f  <*> It a ka = It (f a) (fmap f . ka)
  It f kf <*> Pure a  = It (f a) (fmap ($a) . kf)
  It f kf <*> It a ka = It (f a) (\r -> kf r <*> ka r)

instance Monad It where
  return = Pure
  Pure a >>= f = f a
  It a k >>= f = It (result (f a)) (k >=> f)

instance Apply It where (<.>) = (<*>) 
instance Bind It where (>>-) = (>>=) 

instance Extend It where
  duplicate p@Pure{} = Pure p
  duplicate p@(It _ k) = It p (duplicate . k)

  extend f p@Pure{} = Pure (f p)
  extend f p@(It _ k) = It p (extend f . k)

instance Comonad It where
  extract = result

needIt :: a -> (Rope -> Maybe a) -> It a
needIt z f = k where k = It z $ \r -> case f r of 
  Just a -> Pure a
  Nothing -> k

wantIt :: a -> (Rope -> (# Bool, a #)) -> It a
wantIt z f = It z k where 
  k r -> case f r of
    (# False, a #) -> It a k
    (# True,  a #) -> Pure a

-- given a position, go there, and grab the text forward from that point
fillIt :: Delta -> It (MaybePair Delta Strict.ByteString)
fillIt n = wantIt NothingPair $ \r -> 
  (# bytes n < bytes (rewind (delta r))
  ,  grabLine n h NothingPair JustPair #) 
                                       
-- return the text of the line that contains a given position
lineIt :: Delta -> It (Maybe Strict.ByteString)
lineIt n = wantIt Nothing $ \r -> 
  (# bytes n < bytes (rewind (delta r))
  ,  grabLine n h Nothing (const Just) #)

sliceIt :: Delta -> Delta -> It Strict.ByteString
sliceIt !i !j = wantIt mempty $ \r -> 
  (# bytes n < bytes (rewind (delta r))
  ,  grabRest n h mempty $ const $ 
     Strict.concat . 
     Lazy.toChunks . 
     Lazy.take (fromIntegral (bj - bi)) bs) #)
  where
    bi = bytes i
    bj = bytes j

stepIt :: It a -> Step e a
stepIt = go mempty where
  go r (Pure a) = StepDone mempty a
  go r (It a k) = StepIt (pure a) $ \t -> let s = snoc r t in go s $ k s
