{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Util.It
-- Copyright   :  (C) 2011-2019 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- harder, better, faster, stronger...
----------------------------------------------------------------------------
module Text.Trifecta.Util.It
  ( It(Pure, It)
  , needIt
  , wantIt
  , simplifyIt
  , foldIt
  , runIt
  , fillIt
  , rewindIt
  , sliceIt
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.Monad

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Data.ByteString                as Strict
import Data.ByteString.Lazy           as Lazy
import Data.Profunctor
import Text.Trifecta.Delta
import Text.Trifecta.Rope
import Text.Trifecta.Util.Combinators as Util

-- $setup
-- >>> import Control.Comonad (extract)
-- >>> import Data.ByteString as Strict
-- >>> import Text.Trifecta.Delta
-- >>> import Text.Trifecta.Rope
-- >>> import Text.Trifecta.Util.It

-- | @'It'@ is an <https://wiki.haskell.org/Enumerator_and_iteratee Iteratee>
-- that can produce partial results.
--
-- @'It' r a@ consumes a feed of @r@s and produces @a@s on the way. New values
-- can be fed using @'simplifyIt'@, the current (partial or final) result is
-- extracted using @'extract'@.
--
-- >>> let keepIt    a = Pure a
-- >>> let replaceIt a = It a replaceIt
--
-- >>> extract (keepIt 0)
-- 0
--
-- >>> extract (replaceIt 0)
-- 0
--
-- >>> extract (simplifyIt (keepIt 0) 5)
-- 0
--
-- >>> extract (simplifyIt (replaceIt 0) 5)
-- 5
data It r a
  = Pure a
  -- ^ Final result, rest of the feed is discarded
  | It a (r -> It r a)
  -- ^ Intermediate result, consumed values produce new results

instance Show a => Show (It r a) where
  showsPrec d (Pure a) = showParen (d > 10) $ showString "Pure " . showsPrec 11 a
  showsPrec d (It a _) = showParen (d > 10) $ showString "It " . showsPrec 11 a . showString " ..."

instance Functor (It r) where
  fmap f (Pure a) = Pure $ f a
  fmap f (It a k) = It (f a) $ fmap f . k

instance Profunctor It where
  rmap = fmap
  lmap _ (Pure a) = Pure a
  lmap f (It a g) = It a (lmap f . g . f)

instance Applicative (It r) where
  pure = Pure
  Pure f  <*> Pure a  = Pure $ f a
  Pure f  <*> It a ka = It (f a) $ fmap f . ka
  It f kf <*> Pure a  = It (f a) $ fmap ($ a) . kf
  It f kf <*> It a ka = It (f a) $ \r -> kf r <*> ka r

indexIt :: It r a -> r -> a
indexIt (Pure a) _ = a
indexIt (It _ k) r = extract (k r)

-- | Feed a value to 'It', obtaining a new (partial or final) result.
simplifyIt :: It r a -> r -> It r a
simplifyIt (It _ k) r = k r
simplifyIt pa _       = pa

instance Monad (It r) where
  return = pure
  Pure a >>= f = f a
  It a k >>= f = It (extract (f a)) $ \r -> case k r of
    It a' k' -> It (indexIt (f a') r) $ k' >=> f
    Pure a' -> simplifyIt (f a') r

instance ComonadApply (It r) where (<@>) = (<*>)

-- | 'It' is a cofree comonad
instance Comonad (It r) where
  duplicate p@Pure{}   = Pure p
  duplicate p@(It _ k) = It p (duplicate . k)

  extend f p@Pure{}   = Pure (f p)
  extend f p@(It _ k) = It (f p) (extend f . k)

  extract (Pure a) = a
  extract (It a _) = a

-- | Consumes input until a value can be produced.
--
-- >>> :{
-- let needTen = needIt 0 (\n -> if n < 10 then Nothing else Just n) :: It Int Int
-- :}
--
-- >>> extract needTen
-- 0
--
-- >>> extract (simplifyIt needTen 5)
-- 0
--
-- >>> extract (simplifyIt needTen 11)
-- 11
--
-- >>> extract (simplifyIt (simplifyIt (simplifyIt needTen 5) 11) 15)
-- 11
needIt
    :: a               -- ^ Initial result
    -> (r -> Maybe a)  -- ^ Produce a result if possible
    -> It r a
needIt z f = k where
  k = It z $ \r -> case f r of
    Just a -> Pure a
    Nothing -> k

-- | Consumes input and produces partial results until a condition is met.
-- Unlike 'needIt', partial results are already returned when the condition is
-- not fulfilled yet.
--
-- > >>> :{
-- > let wantTen :: It Int Int
-- >     wantTen = wantIt 0 (\n -> (# n >= 10, n #))
-- > :}
--
-- > >>> extract wantTen
-- > 0
--
-- > >>> extract (simplifyIt wantTen 5)
-- > 5
--
-- > >>> extract (simplifyIt wantTen 11)
-- > 11
--
-- > >>> extract (simplifyIt (simplifyIt (simplifyIt wantTen 5) 11) 15)
-- > 11
wantIt
    :: a                 -- ^ Initial result
    -> (r -> (# Bool, a #))  -- ^ Produce a partial or final result
    -> It r a
wantIt z f = It z k where
  k r = case f r of
    (# False, a #) -> It a k
    (# True,  a #) -> Pure a

-- | The generalized fold (Böhm-Berarducci decoding) over 'It r a'.
--
-- 'foldIt' satisfies the property:
--
-- @foldIt Pure It = id@
foldIt :: (a -> o) -> (a -> (r -> o) -> o) -> It r a -> o
foldIt p _ (Pure a) = p a
foldIt p i (It a k) = i a (\r -> foldIt p i (k r))

-- | Scott decoding of 'It r a'.
--
-- The scott decoding is similar to the generalized fold over a data type, but
-- leaves the recursion step to the calling function.
--
-- 'runIt' satiesfies the property:
--
-- @runIt Pure It = id@
--
-- See also the Scott decoding of lists:
--
-- @runList :: (a -> [a] -> b) -> b -> [a] -> b@
--
-- and compare it with 'foldr' (the Böhm-Berarducci decoding for lists):
--
-- @foldr :: (a -> b -> b) -> b -> [a] -> b@
runIt :: (a -> o) -> (a -> (r -> It r a) -> o) -> It r a -> o
runIt p _ (Pure a) = p a
runIt _ i (It a k) = i a k

-- * Rope specifics

-- | Given a position, go there, and grab the rest of the line forward from that
-- point.
--
-- >>> :set -XOverloadedStrings
-- >>> let secondLine = fillIt Nothing (const Just) (delta ("foo\nb" :: Strict.ByteString))
--
-- >>> extract secondLine
-- Nothing
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo"))
-- Nothing
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo\nbar"))
-- Just "ar"
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo\nbar\nbaz"))
-- Just "ar\n"
fillIt :: r -> (Delta -> Strict.ByteString -> r) -> Delta -> It Rope r
fillIt kf ks n = wantIt kf $ \r ->
  (# bytes n < bytes (rewind (delta r))
  ,  grabLine n r kf ks #)

-- | Return the text of the line that contains a given position
--
-- >>> :set -XOverloadedStrings
-- >>> let secondLine = rewindIt (delta ("foo\nb" :: Strict.ByteString))
--
-- >>> extract secondLine
-- Nothing
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo"))
-- Nothing
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo\nbar"))
-- Just "bar"
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo\nbar\nbaz"))
-- Just "bar\n"
rewindIt :: Delta -> It Rope (Maybe Strict.ByteString)
rewindIt n = wantIt Nothing $ \r ->
  (# bytes n < bytes (rewind (delta r))
  ,  grabLine (rewind n) r Nothing $ const Just #)

-- | Return the text between two offsets.
--
-- >>> :set -XOverloadedStrings
-- >>> let secondLine = sliceIt (delta ("foo\n" :: Strict.ByteString)) (delta ("foo\nbar\n" :: Strict.ByteString))
--
-- >>> extract secondLine
-- ""
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo"))
-- ""
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo\nbar"))
-- "bar"
--
-- >>> extract (simplifyIt secondLine (ropeBS "foo\nbar\nbaz"))
-- "bar\n"
sliceIt :: Delta -> Delta -> It Rope Strict.ByteString
sliceIt !i !j = wantIt mempty $ \r ->
  (# bj < bytes (rewind (delta r))
  ,  grabRest i r mempty $ const $ Util.fromLazy . Lazy.take (fromIntegral (bj - bi)) #)
  where
    bi = bytes i
    bj = bytes j
