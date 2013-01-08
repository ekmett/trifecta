{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Combinators
-- Copyright   :  (c) Edward Kmett 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Text.Trifecta.Combinators
  ( DeltaParsing(..)
  , sliced
  , caret, careted
  , span, spanned
  , fixit
  , MarkParsing(..)
  ) where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.ByteString as Strict hiding (span)
import Data.Semigroup
import Text.Parser.Token
import Text.Trifecta.Delta
import Text.Trifecta.Rendering
import Prelude hiding (span)

class (MonadPlus m, TokenParsing m) => DeltaParsing m where
  -- | Retrieve the contents of the current line (from the beginning of the line)
  line     :: m ByteString
  position :: m Delta
  -- | run a parser, grabbing all of the text between its start and end points
  slicedWith :: (a -> Strict.ByteString -> r) -> m a -> m r
  rend :: DeltaParsing m => m Rendering
  rend = rendered <$> position <*> line
  {-# INLINE rend #-}
  -- | grab the remainder of the current line
  restOfLine :: DeltaParsing m => m ByteString
  restOfLine = Strict.drop . fromIntegral . columnByte <$> position <*> line
  {-# INLINE restOfLine #-}

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (Lazy.StateT s m) where
  line = lift line
  position = lift position
  slicedWith f (Lazy.StateT m) = Lazy.StateT $ \s -> slicedWith (\(a,s') b -> (f a b, s')) $ m s
  rend = lift rend
  restOfLine = lift restOfLine

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (Strict.StateT s m) where
  line = lift line
  position = lift position
  slicedWith f (Strict.StateT m) = Strict.StateT $ \s -> slicedWith (\(a,s') b -> (f a b, s')) $ m s
  rend = lift rend
  restOfLine = lift restOfLine

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (ReaderT e m) where
  line = lift line
  position = lift position
  slicedWith f (ReaderT m) = ReaderT $ slicedWith f . m
  rend = lift rend
  restOfLine = lift restOfLine

instance (MonadPlus m, DeltaParsing m, Monoid w) => DeltaParsing (Strict.WriterT w m) where
  line = lift line
  position = lift position
  slicedWith f (Strict.WriterT m) = Strict.WriterT $ slicedWith (\(a,s') b -> (f a b, s')) m
  rend = lift rend
  restOfLine = lift restOfLine

instance (MonadPlus m, DeltaParsing m, Monoid w) => DeltaParsing (Lazy.WriterT w m) where
  line = lift line
  position = lift position
  slicedWith f (Lazy.WriterT m) = Lazy.WriterT $ slicedWith (\(a,s') b -> (f a b, s')) m
  rend = lift rend
  restOfLine = lift restOfLine

instance (MonadPlus m, DeltaParsing m, Monoid w) => DeltaParsing (Lazy.RWST r w s m) where
  line = lift line
  position = lift position
  slicedWith f (Lazy.RWST m) = Lazy.RWST $ \r s -> slicedWith (\(a,s',w) b -> (f a b, s',w)) $ m r s
  rend = lift rend
  restOfLine = lift restOfLine

instance (MonadPlus m, DeltaParsing m, Monoid w) => DeltaParsing (Strict.RWST r w s m) where
  line = lift line
  position = lift position
  slicedWith f (Strict.RWST m) = Strict.RWST $ \r s -> slicedWith (\(a,s',w) b -> (f a b, s',w)) $ m r s
  rend = lift rend
  restOfLine = lift restOfLine

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (IdentityT m) where
  line = lift line
  position = lift position
  slicedWith f (IdentityT m) = IdentityT $ slicedWith f m
  rend = lift rend
  restOfLine = lift restOfLine

-- | run a parser, grabbing all of the text between its start and end points and discarding the original result
sliced :: DeltaParsing m => m a -> m ByteString
sliced = slicedWith (\_ bs -> bs)
{-# INLINE sliced #-}

caret :: DeltaParsing m => m Caret
caret = Caret <$> position <*> line
{-# INLINE caret #-}

careted :: DeltaParsing m => m a -> m (Careted a)
careted p = (\m l a -> a :^ Caret m l) <$> position <*> line <*> p
{-# INLINE careted #-}

span :: DeltaParsing m => m a -> m Span
span p = (\s l e -> Span s e l) <$> position <*> line <*> (p *> position)
{-# INLINE span #-}

spanned :: DeltaParsing m => m a -> m (Spanned a)
spanned p = (\s l a e -> a :~ Span s e l) <$> position <*> line <*> p <*> position
{-# INLINE spanned #-}

fixit :: DeltaParsing m => m Strict.ByteString -> m Fixit
fixit p = (\(r :~ s) -> Fixit s r) <$> spanned p
{-# INLINE fixit #-}

class (DeltaParsing m, HasDelta d) => MarkParsing d m | m -> d where
  -- | mark the current location so it can be used in constructing a span, or for later seeking
  mark :: m d
  -- | Seek a previously marked location
  release :: d -> m ()

instance (MonadPlus m, MarkParsing d m) => MarkParsing d (Lazy.StateT s m) where
  mark = lift mark
  release = lift . release

instance (MonadPlus m, MarkParsing d m) => MarkParsing d (Strict.StateT s m) where
  mark = lift mark
  release = lift . release

instance (MonadPlus m, MarkParsing d m) => MarkParsing d (ReaderT e m) where
  mark = lift mark
  release = lift . release

instance (MonadPlus m, MarkParsing d m, Monoid w) => MarkParsing d (Strict.WriterT w m) where
  mark = lift mark
  release = lift . release

instance (MonadPlus m, MarkParsing d m, Monoid w) => MarkParsing d (Lazy.WriterT w m) where
  mark = lift mark
  release = lift . release

instance (MonadPlus m, MarkParsing d m, Monoid w) => MarkParsing d (Lazy.RWST r w s m) where
  mark = lift mark
  release = lift . release

instance (MonadPlus m, MarkParsing d m, Monoid w) => MarkParsing d (Strict.RWST r w s m) where
  mark = lift mark
  release = lift . release

instance (MonadPlus m, MarkParsing d m) => MarkParsing d (IdentityT m) where
  mark = lift mark
  release = lift . release
