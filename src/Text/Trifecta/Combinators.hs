{-# language CPP                    #-}
{-# language FlexibleContexts       #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses  #-}
{-# language UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Combinators
-- Copyright   :  (c) Edward Kmett 2011-2019
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
  , careting, careted
  , spanning, spanned
  , fixiting
  , MarkParsing(..)
  ) where

import Control.Applicative
import Control.Monad                     (MonadPlus)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Lazy      as Lazy
import Control.Monad.Trans.RWS.Strict    as Strict
import Control.Monad.Trans.State.Lazy    as Lazy
import Control.Monad.Trans.State.Strict  as Strict
import Control.Monad.Trans.Writer.Lazy   as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.ByteString                   as Strict hiding (span)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Prelude                           hiding (span)

import Text.Parser.Token
import Text.Trifecta.Delta
import Text.Trifecta.Rendering

-- | This class provides parsers with easy access to:
--
-- 1) the current line contents.
-- 2) the current position as a 'Delta'.
-- 3) the ability to use 'sliced' on any parser.
class (MonadPlus m, TokenParsing m) => DeltaParsing m where
  -- | Retrieve the contents of the current line (from the beginning of the line)
  line :: m ByteString

  -- | Retrieve the current position as a 'Delta'.
  position :: m Delta

  -- | Run a parser, grabbing all of the text between its start and end points
  slicedWith :: (a -> Strict.ByteString -> r) -> m a -> m r

  -- | Retrieve a 'Rendering' of the current line noting this position, but not
  -- placing a 'Caret' there.
  rend :: m Rendering
  rend = rendered <$> position <*> line
  {-# inlinable rend #-}

  -- | Grab the remainder of the current line
  restOfLine :: m ByteString
  restOfLine = Strict.drop . fromIntegral . columnByte <$> position <*> line
  {-# inlinable restOfLine #-}

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (Lazy.StateT s m) where
  line = lift line
  {-# inlinable line #-}
  position = lift position
  {-# inlinable position #-}
  slicedWith f (Lazy.StateT m) = Lazy.StateT $ \s -> slicedWith (\(a,s') b -> (f a b, s')) $ m s
  {-# inlinable slicedWith #-}
  rend = lift rend
  {-# inlinable rend #-}
  restOfLine = lift restOfLine
  {-# inlinable restOfLine #-}

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (Strict.StateT s m) where
  line = lift line
  {-# inlinable line #-}
  position = lift position
  {-# inlinable position #-}
  slicedWith f (Strict.StateT m) = Strict.StateT $ \s -> slicedWith (\(a,s') b -> (f a b, s')) $ m s
  {-# inlinable slicedWith #-}
  rend = lift rend
  {-# inlinable rend #-}
  restOfLine = lift restOfLine
  {-# inlinable restOfLine #-}

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (ReaderT e m) where
  line = lift line
  {-# inlinable line #-}
  position = lift position
  {-# inlinable position #-}
  slicedWith f (ReaderT m) = ReaderT $ slicedWith f . m
  {-# inlinable slicedWith #-}
  rend = lift rend
  {-# inlinable rend #-}
  restOfLine = lift restOfLine
  {-# inlinable restOfLine #-}

instance (MonadPlus m, DeltaParsing m, Monoid w) => DeltaParsing (Strict.WriterT w m) where
  line = lift line
  {-# inlinable line #-}
  position = lift position
  {-# inlinable position #-}
  slicedWith f (Strict.WriterT m) = Strict.WriterT $ slicedWith (\(a,s') b -> (f a b, s')) m
  {-# inlinable slicedWith #-}
  rend = lift rend
  {-# inlinable rend #-}
  restOfLine = lift restOfLine
  {-# inlinable restOfLine #-}

instance (MonadPlus m, DeltaParsing m, Monoid w) => DeltaParsing (Lazy.WriterT w m) where
  line = lift line
  {-# inlinable line #-}
  position = lift position
  {-# inlinable position #-}
  slicedWith f (Lazy.WriterT m) = Lazy.WriterT $ slicedWith (\(a,s') b -> (f a b, s')) m
  {-# inlinable slicedWith #-}
  rend = lift rend
  {-# inlinable rend #-}
  restOfLine = lift restOfLine
  {-# inlinable restOfLine #-}

instance (MonadPlus m, DeltaParsing m, Monoid w) => DeltaParsing (Lazy.RWST r w s m) where
  line = lift line
  {-# inlinable line #-}
  position = lift position
  {-# inlinable position #-}
  slicedWith f (Lazy.RWST m) = Lazy.RWST $ \r s -> slicedWith (\(a,s',w) b -> (f a b, s',w)) $ m r s
  {-# inlinable slicedWith #-}
  rend = lift rend
  {-# inlinable rend #-}
  restOfLine = lift restOfLine
  {-# inlinable restOfLine #-}

instance (MonadPlus m, DeltaParsing m, Monoid w) => DeltaParsing (Strict.RWST r w s m) where
  line = lift line
  {-# inlinable line #-}
  position = lift position
  {-# inlinable position #-}
  slicedWith f (Strict.RWST m) = Strict.RWST $ \r s -> slicedWith (\(a,s',w) b -> (f a b, s',w)) $ m r s
  {-# inlinable slicedWith #-}
  rend = lift rend
  {-# inlinable rend #-}
  restOfLine = lift restOfLine
  {-# inlinable restOfLine #-}

instance (MonadPlus m, DeltaParsing m) => DeltaParsing (IdentityT m) where
  line = lift line
  {-# inlinable line #-}
  position = lift position
  {-# inlinable position #-}
  slicedWith f (IdentityT m) = IdentityT $ slicedWith f m
  {-# inlinable slicedWith #-}
  rend = lift rend
  {-# inlinable rend #-}
  restOfLine = lift restOfLine
  {-# inlinable restOfLine #-}

-- | Run a parser, grabbing all of the text between its start and end points and
-- discarding the original result
sliced :: DeltaParsing m => m a -> m ByteString
sliced = slicedWith (\_ bs -> bs)
{-# inlinable sliced #-}

-- | Grab a 'Caret' pointing to the current location.
careting :: DeltaParsing m => m Caret
careting = Caret <$> position <*> line
{-# inlinable careting #-}

-- | Parse a 'Careted' result. Pointing the 'Caret' to where you start.
careted :: DeltaParsing m => m a -> m (Careted a)
careted p = (\m l a -> a :^ Caret m l) <$> position <*> line <*> p
{-# inlinable careted #-}

-- | Discard the result of a parse, returning a 'Span' from where we start to
-- where it ended parsing.
spanning :: DeltaParsing m => m a -> m Span
spanning p = (\s l e -> Span s e l) <$> position <*> line <*> (p *> position)
{-# inlinable spanning #-}

-- | Parse a 'Spanned' result. The 'Span' starts here and runs to the last
-- position parsed.
spanned :: DeltaParsing m => m a -> m (Spanned a)
spanned p = (\s l a e -> a :~ Span s e l) <$> position <*> line <*> p <*> position
{-# inlinable spanned #-}

-- | Grab a fixit.
fixiting :: DeltaParsing m => m Strict.ByteString -> m Fixit
fixiting p = (\(r :~ s) -> Fixit s r) <$> spanned p
{-# inlinable fixiting #-}

-- | This class is a refinement of 'DeltaParsing' that adds the ability to mark
-- your position in the input and return there for further parsing later.
class (DeltaParsing m, HasDelta d) => MarkParsing d m | m -> d where
  -- | mark the current location so it can be used in constructing a span, or
  -- for later seeking
  mark :: m d
  -- | Seek a previously marked location
  release :: d -> m ()

instance (MonadPlus m, MarkParsing d m) => MarkParsing d (Lazy.StateT s m) where
  mark = lift mark
  {-# inlinable mark #-}
  release = lift . release
  {-# inlinable release #-}

instance (MonadPlus m, MarkParsing d m) => MarkParsing d (Strict.StateT s m) where
  mark = lift mark
  {-# inlinable mark #-}
  release = lift . release
  {-# inlinable release #-}

instance (MonadPlus m, MarkParsing d m) => MarkParsing d (ReaderT e m) where
  mark = lift mark
  {-# inlinable mark #-}
  release = lift . release
  {-# inlinable release #-}

instance (MonadPlus m, MarkParsing d m, Monoid w) => MarkParsing d (Strict.WriterT w m) where
  mark = lift mark
  {-# inlinable mark #-}
  release = lift . release
  {-# inlinable release #-}

instance (MonadPlus m, MarkParsing d m, Monoid w) => MarkParsing d (Lazy.WriterT w m) where
  mark = lift mark
  {-# inlinable mark #-}
  release = lift . release
  {-# inlinable release #-}

instance (MonadPlus m, MarkParsing d m, Monoid w) => MarkParsing d (Lazy.RWST r w s m) where
  mark = lift mark
  {-# inlinable mark #-}
  release = lift . release
  {-# inlinable release #-}

instance (MonadPlus m, MarkParsing d m, Monoid w) => MarkParsing d (Strict.RWST r w s m) where
  mark = lift mark
  {-# inlinable mark #-}
  release = lift . release
  {-# inlinable release #-}

instance (MonadPlus m, MarkParsing d m) => MarkParsing d (IdentityT m) where
  mark = lift mark
  {-# inlinable mark #-}
  release = lift . release
  {-# inlinable release #-}
