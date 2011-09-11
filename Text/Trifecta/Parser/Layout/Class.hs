{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Trifecta.Parser.Layout.Class
  ( LayoutToken(..)
  , LayoutState(..)
  , LayoutContext(..)
  , MonadLayoutParser(..)
  , defaultLayoutState
  , layoutBol
  , layoutStack
  , layoutEq
  , disableLayout
  , enableLayout
  , laidout
  ) where

import Control.Applicative
import Control.Monad (guard)
import Data.Lens.Common
import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Token.Class
import Text.Trifecta.Parser.Token.Combinators
import qualified Text.Trifecta.Highlight.Prim as Highlight
import Text.Trifecta.Diagnostic.Rendering.Prim

data LayoutToken
  = VirtualSemi
  | VirtualRightBrace
  | WhiteSpace
  | Other
  deriving (Eq,Ord,Show,Read)

data LayoutContext
  = IndentedLayout Rendering
  | DisabledLayout Rendering

instance HasDelta LayoutContext where
  delta (IndentedLayout r) = delta r
  delta (DisabledLayout r) = delta r

instance HasBytes LayoutContext where
  bytes = bytes . delta

data LayoutState = LayoutState
  { _layoutBol      :: Bool
  , _layoutStack    :: [LayoutContext]
  }

defaultLayoutState :: LayoutState
defaultLayoutState = LayoutState False []

layoutBol :: Lens LayoutState Bool
layoutBol = lens _layoutBol (\s l -> l { _layoutBol = s})

layoutStack :: Lens LayoutState [LayoutContext]
layoutStack = lens _layoutStack (\s l -> l { _layoutStack = s})

disableLayout :: MonadLayoutParser m => m a -> m a
disableLayout p = do
  r <- rend
  modLayout layoutStack (DisabledLayout r:)
  result <- p
  stk <- getLayout layoutStack
  case stk of
    DisabledLayout r':xs | delta r == delta r' -> result <$ setLayout layoutStack xs
    _ -> unexpected "layout"

enableLayout :: MonadLayoutParser m => m a -> m a
enableLayout p = do
  result <- highlight Highlight.Layout $ do
    r <- rend
    modLayout layoutStack (IndentedLayout r:)
    p
  result <$ layout <?> "virtual right brace"

laidout :: MonadLayoutParser m => m a -> m a
laidout p = braces p <|> enableLayout p

layoutEq :: MonadLayoutParser m => LayoutToken -> m LayoutToken
layoutEq s = try $ do
  r <- layout
  guard (s == r)
  return r

class MonadTokenParser m => MonadLayoutParser m where
  layout    :: m LayoutToken
  getLayout :: Lens LayoutState t -> m t
  setLayout :: Lens LayoutState t -> t -> m ()
  modLayout :: Lens LayoutState t -> (t -> t) -> m ()

instance MonadLayoutParser m => MonadLayoutParser (Strict.StateT s m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance MonadLayoutParser m => MonadLayoutParser (Lazy.StateT s m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance MonadLayoutParser m => MonadLayoutParser (ReaderT e m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance (Monoid w, MonadLayoutParser m) => MonadLayoutParser (Strict.WriterT w m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance (Monoid w, MonadLayoutParser m) => MonadLayoutParser (Lazy.WriterT w m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance (Monoid w, MonadLayoutParser m) => MonadLayoutParser (Strict.RWST r w s m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance (Monoid w, MonadLayoutParser m) => MonadLayoutParser (Lazy.RWST r w s m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance MonadLayoutParser m => MonadLayoutParser (IdentityT m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f
