module Text.Trifecta.Diagnostic 
  ( DiagnosticLevel(..)
  , Diagnostic(..)
  ) where

import Control.Applicative
import Control.Comonad
import Data.Bifunctor
import Data.Functor.Apply
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.List.NonEmpty hiding (map)
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Text.Trifecta.Bytes
import Text.Trifecta.Delta
import Text.Trifecta.Render
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

data DiagnosticLevel = Note | Warning | Error | Fatal
  deriving (Eq,Ord,Show,Read)

instance Semigroup DiagnosticLevel where
  (<>) = max

instance Pretty DiagnosticLevel where
  pretty p = prettyTerm p *> empty

instance PrettyTerm DiagnosticLevel where
  prettyTerm Note    = text "note"
  prettyTerm Warning = magenta $ text "warning"
  prettyTerm Error   = red $ text "error"
  prettyTerm Fatal   = red $ text "fatal"

data Diagnostic l m = Diagnostic !Render l m [Diagnostic l m]

instance Renderable (Diagnostic l m) where
  render (Diagnostic r _ _ _) = r

instance HasDelta (Diagnostic l m) where
  delta (Diagnostic r _ _ _) = delta r

instance HasBytes (Diagnostic l m) where
  bytes = bytes . delta

instance Extend (Diagnostic l) where
  extend f d@(Diagnostic r l _ xs) = Diagnostic r l (f d) (map (extend f) xs)

instance Comonad (Diagnostic l) where
  extract (Diagnostic _ _ m _) = m

instance (Pretty l, Pretty m) => Pretty (Diagnostic l m) where
  pretty (Diagnostic r l m xs) = vsep 
    [ pretty (delta r) <> char ':' <+> pretty l <> char ':' <+> pretty m 
    , pretty r
    , nest 2 (prettyList xs)
    ]
  prettyList = vsep . fmap pretty

instance (PrettyTerm l, PrettyTerm m) => PrettyTerm (Diagnostic l m) where
  prettyTerm (Diagnostic r l m xs) = vsep
    [ prettyTerm (delta r) <> char ':' <+> prettyTerm l <> char ':' <+> prettyTerm m 
    , prettyTerm r
    , nest 2 (prettyTermList xs)
    ]
  prettyTermList = vsep . fmap prettyTerm

instance (Pretty l, Pretty m) => Show (Diagnostic l m) where
  showsPrec d = showsPrec d . pretty

instance Functor (Diagnostic l) where
  fmap f (Diagnostic r l m xs) = Diagnostic r l (f m) $ map (fmap f) xs

instance Bifunctor Diagnostic where
  bimap f g (Diagnostic r l m xs) = Diagnostic r (f l) (g m) $ map (bimap f g) xs

instance Foldable (Diagnostic l) where
  foldMap f (Diagnostic _ _ m xs) = f m `mappend` foldMap (foldMap f) xs

instance Traversable (Diagnostic l) where
  traverse f (Diagnostic r l m xs) = Diagnostic r l <$> f m <*> traverse (traverse f) xs

instance Foldable1 (Diagnostic l) where
  foldMap1 f (Diagnostic _ _ m []) = f m
  foldMap1 f (Diagnostic _ _ m (x:xs)) = f m <> foldMap1 (foldMap1 f) (x:|xs)

instance Traversable1 (Diagnostic l) where
  traverse1 f (Diagnostic r l m [])     = fmap (\fm -> Diagnostic r l fm []) (f m)
  traverse1 f (Diagnostic r l m (x:xs)) = (\fm (y:|ys) -> Diagnostic r l fm (y:ys)) 
                                      <$> f m 
                                      <.> traverse1 (traverse1 f) (x:|xs)

instance Bifoldable Diagnostic where
  bifoldMap f g (Diagnostic _ l m xs) = f l `mappend` g m `mappend` foldMap (bifoldMap f g) xs

instance Bitraversable Diagnostic where
  bitraverse f g (Diagnostic r l m xs) = Diagnostic r <$> f l <*>  g m <*> traverse (bitraverse f g) xs
  
instance Bifoldable1 Diagnostic where
  bifoldMap1 f g (Diagnostic _ l m [])     = f l <> g m
  bifoldMap1 f g (Diagnostic _ l m (x:xs)) = f l <> g m <> foldMap1 (bifoldMap1 f g) (x:|xs)

instance Bitraversable1 Diagnostic where
  bitraverse1 f g (Diagnostic r l m [])     = (\fl gm -> Diagnostic r fl gm []) <$> f l <.> g m
  bitraverse1 f g (Diagnostic r l m (x:xs)) = (\fl gm (y:|ys) -> Diagnostic r fl gm (y:ys)) 
                                      <$> f l
                                      <.> g m
                                      <.> traverse1 (bitraverse1 f g) (x:|xs)

