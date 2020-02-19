module McPanel.Model where

import Prelude

import Control.Monad.Free (Free)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Traversable (class Traversable, traverseDefault)

data Direction = Vertical | Horizontal 

newtype Split a = Split 
  { ratio     :: Number
  , direction :: Direction 
  , first     :: a
  , next      :: a
  }

mkSplit :: forall a. Number -> Direction  -> a -> a -> Split a
mkSplit ratio direction first next = Split { ratio, direction, first, next}

derive instance functorSplit :: Functor Split

instance foldableSplit :: Foldable Split
  where
  foldMap f (Split { first, next }) = f first <> f next
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance traversableSplit :: Traversable Split
  where
  sequence (Split { ratio, first, next, direction }) = mkSplit ratio direction <$> first <*> next
  traverse = traverseDefault

type Layout = Free Split
