module Control.Monad.Free.Extra where

import Prelude

import Data.Either (either)
import Control.Monad.Free (Free, resume)

-- Catamorphism for Free monads (not stack safe)

data FreeF f a b = Pure a | Free (f b)

derive instance functorFreeF :: Functor f => Functor (FreeF f a)

project :: forall f a. Functor f => Free f a -> FreeF f a (Free f a)
project = either Free Pure <<< resume

cataFree :: forall f a x. Functor f => (FreeF f a x -> x) -> Free f a -> x
cataFree f = rec
  where
  rec x = f $ map rec $ project x