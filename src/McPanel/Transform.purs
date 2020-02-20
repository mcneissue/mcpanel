module McPanel.Transform where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import McPanel.Model (Panel, Split(..))
import McPanel.Model as M
import Control.Monad.Free.Extra (cataFree, FreeF(..))

data Direction = U | D | L | R

shiftFocus :: forall a. Eq a => Direction -> Panel a -> Panel a
shiftFocus d p = maybe p (\a -> p { focus = a }) $ shiftFocus' d p

shiftFocus' :: forall a. Eq a => Direction -> Panel a -> Maybe a
shiftFocus' d { layout, focus } = either Just (const Nothing) $ cataFree go layout
  where
  go (Pure a) | a == focus = Right $ Right a
              | otherwise  = Right $ Left a
  go (Free s) = chooseShift d s

-- kill me
chooseShift :: forall a. Direction -> Split (Either a (Either a a)) -> Either a (Either a a)
chooseShift d (Split { direction, first, next }) = case { first, next } of
  { first: Left a } -> first
  { next: Left a }  -> next
  { first: Right (Right a), next: Right (Left b) } -> case direction of
    M.Horizontal -> case d of
      D -> Left b
      _ -> first
    M.Vertical -> case d of
      R -> Left b
      _ -> first
  { first: Right (Left b), next: Right (Right a) } -> case direction of
    M.Horizontal -> case d of
      U -> Left b
      _ -> next
    M.Vertical -> case d of
      L -> Left b
      _ -> next
  { first: Right (Left _), next: Right (Left _) } -> case d of
    U -> first
    D -> first
    L -> first
    R -> first
  _ -> first