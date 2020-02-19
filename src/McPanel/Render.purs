module McPanel.Render where

import Prelude

import McPanel.Model (Direction(..), Layout, Split(..))
import Control.Monad.Free.Extra (FreeF(..), cataFree)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.React.Component ((|-), (|=))

render :: forall a. Layout a -> JSX
render l = R.div |= { style: R.css { height: "100%", width: "100%" } } |- cataFree go l
  where
  go (Pure a) = mempty
  go (Free (Split {ratio, direction, first, next})) = 
       R.div |= mkStyle direction ratio         |- inner |- first
    <> R.div |= mkStyle direction (1.0 - ratio) |- inner |- next
  inner = R.div |= { className: "inner" }

-- This is a PITA because react-basic doesn't just use a string for the style attribute...
mkStyle :: Direction -> Number -> { style :: R.CSS }
mkStyle d r = { style }
  where
  style = R.css $ case d of
    Horizontal -> { height: amt, width: "100%", display: "block" }
    Vertical   -> { height: "100%", width: amt, display: "inline-block" }
  amt = show (r * 100.0) <> "%" 