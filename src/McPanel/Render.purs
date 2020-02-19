module McPanel.Render where

import Prelude

import McPanel.Model (Direction(..), Split(..), Panel)
import Control.Monad.Free.Extra (FreeF(..), cataFree)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.React.Component ((|-), (|=))

render :: forall a. Eq a => Panel a -> JSX
render { layout, focus } = 
  R.div 
  |= { style: R.css { height: "100%", width: "100%" } } 
  |- cataFree go layout
  where
  go (Pure a) | focus == a = R.div { className: "focused" }
              | otherwise  = mempty
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