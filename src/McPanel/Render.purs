module McPanel.Render where

import Prelude

import McPanel.Model (Direction(..), Split(..), Panel)
import Control.Monad.Free.Extra (FreeF(..), cataFree)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.React.Component ((|-), (|=))

data Float = L | R

type Options r = { showPanelInfo :: Boolean | r }

render :: forall a r. Eq a => Show a => Options r -> Panel a -> JSX
render { showPanelInfo } { layout, focus } = 
  R.div 
  |= { style: R.css { height: "100%", width: "100%" } } 
  |- cataFree go layout
  where
  go (Pure a) | focus == a = R.div |= { className: "focused leaf" } |- R.text (info a)
              | otherwise  = R.div |= { className: "leaf"} |- R.text (info a)
  go (Free (Split {ratio, direction, first, next})) = 
       R.div |= mkStyle L direction ratio         |- inner |- first
    <> R.div |= mkStyle R direction (1.0 - ratio) |- inner |- next
  inner = R.div |= { className: "inner" }
  info a = if showPanelInfo then show a else mempty

mkStyle :: Float -> Direction -> Number -> { style :: R.CSS }
mkStyle f d r = { style }
  where
  style = R.css $ case d of
    Horizontal -> { height: amt, width: "100%", float: ""}
    Vertical   -> { height: "100%", width: amt, float: float f }
  amt = show (r * 100.0) <> "%"

float :: Float -> String
float L = "left"
float R = "right"