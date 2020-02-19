module App where

import Prelude

import Effect (Effect)
import McPanel.Model (Layout, hsplit, vsplit)
import McPanel.Render (render)
import React.Basic (JSX)
import Snap.SYTC.Component (Cmp')

type State = { layout :: Layout Int }

initState :: State
initState = { layout }
  where
  layout = pure 1 >>= vsplit 1 2 >>= hsplit 1 3

app :: Cmp' Effect JSX State
app _ = render <<< _.layout