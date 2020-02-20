module App where

import Prelude

import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Maybe (fromJust)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import McPanel.Model (hsplit, vsplit, Panel)
import McPanel.Render (render)
import McPanel.Transform (shiftFocus, Direction(..))
import Partial.Unsafe (unsafePartial)
import React.Basic (JSX)
import Snap.SYTC.Component (Cmp)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.UIEvent.KeyboardEvent as KE
import Web.HTML (window)
import Web.HTML.Window as Window

type State = { panel :: Panel Int, nextId :: Int }

data Action = ShiftFocus Direction | Split Direction

initState :: State
initState = { panel: { layout, focus: 1 }, nextId: 4 }
  where
  layout = pure 1 >>= vsplit 1 2 >>= hsplit 1 3

app :: Cmp Effect JSX State Action
app _ = render <<< _.panel

reducer :: Action -> State -> State
reducer (ShiftFocus d) s = over (prop _panel) (shiftFocus d) s
reducer (Split d) s = over (prop _nextId) (add 1) <<< over (prop _panel <<< prop _layout) doSplit $ s
  where
  doSplit = (=<<) case d of
    U -> hsplit s.panel.focus s.nextId
    D -> hsplit s.panel.focus s.nextId
    L -> vsplit s.panel.focus s.nextId
    R -> vsplit s.panel.focus s.nextId

setupListeners :: (Action -> Effect Unit) -> Effect Unit
setupListeners u = do
  w <- Window.toEventTarget <$> window
  l <- eventListener handleKeyDown
  addEventListener (EventType "keydown") l true w
  where
    handleKeyDown e' = 
      let e = unsafePartial $ fromJust $ KE.fromEvent e'
      in case KE.key e of
        "ArrowLeft"  -> u $ ShiftFocus L
        "ArrowRight" -> u $ ShiftFocus R
        "ArrowUp"    -> u $ ShiftFocus U
        "ArrowDown"  -> u $ ShiftFocus D
        "v" -> u $ Split R
        "h" -> u $ Split U
        _  -> pure unit

_panel :: SProxy "panel"
_panel = SProxy

_nextId :: SProxy "nextId"
_nextId = SProxy

_layout :: SProxy "layout"
_layout = SProxy