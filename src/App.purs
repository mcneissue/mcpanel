module App where

import Prelude

import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Maybe (maybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import McPanel.Model (hsplit, vsplit, Panel)
import McPanel.Render (render)
import McPanel.Transform (shiftFocus, Direction(..), panelInfo)
import React.Basic (JSX)
import Snap.SYTC.Component (Cmp)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.UIEvent.KeyboardEvent as KE
import Web.HTML (window)
import Web.HTML.Window as Window

type State = { panel :: Panel Int, nextId :: Int, showPanelInfo :: Boolean }

data Action
  = ShiftFocus Direction 
  | Split Direction
  | ToggleInfo

initState :: State
initState = 
  { panel: { layout, focus: 1 }
  , nextId: 2
  , showPanelInfo: false 
  }
  where
  layout = pure 1

app :: Cmp Effect JSX State Action
app _ s = maybe mempty identity $ render s <$> panelInfo s.panel

reducer :: Action -> State -> State
reducer ToggleInfo s = over (prop _showPanelInfo) not s
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
  l <- eventListener $ maybe (pure unit) handleKeyDown <<< map KE.key <<< KE.fromEvent
  addEventListener (EventType "keydown") l true w
  where
    handleKeyDown k = case k of
      "ArrowLeft"  -> u $ ShiftFocus L
      "ArrowRight" -> u $ ShiftFocus R
      "ArrowUp"    -> u $ ShiftFocus U
      "ArrowDown"  -> u $ ShiftFocus D
      "v" -> u $ Split R
      "h" -> u $ Split U
      "i" -> u ToggleInfo
      _  -> pure unit

_panel :: SProxy "panel"
_panel = SProxy

_nextId :: SProxy "nextId"
_nextId = SProxy

_layout :: SProxy "layout"
_layout = SProxy

_showPanelInfo :: SProxy "showPanelInfo"
_showPanelInfo = SProxy