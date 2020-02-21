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

type KeyBindings =
  { verticalSplit   :: String
  , horizontalSplit :: String
  , toggleInfo      :: String
  , navigateUp      :: String
  , navigateDown    :: String
  , navigateRight   :: String
  , navigateLeft    :: String
  }

initState :: State
initState = 
  { panel: { layout, focus: 1 }
  , nextId: 2
  , showPanelInfo: false 
  }
  where
  layout = pure 1

defaultKeyBindings :: KeyBindings
defaultKeyBindings =
  { verticalSplit: "v"
  , horizontalSplit: "h"
  , toggleInfo: "i"
  , navigateUp: "ArrowUp"
  , navigateDown: "ArrowDown"
  , navigateLeft: "ArrowLeft"
  , navigateRight: "ArrowRight"
  }

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

setupListeners :: KeyBindings -> (Action -> Effect Unit) -> Effect Unit
setupListeners kb u = do
  w <- Window.toEventTarget <$> window
  l <- eventListener $ maybe (pure unit) handleKeyDown <<< map KE.key <<< KE.fromEvent
  addEventListener (EventType "keydown") l true w
  where
    handleKeyDown k
      | k == kb.navigateLeft    = u $ ShiftFocus L
      | k == kb.navigateRight   = u $ ShiftFocus R
      | k == kb.navigateUp      = u $ ShiftFocus U
      | k == kb.navigateDown    = u $ ShiftFocus D
      | k == kb.verticalSplit   = u $ Split R
      | k == kb.horizontalSplit = u $ Split U
      | k == kb.toggleInfo      = u ToggleInfo
      | otherwise = pure unit

_panel :: SProxy "panel"
_panel = SProxy

_nextId :: SProxy "nextId"
_nextId = SProxy

_layout :: SProxy "layout"
_layout = SProxy

_showPanelInfo :: SProxy "showPanelInfo"
_showPanelInfo = SProxy