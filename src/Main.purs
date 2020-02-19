module Main where

import Prelude

import App (app, initState, reducer, setupListeners)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (throwException, error)
import Effect.Ref as Ref
import Snap (snap)
import Snap.React (reactTarget, refSnapper')
import Snap.SYTC.Component (contraHoist)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

element :: Effect Element
element = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById "container"
  maybe (throwException (error "Couldn't find root element")) pure mc

main :: Effect Unit
main = do
  e   <- element
  ref <- Ref.new initState
  launchAff_ do
    av <- AVar.empty
    let snapper = refSnapper' (\a -> pure <<< reducer a) ref av
    let target  = reactTarget e av
    liftEffect $ setupListeners (launchAff_ <<< snapper.put)
    snap snapper (contraHoist launchAff_ app) target