module McPanel.Transform where

import Prelude

import Control.Monad.Free.Extra (cataFree, FreeF(..))
import Data.Filterable (partition)
import Data.Foldable (foldl, maximumBy)
import Data.FoldableWithIndex (findWithIndex)
import Data.Int (pow)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as L
import Data.Map (Map)
import Data.Map (lookup, singleton, union) as M
import Data.Maybe (fromJust, maybe)
import Data.Symbol (SProxy(..))
import McPanel.Model (Direction(..)) as M
import McPanel.Model (Panel, Split(..), Layout)
import Partial.Unsafe (unsafePartial)

data Direction = U | D | L | R

instance showDirection :: Show Direction where
  show x = case x of
    U -> "U"
    D -> "D"
    L -> "L" 
    R -> "R"

derive instance eqDirection  :: Eq Direction
derive instance ordDirection :: Ord Direction

type Coord = { x :: Int, y :: Int }

shiftFocus :: forall a. Ord a => Direction -> Panel a -> Panel a
shiftFocus d p = maybe p (\a -> p { focus = a }) $ findNewFocus newCoord rects
  where
  newCoord = applyMove d case d of
    U -> focusedRect.tl
    D -> br' focusedRect
    L -> focusedRect.tl
    R -> br' focusedRect
  rects = rectMap p.layout
  focusedRect = unsafePartial $ fromJust $ M.lookup p.focus rects
  findNewFocus c = map _.index <<< findWithIndex (\_ -> inRect c)

rectMap :: forall a. Ord a => Layout a -> Map a Rect
rectMap l = let bm = toBranchMap l in toRectMap (maxCoord bm) bm

toBranchMap :: forall a. Ord a => Layout a -> Map a (List Direction)
toBranchMap = cataFree go
  where
  go (Pure a)  = M.singleton a mempty
  go (Free (Split { direction, first, next })) = M.union first' next'
    where
    first' = map (L.Cons dir.l) first
    next'  = map (L.Cons dir.r) next
    dir = case direction of
      M.Horizontal -> { l: U, r: D }
      M.Vertical   -> { l: L, r: R }

toRectMap :: forall a. Ord a => Coord -> Map a (List Direction) -> Map a Rect
toRectMap br = map (foldl restrict { tl: { x: 0, y: 0 }, br })

coordData :: forall a. Ord a => Panel a -> Panel Rect
coordData p = { layout, focus }
  where
  cs = let bm = toBranchMap p.layout in toRectMap (maxCoord bm) bm
  --cs = toBranchMap p.layout
  layout = map (\x -> unsafePartial $ fromJust $ M.lookup x cs) p.layout
  focus = unsafePartial $ fromJust $ M.lookup p.focus cs

applyMove :: Direction -> Coord -> Coord
applyMove d x = case d of
  D -> over (prop _y) (add 1) x
  U -> over (prop _y) (add (-1)) x
  R -> over (prop _x) (add 1) x
  L -> over (prop _x) (add (-1)) x

spans :: List Direction -> { vertical :: List Direction, horizontal :: List Direction }
spans ds =
  let { yes, no } = partition isVertical ds
  in { vertical: yes, horizontal: no }
  where
  isVertical x = case x of
    U -> true
    D -> true
    _ -> false

maxCoord :: forall a. Map a (List Direction) -> Coord
maxCoord m = { x: pow 2 hmax, y: pow 2 vmax }
  where
  vmax = go _.vertical
  hmax = go _.horizontal
  go f = L.length <<< f <<< spans $ dmax f
  dmax f = unsafePartial $ fromJust $ maximumBy (comparing $ spans >>> f >>> L.length) m

type Rect = { tl :: Coord, br :: Coord }

-- The bottom right is actually br - { x: 1, y: 1 }
br' :: Rect -> Coord
br' { tl, br } = { x: br.x - 1, y: br.y - 1 }

restrict :: Rect -> Direction -> Rect
restrict r d = case d of
  U -> over (prop _br <<< prop _y) (\x -> x - vdelta) r
  D -> over (prop _tl <<< prop _y) (\x -> x + vdelta) r
  R -> over (prop _tl <<< prop _x) (\x -> x + hdelta) r
  L -> over (prop _br <<< prop _x) (\x -> x - hdelta) r
  where
  hdelta = div (r.br.x - r.tl.x) 2
  vdelta = div (r.br.y - r.tl.y) 2

inRect :: Coord -> Rect -> Boolean
inRect c r = 
     c.x >= r.tl.x && c.x < r.br.x
  && c.y >= r.tl.y && c.y < r.br.y

_x :: SProxy "x"
_x = SProxy

_y :: SProxy "y"
_y = SProxy

_br :: SProxy "br"
_br = SProxy

_tl :: SProxy "tl"
_tl = SProxy