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
import Data.Maybe (Maybe, maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import McPanel.Model (Direction(..)) as M
import McPanel.Model (Panel, Split(..), Layout)
import Record.Extra (sequenceRecord)

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
shiftFocus d p = maybe p (\a -> p { focus = a }) do
  c <- newCoord
  rs <- rects
  findNewFocus c rs
  where
  newCoord = map (applyMove d) case d of
    U -> map _.tl focusedRect
    D -> map br' focusedRect
    L -> map _.tl focusedRect
    R -> map br' focusedRect
  rects = rectMap p.layout
  focusedRect = rects >>= M.lookup p.focus
  findNewFocus c = map _.index <<< findWithIndex (\_ -> inRect c)

rectMap :: forall a. Ord a => Layout a -> Maybe (Map a Rect)
rectMap l = let bm = toBranchMap l in toRectMap bm <$> maxCoord bm 

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

toRectMap :: forall a. Ord a => Map a (List Direction) -> Coord -> Map a Rect
toRectMap m br = map (foldl restrict { tl: { x: 0, y: 0 }, br }) m

coordData :: forall a. Ord a => Panel a -> Maybe (Panel Rect)
coordData p = sequenceRecord { layout, focus }
  where
  mcs = 
    let bm = toBranchMap p.layout
    in toRectMap bm <$> maxCoord bm 
  layout = mcs >>= \cs -> traverse (flip M.lookup cs) p.layout
  focus  = mcs >>= M.lookup p.focus

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

maxCoord :: forall a. Map a (List Direction) -> Maybe Coord
maxCoord m = sequenceRecord { x: map (pow 2) hmax, y: map (pow 2) vmax }
  where
  vmax = go _.vertical
  hmax = go _.horizontal
  go f = L.length <<< f <<< spans <$> dmax f
  dmax f = maximumBy (comparing $ spans >>> f >>> L.length) m

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