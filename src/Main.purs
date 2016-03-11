module Main where

import Prelude (class Functor, Unit, bind, unit, map, show, pure, const, (*), (+), (/), ($))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import CSS.Border as Border
import CSS.Color as Color
import CSS.Display as Display
import CSS.Geometry as Geometry
import CSS.Overflow as Overflow
import CSS.TextAlign as TextAlign
import CSS.Size as Size
import Data.Array ((..), length)
import Data.Int (toNumber)
import Halogen (HalogenEffects, ComponentDSL, Natural, ComponentHTML, Component, runUI, component, action, modify)
import Halogen.Util (appendToBody)
import Halogen.HTML.Core (className)
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Math ((%))
import DOM.HTML.Types (HTMLElement)

foreign import getScrollTop :: HTMLElement -> Int

type VisibleIndices = Array Int

type State =
  { height :: Int
  , width :: Int
  , colWidth :: Int
  , rowCount :: Int
  , rowHeight :: Int
  , visibleIndices :: Array Int
  }

data Query a
  = Init a
  | UserScroll Int a

calculateVisibleIndices :: State -> Int -> State
calculateVisibleIndices model scrollTop =
  case model of
    { rowHeight, rowCount, height } -> do
      let firstRow = scrollTop / rowHeight
      let visibleRows = (height + 1) / rowHeight
      let lastRow = firstRow + visibleRows

      model { visibleIndices = firstRow..lastRow }

tableView :: State -> ComponentHTML Query
tableView { rowCount, rowHeight, colWidth, visibleIndices } = do
  let rows =
    map
        (\index -> do
          let i = toNumber index
          let key = show (i % (toNumber (length visibleIndices)))

          H.tr
            [ P.key key
            , CSS.style do
                Display.position Display.absolute
                Geometry.top (Size.px (i * (toNumber rowHeight)))
                Geometry.width (Size.pct (toNumber 100))
            ]
            [ H.td
                [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
                [ H.text $ show i
                ]
            , H.td
                [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
                [ H.text $ show (i * 1.0)
                ]
            , H.td
                [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
                [ H.text $ show (i * 100.0)
                ]
            ]
          )
        visibleIndices

  H.table
    [ CSS.style do
        Geometry.height (Size.px (toNumber (rowCount * rowHeight)))
    ]
    [ H.tbody_ rows
    ]

ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render state =
      H.div
        [ P.initializer \_ -> action Init
        ]
        [ H.h1
          [ CSS.style do TextAlign.textAlign TextAlign.center
          ]
          [ H.text "Scroll Table!!!!"
          ]
        , H.div_
            [ H.div
                [ P.class_ (className "container")
                , CSS.style do
                    Display.position Display.relative
                    Geometry.height (Size.px (toNumber state.height))
                    Geometry.width (Size.px (toNumber state.width))
                    Overflow.overflowX Overflow.hidden
                    Border.border Border.solid (Size.px 1.0) Color.black
                , E.onScroll (E.input \x -> do
                    let top = getScrollTop x.target
                    pure $ action (UserScroll top))
                ]
                [ tableView state
                ]
            ]
        ]

    eval :: Natural Query (ComponentDSL State Query g)
    eval (Init next) =
      pure next
    eval (UserScroll e next) = do
      modify (\s -> calculateVisibleIndices s e)
      pure next

initialState :: State
initialState =
  calculateVisibleIndices
    { height: 600
    , width: 900
    , colWidth: 300
    , rowCount: 10000
    , rowHeight: 30
    , visibleIndices: []
    }
    0

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI ui initialState
  appendToBody app.node
