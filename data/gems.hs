{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Blaze
import Text.Blaze.Internal ( MarkupM(Parent) )
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Renderer.String
import Data.Monoid

main :: IO ()
main = putStrLn $ renderMarkup top

squareWidth, squareHeight :: Int
squareWidth  = 30
squareHeight = 26

row :: Int -> Int
row r = squareHeight * (5 - r) - quot squareHeight 2

col :: Int -> Int
col c = squareWidth * c + quot squareWidth 2

drum :: Int -> Int -> S.Svg
drum x y = S.rect
  ! A.width "20"
  ! A.height "20"
  ! A.x (toValue $ x - 10)
  ! A.y (toValue $ y - 10)
  ! A.rx "2"
  ! A.ry "2"

flam :: Int -> Int -> S.Svg
flam x y = S.rect
  ! A.width "20"
  ! A.height (toValue $ 20 + squareHeight)
  ! A.x (toValue $ x - 10)
  ! A.y (toValue $ y - 10 - squareHeight)
  ! A.rx "2"
  ! A.ry "2"

ghost :: Int -> Int -> S.Svg
ghost x y = S.rect
  ! A.width "10"
  ! A.height "10"
  ! A.x (toValue $ x - 5)
  ! A.y (toValue $ y - 5)
  ! A.rx "2"
  ! A.ry "2"

crash :: Int -> Int -> S.Svg
crash x y = S.circle
  ! A.r "10"
  ! A.cx (toValue x)
  ! A.cy (toValue y)

closedHihat :: Int -> Int -> S.Svg
closedHihat x y = let
  pts =
    [ (x     , y - 10)
    , (x - 10, y     )
    , (x     , y + 10)
    , (x + 10, y     )
    ]
  in S.polygon ! A.points (toValue $ polygonPoints pts)

footHihat :: Int -> Int -> S.Svg
footHihat x y = let
  pts =
    [ (x    , y - 5)
    , (x - 5, y    )
    , (x    , y + 5)
    , (x + 5, y    )
    ]
  in S.polygon ! A.points (toValue $ polygonPoints pts)

openHihat :: Int -> Int -> S.Svg
openHihat x y = let
  pts =
    [ (x     , y - 10)
    , (x - 10, y - 5 )
    , (x - 5 , y     )
    , (x - 10, y + 5 )
    , (x     , y + 10)
    , (x + 10, y + 5 )
    , (x + 5 , y     )
    , (x + 10, y - 5 )
    ]
  in S.polygon ! A.points (toValue $ polygonPoints pts)

ride :: Int -> Int -> S.Svg
ride x y = let
  pts =
    [ (x     , y - 10)
    , (x - 10, y + 10)
    , (x + 10, y + 10)
    ]
  in S.polygon ! A.points (toValue $ polygonPoints pts)

polygonPoints :: [(Int, Int)] -> String
polygonPoints = unwords . map (\(x, y) -> show x ++ "," ++ show y)

style :: S.Svg
style = Parent "style" "<style" "</style>" $ preEscapedToMarkup $ unlines
  [ ".thick  { stroke-width: 2; stroke: black; stroke-linejoin: round }"
  , ".orange { fill: rgb(200, 100, 30 ) }"
  , ".red    { fill: rgb(200, 30 , 30 ) }"
  , ".yellow { fill: rgb(170, 170, 30 ) }"
  , ".blue   { fill: rgb(60 , 60 , 200) }"
  , ".green  { fill: rgb(50 , 200, 50 ) }"
  , ".blackLine { stroke-width: 2; stroke: black }"
  , ".blackLine2 { stroke-width: 2; stroke: black; stroke-linecap: round }"
  , ".grayLine { stroke-width: 2; stroke: rgb(170, 170, 170) }"
  ]

rightHand :: Int -> Int -> S.Svg
rightHand x y = S.line
  ! A.x1 (toValue x)
  ! A.x2 (toValue $ x + 9)
  ! A.y1 (toValue y)
  ! A.y2 (toValue $ y + 9)
  ! A.class_ "blackLine2"

leftHand :: Int -> Int -> S.Svg
leftHand x y = S.line
  ! A.x1 (toValue x)
  ! A.x2 (toValue $ x - 9)
  ! A.y1 (toValue y)
  ! A.y2 (toValue $ y + 9)
  ! A.class_ "blackLine2"

top :: S.Svg
top = let
  fns =
    [ \c -> S.line
      ! A.x1 (toValue $ col c)
      ! A.x2 (toValue $ col c)
      ! A.y1 (toValue $ row 0)
      ! A.y2 (toValue $ row 4)
      ! A.class_ "blackLine"
    , \c -> S.line
      ! A.x1 (toValue $ col c)
      ! A.x2 (toValue $ col c)
      ! A.y1 (toValue $ row 0)
      ! A.y2 (toValue $ row 4)
      ! A.class_ "grayLine"
    , \c -> S.line
      ! A.x1 (toValue $ col c)
      ! A.x2 (toValue $ col c)
      ! A.y1 (toValue $ row 1)
      ! A.y2 (toValue $ row 3)
      ! A.class_ "grayLine"
    , \c -> rightHand   (col c) (row 0)
    , \c -> rightHand   (col c) (row 1)
    , \c -> rightHand   (col c) (row 2)
    , \c -> rightHand   (col c) (row 3)
    , \c -> rightHand   (col c) (row 4)
    , \c -> leftHand    (col c) (row 0)
    , \c -> leftHand    (col c) (row 1)
    , \c -> leftHand    (col c) (row 2)
    , \c -> leftHand    (col c) (row 3)
    , \c -> leftHand    (col c) (row 4)
    , \c -> drum        (col c) (row 0) ! A.class_ "thick orange"
    , \c -> drum        (col c) (row 1) ! A.class_ "thick red"
    , \c -> drum        (col c) (row 2) ! A.class_ "thick yellow"
    , \c -> drum        (col c) (row 3) ! A.class_ "thick blue"
    , \c -> drum        (col c) (row 4) ! A.class_ "thick green"
    , \c -> flam        (col c) (row 1) ! A.class_ "thick red"
    , \c -> ghost       (col c) (row 0) ! A.class_ "thick orange"
    , \c -> ghost       (col c) (row 1) ! A.class_ "thick red"
    , \c -> ghost       (col c) (row 2) ! A.class_ "thick yellow"
    , \c -> ghost       (col c) (row 3) ! A.class_ "thick blue"
    , \c -> ghost       (col c) (row 4) ! A.class_ "thick green"
    , \c -> crash       (col c) (row 2) ! A.class_ "thick yellow"
    , \c -> crash       (col c) (row 3) ! A.class_ "thick blue"
    , \c -> crash       (col c) (row 4) ! A.class_ "thick green"
    , \c -> closedHihat (col c) (row 2) ! A.class_ "thick yellow"
    , \c -> closedHihat (col c) (row 3) ! A.class_ "thick blue"
    , \c -> closedHihat (col c) (row 4) ! A.class_ "thick green"
    , \c -> openHihat   (col c) (row 2) ! A.class_ "thick yellow"
    , \c -> openHihat   (col c) (row 3) ! A.class_ "thick blue"
    , \c -> openHihat   (col c) (row 4) ! A.class_ "thick green"
    , \c -> footHihat   (col c) (row 0) ! A.class_ "thick yellow"
    , \c -> ride        (col c) (row 2) ! A.class_ "thick yellow"
    , \c -> ride        (col c) (row 3) ! A.class_ "thick blue"
    , \c -> ride        (col c) (row 4) ! A.class_ "thick green"
    ]
  in S.docTypeSvg
    ! A.width  (toValue $ squareWidth  * length fns)
    ! A.height (toValue $ squareHeight * 5)
    $ mappend style $ mconcat $ zipWith ($) fns [0..]
