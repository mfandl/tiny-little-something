module Main where

import Prelude

import Data.Int (toNumber)
import Data.Enum (enumFromTo)
import Data.JSDate (now, getTime)
import Data.Maybe (fromJust)
import Effect (Effect, foreachE)
import Graphics.Canvas (Context2D, fillRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setFillStyle)
import Math (cos, sin)
import Partial.Unsafe (unsafePartial)
import Web.HTML (window)
import Web.HTML.Window (Window, requestAnimationFrame)

newtype DisplayInfo = DisplayInfo {
  width :: Number,
  height :: Number
  }

newtype Vec2 = Vec2 {
  x :: Number,
  y :: Number
  }

derive newtype instance vec2Show :: Show Vec2

newtype Star = Star {
  position :: Vec2,
  size :: Number
  }

derive newtype instance vec2Star :: Show Star

type Seconds = Number

stars :: Array Star
stars = map (createStar <<< toNumber) $ enumFromTo 1 100
  where
        createStar :: Number -> Star
        createStar i = Star {
            position: Vec2 { x: 10.0 + (psin i) * 300.0, y: 10.0 + (pcos i) * 220.0 },
            size: 50.0
          }
        psin :: Number -> Number
        psin a = (1.0 + (sin a)) / 2.0
        pcos :: Number -> Number
        pcos a = (1.0 + (cos a)) / 2.0

generateStars :: Array Vec2 -> Array Star
generateStars = map createStar
  where
        createStar v = Star {
            position: v,
            size: 1.0
          }

getCurrentTimeSeconds :: Effect Seconds
getCurrentTimeSeconds = do
  currentTime <- getTime <$> now
  pure $ currentTime / 1000.0

drawStar ::Context2D -> Number -> Star -> Effect Unit
drawStar ctx elapsedTime (Star { position: Vec2 { x, y }, size }) =
  fillRect ctx { x: x + x * (sin (elapsedTime * 2.0)), y: y + y * (cos (elapsedTime * 2.1)), width: size, height: (size) * (sin (elapsedTime * 10.0))
  }

draw :: Window -> DisplayInfo -> Context2D -> Effect Unit
draw wnd di@(DisplayInfo { width, height }) ctx = void $ requestAnimationFrame
  do
    draw wnd di ctx
    t <- getCurrentTimeSeconds
    setFillStyle ctx "rgba(255, 255, 255, 0.05)"
    fillRect ctx {
      x: 0.0,
      y: 0.0,
      width: width,
      height: height }
    setFillStyle ctx "rgba(0, 0, 0, 1)"
    foreachE stars (drawStar ctx t)
  wnd

main :: Effect Unit
main = do
  wnd <- window
  canvas <- unsafePartial $ fromJust <$> getCanvasElementById "cnvs"
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  draw wnd (DisplayInfo { width: w, height: h }) ctx

