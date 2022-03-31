{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Frontend where

import Common.Route
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Float (float2Int, int2Float)
import Obelisk.Frontend
import Obelisk.Generated.Static (static)
import Obelisk.Route
import Reflex.Dom.Core

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Obelisk Minimal Example"
        elAttr "link" ("href" =: "main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank,
      _frontend_body = do
        prerender_ blank renderView

        blank
    }

-- Renders the complete basketball court, players and controls
renderView :: (MonadWidget t m) => m ()
renderView = do
  let movList = [mov1, mov2, mov3, mov5] -- TODO make configurable
  -- here we keep the fps constant at 30 regardless of the 'speedFactor'
  -- e.g. 2 seconds playtime at 0.5 speed and 30 fps is  2/0.5*30=120 fps
  let speedFactor = 2 -- todo make this configurable
  let fps = float2Int (1 / speedFactor * 30)
  let speed = 1 / (toNDT speedFactor * toNDT fps)
  rec t0 <- liftIO Time.getCurrentTime
      eTick <- tickLossy speed t0
      let arrows = map arrow movList
      movements <- mapM (renderMovements fps eStart ePause eReset eTick) movList
      svgBBalCourt $ do
        mapM_ drawArrow arrows
        mapM (\m -> elDynSvgAttr "circle" m blank) movements
      eStart <- button "Start"
      ePause <- button "Pause"
      eReset <- button "Reset"
  return ()

-- creates 1 dynamic map linked to the ticks and start & reset button
renderMovements :: (DomBuilder t m, MonadWidget t m) => Int -> Event t () -> Event t () -> Event t () -> Event t TickInfo -> Movement -> m (Dynamic t (Map.Map T.Text T.Text))
renderMovements fps eStart ePause eReset eTick mov = do
  beStartStop <- hold never . leftmost $ [((1 +) <$ eTick) <$ eStart, ((0 +) <$ eTick) <$ ePause, (const 0 <$ eTick) <$ eReset]
  let eSwitch = switch beStartStop
  fmap (getFrameAt mov fps) <$> foldDyn ($) 0 eSwitch

toNDT :: (Real a) => a -> Time.NominalDiffTime
toNDT = fromRational . toRational

-- Draw calculations

-- using elDynAttrNS' to build svg in prerender (https://github.com/obsidiansystems/obelisk/issues/828)

type Height = Int

type Width = Int

elDynSvgAttr ::
  (DomBuilder t m, PostBuild t m) =>
  T.Text ->
  Dynamic t (Map.Map T.Text T.Text) ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
elDynSvgAttr = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

svgBody :: (DomBuilder t m, PostBuild t m) => Height -> Width -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
svgBody h w = elDynSvgAttr "svg" $ constDyn $ "height" =: toText h <> "width" =: toText w

svgRect :: (DomBuilder t m, PostBuild t m) => Point -> Height -> Width -> T.Text -> m (Element EventResult (DomBuilderSpace m) t, ())
svgRect (x, y) h w style = elDynSvgAttr "rect" (constDyn $ "x" =: toText x <> "y" =: toText y <> "height" =: toText h <> "width" =: toText w <> "style" =: style) blank

svgBBalCourt :: (MonadWidget t m) => m a -> m ()
svgBBalCourt m = do
  svgBody 800 1000 $ do
    svgRect (0, 0) 800 1000 "fill:white;stroke:black;stroke-width:5;opacity:1"
    svgRect (250, 0) 400 500 "fill:brown;stroke:black;stroke-width:5;opacity:0.5"
    m
  return ()

drawLine :: (DomBuilder t m, PostBuild t m) => Point -> Point -> Map.Map T.Text T.Text -> m (Element EventResult (DomBuilderSpace m) t, ())
drawLine (x1, y1) (x2, y2) otherAttrs = elDynSvgAttr "line" (constDyn $ "x1" =: toText x1 <> "y1" =: toText y1 <> "x2" =: toText x2 <> "y2" =: toText y2 <> "stroke-width" =: "5" <> "style" =: "fill:black;stroke:black" <> otherAttrs) blank

drawArrow :: (DomBuilder t m, PostBuild t m) => Arrow -> m (Element EventResult (DomBuilderSpace m) t, ())
drawArrow a = do
  let (p1, p2) = getPoints a
  elDynSvgAttr "marker" (constDyn $ "id" =: "triangle" <> "viewBox" =: "0 0 10 10" <> "markerUnits" =: "strokeWidth" <> "markerWidth" =: "4" <> "markerHeight" =: "3" <> "refX" =: "0" <> "refY" =: "5" <> "orient" =: "auto") $ do
    elDynSvgAttr "path" (constDyn $ "d" =: "M 0 0 L 10 5 L 0 10 z") blank
  elAttr "polygon" ("points" =: "0 0, 10 3.5, 0 7") blank
  drawLine p1 p2 ("marker-end" =: "url(#triangle)")

toText :: (Show a) => a -> T.Text
toText = T.pack . show

circleAttr :: Int -> Point -> Map.Map T.Text T.Text
circleAttr r (x, y) = "cx" =: toText x <> "cy" =: toText y <> "r" =: toText r <> "fill" =: "black"

-------------------------
-- Movement calculations-
-------------------------
type Seconds = Int

type Point = (Float, Float)

newtype Player = Player T.Text
  deriving (Show)

data Arrow = One | Two | Three | Four
  deriving (Show)

getPoints :: Arrow -> (Point, Point)
getPoints One = ((100, 100), (500, 500))
getPoints Two = ((100, 100), (100, 500))
getPoints Three = ((500, 100), (100, 500))
getPoints Four = ((300, 500), (300, 100))

start :: Arrow -> Point
start = fst . getPoints

end :: Arrow -> Point
end = snd . getPoints

data Movement = Movement
  { player :: Player,
    arrow :: Arrow,
    startTime :: Seconds,
    endTime :: Seconds
  }
  deriving (Show)

mkMovement :: Arrow -> Seconds -> Seconds -> Movement
mkMovement a beginT endT = Movement {player = Player "One", arrow = a, startTime = if beginT > endT then endT else beginT, endTime = endT}

mov1 :: Movement
mov1 = mkMovement One  1 6

mov2 :: Movement
mov2 = mkMovement Two  0 10

mov3 :: Movement
mov3 = mkMovement Three 0 3

mov5 :: Movement
mov5 = mkMovement Four 0 5

-- Difference between 2 points
diff :: Point -> Point -> Point
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- divide points x and y coordinate by a float
divPoint :: Point -> Int -> Point
divPoint (x, y) n = let n' = int2Float n in (x / n', y / n')

-- return stepsize given the n of frames a movement takes
stepSize :: Movement -> Int -> Point
stepSize mov travTime = diff (start (arrow mov)) (end (arrow mov)) `divPoint` travTime

-- renders a list of 'frames' (= position on x and y axes of circle at moment in time)
-- from it's starting frame to it's end frame with 30 frames per second
renderFrames :: Movement -> Int -> [Point]
renderFrames mov framesPerSecond =
  let stationaryFrames = replicate (startTime mov * framesPerSecond) (start $ arrow mov)
      nOfFrames = ((endTime mov - startTime mov) * framesPerSecond) - 2 -- leave 2 frames for begin and end frame
      (xStart, yStart) = start (arrow mov)
      (xStep, yStep) = stepSize mov nOfFrames
   in stationaryFrames ++ [(xStart + i * xStep, yStart + yStep * i) | i <- [0 .. (int2Float nOfFrames)]] ++ [end (arrow mov)]

-- Calculate attributes of cicle at given frame
getFrameAt :: Movement -> Int -> Int -> Map.Map T.Text T.Text
getFrameAt mov framesPerSecond frameN =
  let allFrames = renderFrames mov framesPerSecond
      currFrame = if length allFrames > frameN then allFrames !! frameN else last allFrames
   in circleAttr 10 currFrame