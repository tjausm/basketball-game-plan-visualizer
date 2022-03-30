{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Frontend where

import Common.Route
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Float (float2Int, int2Float)
import Obelisk.Frontend
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
        prerender_ blank $ renderView
        blank
    }

-- Renders the complete basketball court, players and controls
renderView :: (MonadWidget t m) =>  m ()
renderView = do
        let speedFactor = 2 -- todo make this configurable
        -- here we keep the fps constant at 30 regardless of the 'speedFactor'
        -- e.g. 2 seconds playtime at 0.5 speed and 30 fps is  2/0.5*30=120 fps
        let fps = float2Int (1 / speedFactor * 30)
        let speed = 1 / (toNDT speedFactor * toNDT fps)
        rec
          t0 <- liftIO Time.getCurrentTime
          eTick <- tickLossy speed t0
          movements <- mapM (renderMovements fps eStart ePause eTick) [mov1,mov2,mov3,mov4,mov5]
          svgBody 1000 750 $ do
            mapM (\m -> elDynSvgAttr "circle" m blank) movements
          eStart <- button "Start"
          ePause <- button "Reset"
        return ()

-- creates 1 dynamic map linked to the ticks and start & reset button
renderMovements :: (DomBuilder t m, MonadWidget t m) => Int -> Event t () -> Event t () -> Event t TickInfo -> Movement -> m (Dynamic t (Map.Map T.Text T.Text))
renderMovements fps eStart ePause eTick mov = do
  beStartStop <- hold never . leftmost $ [ (const 0 <$ eTick) <$ ePause, ((1+) <$ eTick) <$ eStart ]
  let eSwitch = switch beStartStop
  fmap (getFrameAt mov fps) <$> foldDyn ($) 0 eSwitch

toNDT :: (Real a) => a -> Time.NominalDiffTime
toNDT = fromRational . toRational

-- Draw calculations

-- using elDynAttrNS' to build svg in prerender (https://github.com/obsidiansystems/obelisk/issues/828)
elDynSvgAttr  :: (DomBuilder t m, PostBuild t m)
  => T.Text
  -> Dynamic t (Map.Map T.Text T.Text)
  -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynSvgAttr = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

svgBody :: (DomBuilder t m, PostBuild t m) => Int -> Int -> m a3 -> m (Element EventResult (DomBuilderSpace m) t, a3)
svgBody h w = elDynSvgAttr "svg" $ constDyn $ "width" =: toText h <> "height" =: toText w

toText :: (Show a) => a -> T.Text
toText = T.pack . show

circleAttr :: Int -> Point -> Map.Map T.Text T.Text
circleAttr r (x, y) = "cx" =: toText x <> "cy" =: toText y <> "r" =: toText r <> "fill" =: "black"

-------------------------
-- Movement calculations-
-------------------------

data Player = One
  deriving (Show)

type Seconds = Int

type Point = (Float, Float)

data Arrow = Arrow
  { start :: Point,
    end :: Point
  }
  deriving (Show)

data Movement = Movement
  { player :: Player,
    arrow :: Arrow,
    startTime :: Seconds,
    endTime :: Seconds
  }
  deriving (Show)


mkArrow :: Point -> Point -> Arrow
mkArrow p1 p2 = Arrow {start = p1, end = p2}

mkMovement :: Arrow -> Seconds -> Seconds -> Movement
mkMovement a beginT endT = Movement {player = One, arrow = a, startTime = if beginT > endT then endT else beginT, endTime = endT}

mov1 :: Movement
mov1 = mkMovement (mkArrow (100, 100) (500, 500)) 1 6

mov2 :: Movement
mov2 = mkMovement (mkArrow (100, 100) (100, 500)) 0 10

mov3 :: Movement
mov3 = mkMovement (mkArrow (500, 100) (100, 500)) 0 3

mov4 :: Movement
mov4 = mkMovement (mkArrow (300, 500) (300, 100)) 0 5

mov5 :: Movement
mov5 = mkMovement (mkArrow (600, 100) (100, 500)) 0 5

-- Difference between 2 points
diff :: Point -> Point -> Point
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- divide points x and y coordinate by a float
divPoint :: Point -> Int -> Point
divPoint (x, y) n = let n' = int2Float n in (x / n', y / n')

-- return stepsize given the n of frames a movement takes
stepSize :: Movement -> Int -> Point
stepSize mov travTime = diff (start (arrow mov)) (end (arrow mov)) `divPoint` travTime

--framesPerSecond :: Int
--framesPerSecond = 60

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