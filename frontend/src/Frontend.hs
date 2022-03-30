{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

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
import Control.Monad (join)

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
        -- from https://github.com/obsidiansystems/obelisk/issues/856
        {--
        el "h2" $ text "Using foldDyn with function application"
        rec dynNum <- foldDyn ($) (0 :: Int) $ leftmost [(+ 1) <$ evIncr, (+ (-1)) <$ evDecr, const 0 <$ evReset]
            let currAttr = getFrameAt testMovement <$> dynNum
            svg 1000 750 $ do
              elDynAttr "circle" currAttr blank
            evIncr <- button "Increment"
            evDecr <- button "Decrement"
            evReset <- button "Reset"
        return ()
        --}
        prerender_ blank $ timerWidget
        blank
        {--
        let speedF = 1
        svg 1000 750 $ do
          moveCircle speedF mov1
          moveCircle speedF mov2
          moveCircle speedF mov3
          moveCircle speedF mov4
          moveCircle speedF mov5--}
    }

timerWidget :: MonadWidget t m =>  m ()
timerWidget = do
        rec
          t0 <- liftIO Time.getCurrentTime
          eTick <- tickLossy 1.0 t0
          circleTrajectory <- timer' eStart ePause eTick
          svgBody 1000 750 $ do
            elSvgAttr "circle" circleTrajectory blank
          eStart <- button "Start"
          ePause <- button "Reset"
        return ()


timer' :: MonadWidget t m => Event t () -> Event t () -> Event t TickInfo -> m (Dynamic t (Map.Map T.Text T.Text))
timer' eStart ePause eTick = do
  beStartStop <- hold never . leftmost $ [ (const 0 <$ eTick) <$ ePause, ((1+) <$ eTick) <$ eStart ]
  let eSwitch = switch beStartStop
  fmap (getFrameAt mov1 3) <$> foldDyn ($) 0 eSwitch

toNDT :: (Real a) => a -> Time.NominalDiffTime
toNDT = fromRational . toRational

-- given a speedFactor e.g. 0.5 is half speed, 2 is double speed
-- and a movement we can move a circle over a canvas
moveCircle ::
  (PostBuild t m, DomBuilder t m, Prerender t m, MonadHold t m) =>
  Float ->
  Movement ->
  m ()
moveCircle speedFactor mov =
  do
    dETick <- prerender (return never) $ do
      new <- liftIO Time.getCurrentTime
      eTick <- tickLossy speed new
      return $ getFrameAt mov fps . fromInteger <$> (_tickInfo_n <$> eTick)
    let circleTrajectory = holdDyn Map.empty $ switchDyn dETick
    let dynCircle d = elDynAttr "circle" d blank
    dynCircle =<< circleTrajectory
  where
    -- here we keep the fps constant at 30 regardless of the 'speedFactor'
    -- e.g. 2 seconds playtime at 0.5 speed and 30 fps is  2/0.5*30=120 fps
    fps = float2Int (1 / speedFactor * 30)
    speed = 1 / (toNDT speedFactor * toNDT fps)

-- Draw calculations

-- using elDynAttrNS' to build svg in prerender (https://github.com/obsidiansystems/obelisk/issues/828)
elSvgAttr  :: (DomBuilder t m, PostBuild t m)
  => T.Text
  -> Dynamic t (Map.Map T.Text T.Text)
  -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elSvgAttr = elDynAttrNS' (Just "http://www.w3.org/2000/svg")  

svgBody :: (DomBuilder t m, PostBuild t m) => Int -> Int -> m a3 -> m (Element EventResult (DomBuilderSpace m) t, a3)
svgBody h w = elSvgAttr "svg" $ constDyn $ "width" =: toText h <> "height" =: toText w

circle :: (DomBuilder t m) => Int -> Point -> m a -> m a
circle r p = elAttr "circle" (circleAttr r p)

toText :: (Show a) => a -> T.Text
toText = T.pack . show

circleAttr :: Int -> Point -> Map.Map T.Text T.Text
circleAttr r (x, y) = "cx" =: toText x <> "cy" =: toText y <> "r" =: toText r <> "fill" =: "black"

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

-------------------------
-- Movement calculations-
-------------------------
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

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

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