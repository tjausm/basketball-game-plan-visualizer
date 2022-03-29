{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Common.Route
import Control.Monad.IO.Class (liftIO)
import Data.List (unfoldr)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import GHC.Float (int2Float)
import Data.Time (secondsToDiffTime, NominalDiffTime)

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
        svg 1000 750 $ do
          moveCircle mov1
          moveCircle mov2
          moveCircle mov3

    }

moveCircle ::
  (PostBuild t m, DomBuilder t m, Prerender t m, MonadHold t m) =>
  Movement ->
  m ()
moveCircle mov =
  do
      dETick <- prerender (return never) $ do
        new <- liftIO Time.getCurrentTime
        eTick <- tickLossy 0.033 new -- TODO: make this variable with frames per second
        return $ getFrameAt mov . fromInteger <$> (_tickInfo_n <$> eTick)
      let circleTrajectory = holdDyn Map.empty $ switchDyn dETick
      let dynCircle d = elDynAttr "circle" d blank
      dynCircle =<< circleTrajectory

svg :: (DomBuilder t m) => Int -> Int -> m a2 -> m a2
svg h w = elAttr "svg" ("width" =: (toText h) <> "height" =: (toText w))

circle :: (DomBuilder t m) => Int -> Point -> m a -> m a
circle r p = elAttr "circle" (circleAttr r p)

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

-- Draw calculations
toText :: (Show a) => a -> T.Text
toText = T.pack . show

circleAttr :: Int -> Point -> Map.Map T.Text T.Text
circleAttr r (x, y) = "cx" =: toText x <> "cy" =: toText y <> "r" =: toText r <> "fill" =: "black"

-- Movement calculations
--
mkArrow :: Point -> Point -> Arrow
mkArrow p1 p2 = Arrow {start = p1, end = p2}

mkMovement :: Arrow -> Seconds -> Seconds -> Movement
mkMovement a beginT endT = Movement {player = One, arrow = a, startTime = beginT, endTime = endT}

mov1 :: Movement
mov1 = mkMovement (mkArrow (100,100) (500,500)) 1 6
mov2 :: Movement
mov2 = mkMovement (mkArrow (100,100) (100,500)) 0 10
mov3 :: Movement
mov3 = mkMovement (mkArrow (500,100) (100,500)) 0 3



addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

diff :: Point -> Point -> Point
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

divPoint :: Point -> Int -> Point
divPoint (x, y) n = let n' = int2Float n in (x / n', y / n')

stepSize :: Movement -> Int -> (Float, Float)
stepSize mov travTime = diff (start (arrow mov)) (end (arrow mov)) `divPoint` travTime

-- framesPerSecond :: Int
framesPerSecond = 30

-- renders a list of 'frames' (= position on x and y axes of circle at moment in time)
-- from it's starting frame to it's end frame with 30 frames per second
renderFrames :: Movement -> [Point]
renderFrames mov =
  let  
      framesPerSecond = framesPerSecond
      stationaryFrames = replicate (startTime mov * framesPerSecond) (start $ arrow mov)
      nOfFrames = (endTime mov - startTime mov) * framesPerSecond
      startFrame = start (arrow mov)
      endFrame = end (arrow mov)
      step = stepSize mov nOfFrames
      getNext frame
        | endFrameIsPassed frame nextFrame = Nothing
        | otherwise = Just (frame, nextFrame) -- TODO cant compare points this way
        where
          endFrameIsPassed p1 p2  = undefined -- calculate rectange and check wether endframe lies withing
          nextFrame = frame `addPoint` step
   in stationaryFrames ++ unfoldr getNext startFrame ++ [endFrame]

-- Calculate attributes of cicle at given frame
getFrameAt :: Movement -> Int -> Map.Map T.Text T.Text
getFrameAt mov frameN =
  let allFrames = renderFrames mov 
      currFrame = if length allFrames > frameN then allFrames !! frameN else last allFrames
   in circleAttr 10 currFrame