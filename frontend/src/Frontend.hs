{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Frontend where

import Common.Route
import Control.Monad.Fix
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Time as Time
import Data.List ( unfoldr )
import Obelisk.Frontend
import Obelisk.Generated.Static
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
        elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank,
      _frontend_body = do

        -- from https://github.com/obsidiansystems/obelisk/issues/856
        el "div" $ do
          dETick <- prerender (return never) $ do
            new <- liftIO Time.getCurrentTime
            eTick <- tickLossy 2 new
            return $ T.pack . show . _tickInfo_n <$> eTick -- _tickInfo_n <$> -> gets amount of ticks as integer
          --let (test :: Dynamic t (Event t T.Text)) = dETick
          dynText =<< (holdDyn "No Ticks Yet" $ switchDyn $ dETick)

        el "div" $ do
          dETick <- prerender (return never) $ do
            new <- liftIO Time.getCurrentTime
            eTick <- tickLossy 2 new
            return $ getFrameAt testMovement <$> (_tickInfo_n <$> eTick) 
          --let (test :: Dynamic t (Event t (Map.Map T.Text T.Text))) = dETick
          -- holdDyn :: a -> Event t a -> m (Dynamic t a) 
          -- switchDyn :: Dynamic t (Event t a) -> Event t a 
          -- elDynAttr ::  Text -> Dynamic t (Map Text Text) -> m a -> m a
          let (kkr1 :: Dynamic t (Map.Map T.Text T.Text) -> m a -> m a) = elDynAttr "circle"
          let (kanker :: _) = elDynAttr "circle" =<< (holdDyn (Map.empty :: Map.Map T.Text T.Text) $ switchDyn dETick) 
          return ()
        
        rec el "div" $ text "Counter as fold"
            numbs <- foldDyn (+) (0 :: Int)  (1 <$ evIncr)
            el "div" $ display numbs
            evIncr <- button "Increment"
        return ()
    }


emptyVal :: Map.Map T.Text T.Text
emptyVal = Map.empty

svg :: (DomBuilder t m) => Int -> Int -> m a2 -> m a2
svg h w = elAttr "svg" ("width" =: (toText h) <> "height" =: (toText w)) 

circle :: (DomBuilder t m) => Int -> Point -> m a -> m a
circle r p = elAttr "circle" (circleAttr r p) 

data Player = One
  deriving (Show)

type Seconds = Float

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
toText =  T.pack . show

circleAttr :: Int -> Point -> Map.Map T.Text T.Text
circleAttr r (x,y) = "cx" =: toText x <> "cy" =: toText y<> "r" =: toText r <> "fill" =: "yellow"

-- Movement calculations
-- 
testArrow :: Arrow
testArrow = Arrow {start = (100,100), end = (500,500)}

testMovement :: Movement
testMovement = Movement {player = One, arrow = testArrow, startTime = 2, endTime = 8}

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

diff :: Point -> Point -> Point
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

divPoint :: Point -> Float -> Point
divPoint (x, y) n = (x / n, y / n)

stepSize :: Movement -> Float -> (Float, Float)
stepSize mov travTime = diff (start (arrow mov)) (end (arrow mov)) `divPoint` travTime

-- renders a list of points where the circle is at each moment in time 
-- from it's starting time to it's end time with intervals of 1 second
renderFrames :: Movement -> [Point]
renderFrames mov =
  let
      travelTime = endTime mov - startTime mov
      startP = start (arrow mov)
      endP = end (arrow mov)
      step = stepSize mov travelTime
      getNext point
        | nextPoint < endP = Just (point, nextPoint)
        | otherwise = Nothing
        where
            nextPoint = point `addPoint` step
   in unfoldr getNext startP ++ [endP]

-- Calculate attributes of cicle at given frame
getFrameAt :: Movement -> Integer -> Map.Map T.Text T.Text
getFrameAt mov posIntegral =
    let
        allFrames = renderFrames mov
        pos = fromIntegral posIntegral
        currFrame = if length allFrames > pos then allFrames !! pos else last allFrames
    in circleAttr 10 currFrame
