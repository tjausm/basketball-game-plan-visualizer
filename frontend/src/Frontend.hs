{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend where

import Common.Route
import Control.Monad.Fix
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Time as Time
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core

 
data Player = One 
type Seconds = Int
type Point = (Int, Int)

data Arrow = Arrow
  {
    start :: Point,
    end :: Point
  }

data Movement = Movement
  {
    player :: Player,
    arrow :: Arrow,
    startTime :: Seconds,
    endTime :: Seconds
  }

renderFrames :: Movement -> [Point]
renderFrames mov  = 
  let 
    travelTime = min 0 $ (startTime mov) - (endTime mov)

    diff (x1, y1) (x2,y2) = (x2 - x1, y2 - y1) 
  in
    undefined

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
          dynText =<< (holdDyn "No Ticks Yet" $ switchDyn $ dETick)
        
        rec el "div" $ text "Counter as fold"
            numbs <- foldDyn (+) (0 :: Int)  (1 <$ evIncr)
            el "div" $ display numbs
            evIncr <- button "Increment"
        return ()
    }

toText :: (Show a) => a -> T.Text
toText =  T.pack . show 

svg :: (DomBuilder t m) => Int -> Int -> m a2 -> m a2
svg h w = elAttr "svg" ("width" =: (toText h) <> "height" =: (toText w)) 
 
circle :: (DomBuilder t m) => Int -> Int -> Int -> m a -> m a
circle x y r = elAttr "circle" (circleAttr x y r)

circleAttr :: Int -> Int -> Int -> Map.Map T.Text T.Text
circleAttr x y r = ("cx" =: (toText x) <> "cy" =: (toText y)<> "r" =: (toText r) <> "fill" =: "yellow")

followMovement :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) => m ()
followMovement = do
  rec 
    
    evIncr <- button "Next step"
  return ()

getFrameAt :: Movement -> Int -> Map.Map T.Text T.Text
getFrameAt mov pos =
    let
        allFrames = renderFrames
    in if length allFrames > pos 
        then Map.singleton k a init allFrames 
        else allFrames !! pos_


clickMoveCircle :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) => m ()
clickMoveCircle = do 
  rec  
    dynBool <- toggle False evClick
    let dynAttrs = attrs <$> dynBool
    svg 400 400 $ do
      elDynAttr "circle" dynAttrs blank
    evClick <- button "Move circle"  
  return ()

attrs :: Bool -> Map.Map T.Text T.Text
attrs True = circleAttr 100 100 100
attrs False = circleAttr 200 100 100
