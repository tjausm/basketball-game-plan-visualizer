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
import GHC.Float (int2Float)
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

        -- import Bootstrap and fontawesome from cdn
        elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" <> "rel" =: "stylesheet") blank
        elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css" <> "rel" =: "stylesheet") blank,
      _frontend_body = do
        prerender_ blank (renderView [mov1, mov2, mov3, mov5])

        blank
    }

-- Renders the complete basketball court, players and controls
renderView :: (MonadWidget t m) => [Movement] -> m ()
renderView movements = do
  let fps = 30 -- hardcode frames per second to 30
  rec -- initialise timer
      t0 <- liftIO Time.getCurrentTime
      eTick <- tickLossy (1 / toNDT fps) t0

      -- get the arrows and render the movements
      let arrows = map arrow movements
      renderedMovements <- mapM (renderMovements fps eStart ePause eReset eTick) movements

      -- draw the BBalCourt, arrows and the player animations
      rowCenter $ do
        colCenter 12 $ do
          drawSvgBody 1400 1000 $ do
            mapM_ drawArrow arrows
            mapM_ (\m -> elDynSvgAttr "circle" m blank) renderedMovements
          elAttr "img" ("src" =: "/static/1shk1nm322652sism9gqw8sbz34dangql5fs9f51ibnclqypjbb8-bbcourt.jpg") blank


      -- draw the start, pause and reset button
      (eStart, ePause, eReset) <-
        rowCenter $ do
          colCenter 12 $ do
            s <- playButton
            p <- pauseButton
            r <- resetButton
            return (s, p, r)

  blank

-- creates 1 dynamic map linked to the ticks and start & reset button
renderMovements :: (DomBuilder t m, MonadWidget t m) => Int -> Event t () -> Event t () -> Event t () -> Event t TickInfo -> Movement -> m (Dynamic t (Map.Map T.Text T.Text))
renderMovements fps eStart ePause eReset eTick mov = do
  beStartStop <- hold never . leftmost $ [((1 +) <$ eTick) <$ eStart, ((0 +) <$ eTick) <$ ePause, (const 0 <$ eTick) <$ eReset]
  let eSwitch = switch beStartStop
  fmap (getFrameAt mov fps) <$> foldDyn ($) 0 eSwitch

toNDT :: (Real a) => a -> Time.NominalDiffTime
toNDT = fromRational . toRational

----------------
-- UI & bootstrap elements--
----------------
buttonWithIcon :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
buttonWithIcon c icon = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) (elClass "i" icon blank)
  return $ domEvent Click e

playButton :: DomBuilder t m => m (Event t ())
playButton = buttonWithIcon "btn btn-lg btn-primary me-1" "fa-solid fa-play"

pauseButton :: DomBuilder t m => m (Event t ())
pauseButton = buttonWithIcon "btn btn-lg btn-secondary me-1" "fa-solid fa-pause"

resetButton :: DomBuilder t m => m (Event t ())
resetButton = buttonWithIcon "btn btn-lg btn-danger" "fa-solid fa-stop"

rowCenter :: DomBuilder t m => m a -> m a
rowCenter = elClass "div" "row justify-content-center"

colCenter :: DomBuilder t m => Int -> m a -> m a
colCenter size = elAttr "div" $ "class" =: T.pack ("col-" ++ show size) <> "align" =: "center"

-----------------
-- SVG elements--
-----------------

type Height = Int
type Width = Int
type Color = T.Text

-- using elDynAttrNS' to build svg in prerender (https://github.com/obsidiansystems/obelisk/issues/828)
elDynSvgAttr ::
  (DomBuilder t m, PostBuild t m) =>
  T.Text ->
  Dynamic t (Map.Map T.Text T.Text) ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
elDynSvgAttr = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

-- added position absolute to draw svg body over images
drawSvgBody :: (DomBuilder t m, PostBuild t m) => Height -> Width -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
drawSvgBody h w = elDynSvgAttr "svg" $ constDyn $ "height" =: toText h <> "width" =: toText w <> "style" =: "position:absolute"

drawRect :: (DomBuilder t m, PostBuild t m) => Point -> Height -> Width -> T.Text -> m (Element EventResult (DomBuilderSpace m) t, ())
drawRect (x, y) h w style = elDynSvgAttr "rect" (constDyn $ "x" =: toText x <> "y" =: toText y <> "height" =: toText h <> "width" =: toText w <> "style" =: style) blank

drawLine :: (DomBuilder t m, PostBuild t m) => Width -> Point -> Point -> Map.Map T.Text T.Text -> m (Element EventResult (DomBuilderSpace m) t, ())
drawLine strokeWidth (x1, y1) (x2, y2) otherAttrs = elDynSvgAttr "line" (constDyn $ "x1" =: toText x1 <> "y1" =: toText y1 <> "x2" =: toText x2 <> "y2" =: toText y2 <> "stroke-width" =: toText strokeWidth <> "style" =: "fill:black;stroke:black" <> otherAttrs) blank

drawArrow :: (DomBuilder t m, PostBuild t m) => Arrow -> m (Element EventResult (DomBuilderSpace m) t, ())
drawArrow a = do
  let (p1, p2) = getPoints a
  elDynSvgAttr "marker" (constDyn $ "id" =: "triangle" <> "viewBox" =: "0 0 10 10" <> "markerUnits" =: "strokeWidth" <> "markerWidth" =: "4" <> "markerHeight" =: "3" <> "refX" =: "0" <> "refY" =: "5" <> "orient" =: "auto") $ do
    elDynSvgAttr "path" (constDyn $ "d" =: "M 0 0 L 10 5 L 0 10 z") blank
  elAttr "polygon" ("points" =: "0 0, 10 3.5, 0 7") blank
  drawLine 7 p1 p2 ("marker-end" =: "url(#triangle)")

toText :: (Show a) => a -> T.Text
toText = T.pack . show

playerAttr :: Point -> Map.Map T.Text T.Text
playerAttr p = circleAttr "BurlyWood" 30 p

circleAttr :: Color -> Int -> Point -> Map.Map T.Text T.Text
circleAttr color r (x, y) = "cx" =: toText x <> "cy" =: toText y <> "r" =: toText r <> "fill" =: color

-------------------------
-- Movement calculations-
-------------------------
type Seconds = Int

type Point = (Float, Float)

newtype Player = Player T.Text
  deriving (Show)

data Arrow = One | Two | Three | Four
  deriving (Show)

-- hard code the coordinates of begin and end point of the arrows here
getPoints :: Arrow -> (Point, Point)
getPoints One = ((800, 1100), (800, 300))
getPoints Two = ((200, 300), (400, 200))
getPoints Three = ((200, 500), (500, 300))
getPoints Four = ((400, 1200), (500, 700))

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
mov1 = mkMovement One 1 6

mov2 :: Movement
mov2 = mkMovement Two 0 10

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
   in playerAttr currFrame