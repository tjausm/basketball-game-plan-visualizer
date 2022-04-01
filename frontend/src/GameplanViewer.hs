{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GameplanViewer (render, mkMovement, Arrow(One, Two, Three, Four), exampleAnimation) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Float (float2Int, int2Float)
import Reflex.Dom.Core

-- Renders the complete basketball court, players and controls
render :: (MonadWidget t m) => [Movement] -> m ()
render movements = do
  importBootstrap
  importFontAwesome

  rec -- initialise timer
      t0 <- liftIO Time.getCurrentTime
      eTick <- tickLossy (1 / toNDT fps) t0

      -- get the arrows and render the movements
      let arrows = map arrow movements
      renderedMovements <- mapM (renderMovements fps eStart ePause eReset eTick) movements

      -- draw the BBalCourt, arrows and the player animations
      rowCenter $ do
        colCenter 12 $ do
          drawSvgBody viewWidth viewHeight $ do
            mapM_ drawArrow arrows
            mapM_ (\m -> elDynSvgAttr "circle" m blank) renderedMovements
          drawBBCourt viewWidth viewHeight

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
renderMovements framesPerSecond eStart ePause eReset eTick mov = do
  beStartStop <- hold never . leftmost $ [((1 +) <$ eTick) <$ eStart, ((0 +) <$ eTick) <$ ePause, (const 0 <$ eTick) <$ eReset]
  let eSwitch = switch beStartStop
  fmap (getFrameAt mov framesPerSecond) <$> foldDyn ($) 0 eSwitch

toNDT :: (Real a) => a -> Time.NominalDiffTime
toNDT = fromRational . toRational

----------------
-- UI & bootstrap elements--
----------------
importBootstrap :: (DomBuilder t m) => m ()
importBootstrap = elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" <> "rel" =: "stylesheet") blank

importFontAwesome :: (DomBuilder t m) => m ()
importFontAwesome = elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css" <> "rel" =: "stylesheet") blank

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

-- Constant values of drawables & animations
scale :: Float -> Int
scale i = let conScale = 0.65 in float2Int $ i * conScale

viewWidth :: Width
viewWidth = scale 1000

viewHeight :: Height
viewHeight = scale 1400

playerWidth :: Width
playerWidth = scale 30

bbalWidth :: Width
bbalWidth = scale 20

playerColor :: Color
playerColor = "BurlyWood"

bbalColor :: Color
bbalColor = "Brown"

arrowWidth :: Width
arrowWidth = scale 7

fps :: Int
fps = 30

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
drawSvgBody w h = elDynSvgAttr "svg" $ constDyn $ "height" =: toText h <> "width" =: toText w <> "style" =: "position:absolute"

drawLine :: (DomBuilder t m, PostBuild t m) => Width -> Point -> Point -> Map.Map T.Text T.Text -> m (Element EventResult (DomBuilderSpace m) t, ())
drawLine strokeWidth (x1, y1) (x2, y2) otherAttrs = elDynSvgAttr "line" (constDyn $ "x1" =: toText x1 <> "y1" =: toText y1 <> "x2" =: toText x2 <> "y2" =: toText y2 <> "stroke-width" =: toText strokeWidth <> "style" =: "fill:black;stroke:black" <> otherAttrs) blank

drawArrow :: (DomBuilder t m, PostBuild t m) => Arrow -> m (Element EventResult (DomBuilderSpace m) t, ())
drawArrow a = do
  let (p1, p2) = getPoints a
  elDynSvgAttr "marker" (constDyn $ "id" =: "triangle" <> "viewBox" =: "0 0 10 10" <> "markerUnits" =: "strokeWidth" <> "markerWidth" =: "4" <> "markerHeight" =: "3" <> "refX" =: "0" <> "refY" =: "5" <> "orient" =: "auto") $ do
    elDynSvgAttr "path" (constDyn $ "d" =: "M 0 0 L 10 5 L 0 10 z") blank
  elAttr "polygon" ("points" =: "0 0, 10 3.5, 0 7") blank
  drawLine arrowWidth p1 p2 ("marker-end" =: "url(#triangle)")

drawBBCourt :: (DomBuilder t m) => Int -> Int -> m ()
drawBBCourt w h = elAttr "img" ("src" =: "/static/1shk1nm322652sism9gqw8sbz34dangql5fs9f51ibnclqypjbb8-bbcourt.jpg" <> "width" =: toText w <> "Height" =: toText h) blank

toText :: (Show a) => a -> T.Text
toText = T.pack . show

playerAttr :: Point -> Map.Map T.Text T.Text
playerAttr = circleAttr playerColor playerWidth

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
getPoints a =
  let scale' = int2Float . scale
      sp ((p1, p2), (p3, p4)) = ((scale' p1, scale' p2), (scale' p3, scale' p4))
   in case a of
        One -> sp ((800, 1100), (800, 300))
        Two -> sp ((200, 300), (400, 200))
        Three -> sp ((200, 500), (500, 300))
        Four -> sp ((400, 1200), (500, 700))

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

mkMovement :: Arrow -> Int -> Int-> Movement
mkMovement a beginT endT = Movement {player = Player "One", arrow = a, startTime = if beginT > endT then endT else beginT, endTime = endT}

exampleAnimation :: [Movement]
exampleAnimation = [mkMovement One 1 6, mkMovement Two 0 10, mkMovement Three 0 3, mkMovement Four 0 5]

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