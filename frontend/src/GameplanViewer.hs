{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GameplanViewer (example, render, PlayerMovement (..), Player (..), Arrow (..)) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Float (float2Int, int2Float)
import Reflex.Dom.Core
import Prelude

-- Terminology
-- Animation = a set of 'movements' 
-- Movement = description of the movement over time of some object on the screen
-- Frame = the position of an object on the screen, described as a Dynamic (Map Text Text) that can be mapped on a svg


-------------------
-- Example usage --
-------------------
example :: (Monad m, Prerender t m) => m ()
example = do
  -- generate some movements
  let mkMovement a beginT endT = PlayerMovement {player = Player "One", arrow = a, startTime = if beginT > endT then endT else beginT, endTime = endT}
  let exampleAnimation =
        Animation
          { ball = [(Player "One", 0, 10)],
            players =
              [ mkMovement (Arrow "" (800, 1100) (800, 300)) 1 6,
                mkMovement (Arrow "" (200, 300) (400, 200)) 0 10,
                mkMovement (Arrow "" (200, 500) (500, 300)) 0 3,
                mkMovement (Arrow "" (400, 1200) (500, 700)) 0 5
              ]
          }

  -- pass a [Movement] here and the render function takes care of the rest
  prerender_ blank (GameplanViewer.render exampleAnimation)

------------------------
-- Rendering functions--
------------------------

-- Renders the complete basketball court, players and controls
render :: (MonadWidget t m) => Animation -> m ()
render animation = do
  importBootstrap
  importFontAwesome

  let playerMovements = players animation
  let ballMovement = ball animation

  rec -- initialise timer
      t0 <- liftIO Time.getCurrentTime
      eTick <- tickLossy (1 / fromRational (toRational framesPerSecond)) t0

      -- get the arrows and render the movements
      let arrows = map arrow playerMovements
      playerMovementFrames <- mapM (renderMovement eStart ePause eReset eTick . getPlayerFrameAt) playerMovements
      ballMovementFrames <- renderMovement eStart ePause eReset eTick $ getBallFrameAt animation

      -- draw the BBalCourt, arrows and the player animations
      rowCenter $ do
        colCenter 12 $ do
          drawSvgBody viewWidth viewHeight $ do
            mapM_ drawArrow arrows
            mapM_ (\m -> elDynSvgAttr "circle" m blank) playerMovementFrames
            elDynSvgAttr "circle" ballMovementFrames blank
          drawBBCourt viewWidth viewHeight

      -- draw the start, pause and reset button
      (eStart, ePause, eReset) <- -- m ()
      -- m ()
        rowCenter $ do
          colCenter 12 $ do
            s <- playButton
            p <- pauseButton
            r <- resetButton
            return (s, p, r)

  blank

-- creates 1 dynamic map linked to the ticks and start& reset button
renderMovement :: (DomBuilder t m, MonadWidget t m) =>  Event t () -> Event t () -> Event t () -> Event t TickInfo -> RenderFrame -> m (Dynamic t (Map.Map T.Text T.Text))
renderMovement eStart ePause eReset eTick getFrame = do
  beStartStop <- hold never . leftmost $ [((1 +) <$ eTick) <$ eStart, ((0 +) <$ eTick) <$ ePause, (const 0 <$ eTick) <$ eReset]
  let eSwitch = switch beStartStop
  fmap getFrame <$> foldDyn ($) 0 eSwitch


----------------------------
-- UI & bootstrap elements--
----------------------------
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

scalePoint :: Point -> Point
scalePoint (x, y) = let scale' = int2Float . scale in (scale' x, scale' y)

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

framesPerSecond :: Int
framesPerSecond = 30

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
  let (p1, p2) = (start a, end a)
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

ballAttr :: Point -> Map.Map T.Text  T.Text
ballAttr = circleAttr bbalColor bbalWidth

circleAttr :: Color -> Int -> Point -> Map.Map T.Text T.Text
circleAttr color r (x, y) = "cx" =: toText x <> "cy" =: toText y <> "r" =: toText r <> "fill" =: color

-------------------------
-- Movement calculations-
-------------------------
type Seconds = Int

-- a function that given the framesPerSecond and 'tick' should return the right frame
type RenderFrame =  Int -> Map.Map T.Text T.Text

type Point = (Float, Float)

newtype Player = Player T.Text
  deriving (Show, Eq, Ord)

data Arrow = Arrow T.Text Point Point
  deriving (Show)

data PlayerMovement = PlayerMovement
  { player :: Player,
    arrow :: Arrow,
    startTime :: Seconds,
    endTime :: Seconds
  }
  deriving (Show)

type BallMovement = [(Player, Seconds, Seconds)]

data Animation = Animation
  { ball :: BallMovement,
    players :: [PlayerMovement]
  }

start :: Arrow -> Point
start (Arrow _ s _) = scalePoint s

end :: Arrow -> Point
end (Arrow _ _ e) = scalePoint e

-- Difference between 2 points
diff :: Point -> Point -> Point
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- divide points x and y coordinate by a float
divPoint :: Point -> Int -> Point
divPoint (x, y) n = let n' = int2Float n in (x / n', y / n')

-- return stepsize given the n of frames a movement takes
stepSize :: Point -> Point -> Int -> Point
stepSize startP endP travTime = diff (startP) (endP) `divPoint` travTime

-- calculates a list of position on x and y axes of circle at each tick
-- from it's starting frame to it's end frame with the given frames per second
computePlayerPositions :: PlayerMovement -> [Point]
computePlayerPositions mov =
  let stationaryFrames = replicate (startTime mov * framesPerSecond) (start $ arrow mov)
      nOfFrames = ((endTime mov - startTime mov) * framesPerSecond) - 2 -- leave 2 frames for begin and end frame
      (xStart, yStart) = start (arrow mov)
      (xStep, yStep) = stepSize (start $ arrow mov) (end $ arrow mov) nOfFrames
   in stationaryFrames ++ [(xStart + i * xStep, yStart + yStep * i) | i <- [0 .. (int2Float nOfFrames)]] ++ [end (arrow mov)]

-- computes 1 throw, catch and subsequent movement of ball with catching player
computeThrowCatchPositions :: Point -> PlayerMovement -> Seconds -> Seconds -> Seconds -> [Point] 
computeThrowCatchPositions startP@(xStart, yStart) toMov startThrow startMov endMov =
  let 
    toFr m = framesPerSecond * m
    toPositions = drop (toFr startMov) $ take (toFr endMov) $ computePlayerPositions toMov -- take the frames the ball moves with this player

    throwFrames = max 0 ((startMov- startThrow) * framesPerSecond - 2) 
    (xStep, yStep) = stepSize startP (head toPositions) throwFrames
  in startP : [(xStart + i * xStep, yStart + yStep * i) | i <- [0 .. (int2Float throwFrames)]] ++ toPositions


transform :: BallMovement -> [(PlayerMovement, Seconds, Seconds, Seconds)]
transform ballMov = undefined 

folt :: (PlayerMovement, Seconds, Seconds, Seconds) -> (Point, PlayerMovement, Seconds, Seconds, Seconds) -> (Point, PlayerMovement, Seconds, Seconds, Seconds) 
folt = undefined 


computeBallPositions :: Animation  -> [Point]
computeBallPositions animation  =
  let
    playersWithBall = map (\(a,_,_) -> a) (ball animation)
    movementsWithBall = filter (\mov -> player mov `elem` playersWithBall) (players animation)
    playerAndPositions = Map.fromList $ map (\mov -> (player mov,  mov)) movementsWithBall

    otherPositions = foldr

    addOffset a (x,y) = (x + a, y)

    -- always let ball start with the first player that receives it
    endOfFirstMovement = (\(_,_,a) -> a) $ head (ball animation)
    firstBallPositions = take (framesPerSecond * endOfFirstMovement) $ computePlayerPositions $ head movementsWithBall


  in map (addOffset 15) (firstBallPositions)

-- Calculate attributes of circle representing player at given frame
getFrameAt :: [Point]-> (Point -> Map.Map T.Text T.Text) ->  Int -> Map.Map T.Text T.Text
getFrameAt allPositions drawP frameN =
  let currPosition = if length allPositions > frameN then allPositions !! frameN else last allPositions
   in drawP currPosition

getPlayerFrameAt :: PlayerMovement -> Int -> Map.Map T.Text T.Text
getPlayerFrameAt mov = getFrameAt (computePlayerPositions mov) playerAttr


getBallFrameAt :: Animation -> Int ->  Map.Map T.Text T.Text
getBallFrameAt animation = getFrameAt (computeBallPositions animation) ballAttr
