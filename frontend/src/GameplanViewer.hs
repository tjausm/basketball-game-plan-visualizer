{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GameplanViewer (example, render, PlayerMovement (..), Player (..), Arrow (..)) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Time as Time
import Reflex.Dom.Core
import Prelude
import GHC.Float (int2Float)

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
  let mkMovement player a beginT endT = PlayerMovement {player = Player player, arrow = a, startTime = if beginT > endT then endT else beginT, endTime = endT}
  let exampleAnimation =
        Animation
          { ball = [(Player "Four", 0, 1), (Player "Two", 2, 3), (Player "Three", 4,6), (Player "One", 8, 10)],
            players =
              [ mkMovement "One" (Arrow "" (800, 1100) (800, 300)) 1 6,
                mkMovement "Two" (Arrow "" (200, 300) (400, 200)) 0 10,
                mkMovement "Three" (Arrow "" (200, 500) (500, 300)) 0 8,
                mkMovement "Four" (Arrow "" (400, 1200) (500, 700)) 0 6
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

-- creates 1 dynamic map linked to the ticks and start & reset button
renderMovement :: (DomBuilder t m, MonadWidget t m) => Event t () -> Event t () -> Event t () -> Event t TickInfo -> RenderFrame -> m (Dynamic t (M.Map T.Text T.Text))
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

type Height = Float

type Width = Float

type Color = T.Text

-- Constant values of drawables & animations
scale :: (Fractional a) => a -> a
scale i = let conScale = 0.65 in i * conScale

scalePoint :: Point -> Point
scalePoint (x, y) = (scale x, scale y)

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
  Dynamic t (M.Map T.Text T.Text) ->
  m a ->
  m (Element EventResult (DomBuilderSpace m) t, a)
elDynSvgAttr = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

-- added position absolute to draw svg body over images
drawSvgBody :: (DomBuilder t m, PostBuild t m) => Height -> Width -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
drawSvgBody w h = elDynSvgAttr "svg" $ constDyn $ "height" =: toText h <> "width" =: toText w <> "style" =: "position:absolute"

drawLine :: (DomBuilder t m, PostBuild t m) => Width -> Point -> Point -> M.Map T.Text T.Text -> m (Element EventResult (DomBuilderSpace m) t, ())
drawLine strokeWidth (x1, y1) (x2, y2) otherAttrs = elDynSvgAttr "line" (constDyn $ "x1" =: toText x1 <> "y1" =: toText y1 <> "x2" =: toText x2 <> "y2" =: toText y2 <> "stroke-width" =: toText strokeWidth <> "style" =: "fill:black;stroke:black" <> otherAttrs) blank

drawArrow :: (DomBuilder t m, PostBuild t m) => Arrow -> m (Element EventResult (DomBuilderSpace m) t, ())
drawArrow a = do
  let (p1, p2) = (start a, end a)
  elDynSvgAttr "marker" (constDyn $ "id" =: "triangle" <> "viewBox" =: "0 0 10 10" <> "markerUnits" =: "strokeWidth" <> "markerWidth" =: "4" <> "markerHeight" =: "3" <> "refX" =: "0" <> "refY" =: "5" <> "orient" =: "auto") $ do
    elDynSvgAttr "path" (constDyn $ "d" =: "M 0 0 L 10 5 L 0 10 z") blank
  elAttr "polygon" ("points" =: "0 0, 10 3.5, 0 7") blank
  drawLine arrowWidth p1 p2 ("marker-end" =: "url(#triangle)")

drawBBCourt :: (DomBuilder t m, Num a, Show a) => a -> a -> m ()
drawBBCourt w h = elAttr "img" ("src" =: "/static/1shk1nm322652sism9gqw8sbz34dangql5fs9f51ibnclqypjbb8-bbcourt.jpg" <> "width" =: toText w <> "Height" =: toText h) blank

toText :: (Show a) => a -> T.Text
toText = T.pack . show

playerAttr :: Point -> M.Map T.Text T.Text
playerAttr = circleAttr playerColor playerWidth

ballAttr :: Point -> M.Map T.Text T.Text
ballAttr = circleAttr bbalColor bbalWidth

circleAttr :: (Num a, Show a) => Color -> a -> Point -> M.Map T.Text T.Text
circleAttr color r (x, y) = "cx" =: toText x <> "cy" =: toText y <> "r" =: toText r <> "fill" =: color

-------------------------
-- Movement calculations-
-------------------------
type Seconds = Int

-- a function that given the framesPerSecond and 'tick' should return the right frame
type RenderFrame = Int -> M.Map T.Text T.Text

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
   in 
      stationaryFrames ++ [(xStart + i * xStep, yStart + yStep * i) | i <- [0 .. (int2Float nOfFrames)]] ++ repeat (end (arrow mov))

-- computes 1 throw, catch and subsequent movement of the ball 
computeThrowCatchMovePositions :: (Point, PlayerMovement, Seconds, Seconds, Seconds) -> [Point]
computeThrowCatchMovePositions (startP@(xStart, yStart), toMov, startThrow, startMov, endMov) =
  let toFr m = framesPerSecond * m
      toPositions = drop (toFr startMov) $ take (toFr endMov) $ computePlayerPositions toMov -- take the frames the ball moves with this player
      throwFrames = max 0 ((startMov - startThrow) * framesPerSecond - 1)
      (xStep, yStep) = stepSize startP (head toPositions) throwFrames
   in [(xStart + i * xStep, yStart + yStep * i) | i <- [0 .. (int2Float throwFrames)]] ++ toPositions

-- transform ballMovement to startThrow, startMov, endMov (adding throw time of last player or throwtime = 0 for first player)
addStartThrow :: BallMovement -> [(Player, Seconds, Seconds, Seconds)]
addStartThrow ballMov =
  let f = (\acc (p, b, c) -> acc ++ [(p, (\(_, _, _, a) -> a) (last acc), b, c)])
      initial = (\(p, a, b) -> (p, 0, a, b)) $ head ballMov
   in foldl f [initial] (tail ballMov)

computeBallPositions :: Animation -> [Point]
computeBallPositions Animation {ball = ballMov, players = playerMovs} =
  let playerAndPositions = M.fromList $ map (\mov -> (player mov, mov)) playerMovs

      mapMaybeFstOf4 f (t, b, c, d) = case f t of
        Just a -> Just (a, b, c, d)
        Nothing -> Nothing
      addFst a (b, c, d, e) = (a, b, c, d, e)

      playerMoveAndTimes = mapMaybe (mapMaybeFstOf4 (`M.lookup` playerAndPositions)) (addStartThrow ballMov)
      initialBallP = (viewWidth / 2, viewHeight / 2)
      f acc a = let lastPoint = if null acc then initialBallP else last acc in acc ++ computeThrowCatchMovePositions (addFst lastPoint a) 
      allPositions = foldl f [] playerMoveAndTimes

      addOffset a (x, y) = (x + a, y)
   in map (addOffset 15) (allPositions ++ repeat (last allPositions))

-- Takes an infinite list of positions and transforms it to a frame (the last movement frame is repeated for infinity)
getFrameAt :: [Point] -> (Point -> M.Map T.Text T.Text) -> Int -> M.Map T.Text T.Text
getFrameAt allPositions drawP frameN = drawP (allPositions !! frameN)

getPlayerFrameAt :: PlayerMovement -> Int -> M.Map T.Text T.Text
getPlayerFrameAt mov = getFrameAt (computePlayerPositions mov) playerAttr

getBallFrameAt :: Animation -> Int -> M.Map T.Text T.Text
getBallFrameAt animation = getFrameAt (computeBallPositions animation) ballAttr
