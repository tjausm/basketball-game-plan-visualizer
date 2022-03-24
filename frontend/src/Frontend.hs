{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Common.Api
import Common.Route
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import qualified Data.Time as Time
import Language.Javascript.JSaddle (eval, liftJSM)
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core
import Language.Javascript.JSaddle

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
        el "h1" $ text "Welcome to Obelisk!"
        el "p" $ text $ T.pack commonStuff
        -- `prerender` and `prerender_` let you choose a widget to run on the server
        -- during prerendering and a different widget to run on the client with
        -- JavaScript. The following will generate a `blank` widget on the server and
        -- print "Hello, World!" on the client.
        prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

        el "div" $ do
          dETick <- prerender (return never) $ do
            new <- liftIO Time.getCurrentTime
            eTick <- tickLossy 3 new
            return $ T.pack . show . _tickInfo_n <$> eTick
          dynText =<< (holdDyn "No Ticks Yet" $ switchDyn $ dETick)
              
        -- el "div" $ do
        --   el "h2" $ text "A Simple Clock"
        --   now <- liftIO Time.getCurrentTime
        --   evTick <- tickLossy 1 now
        --   let evTime = (T.pack . show . _tickInfo_lastUTC) <$>  evTick
        --   dynText =<< holdDyn "No ticks yet" evTime
        {--elAttr "img" ("src" =: $(static "obelisk.jpg")) blank
        el "div" $ do
          exampleConfig <- getConfig "common/example"
          case exampleConfig of
            Nothing -> text "No config file found in config/common/example"
            Just s -> text $ T.decodeUtf8 s --}
        return () --}
    }

toText :: (Show a) => a -> T.Text
toText =  T.pack . show 

svg :: (DomBuilder t m) => Int -> Int -> m a2 -> m a2
svg h w = elAttr "svg" ("width" =: (toText h) <> "height" =: (toText w)) 
 

circle :: (DomBuilder t m) => Int -> Int -> Int -> m a -> m a
circle x y r = elAttr "circle" (circleAttr x y r)

circleAttr :: Int -> Int -> Int -> Map.Map T.Text T.Text
circleAttr x y r = ("cx" =: (toText x) <> "cy" =: (toText y)<> "r" =: (toText r) <> "fill" =: "yellow")

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

bodyElement :: (DomBuilder t m,
                  Control.Monad.IO.Class.MonadIO (Performable m),
                  Control.Monad.IO.Class.MonadIO m, TriggerEvent t m,
                  PerformEvent t m, PostBuild t m, MonadHold t m, MonadFix m) =>  m ()
bodyElement = do
  el "h2" $ text "A Simple Clock"
  now <- liftIO Time.getCurrentTime
  evTick <- tickLossy 1 now
  let evTime = (T.pack . show . _tickInfo_lastUTC) <$>  evTick
  dynText =<< holdDyn "No ticks yet" evTime