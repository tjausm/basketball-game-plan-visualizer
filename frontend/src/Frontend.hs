{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Common.Api
import Common.Route
import Control.Monad
import Control.Monad.Fix
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import Language.Javascript.JSaddle (eval, liftJSM)
import Obelisk.Configs
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
        el "h1" $ text "Welcome to Obelisk!"
        el "p" $ text $ T.pack commonStuff
        -- `prerender` and `prerender_` let you choose a widget to run on the server
        -- during prerendering and a different widget to run on the client with
        -- JavaScript. The following will generate a `blank` widget on the server and
        -- print "Hello, World!" on the client.
        prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
        rec  
          dynBool <- toggle False evClick
          let dynAttrs = attrs <$> dynBool
          svg 400 400 $ do
            elDynAttr "circle" dynAttrs blank
          evClick <- button "Move circle"  
        return ()
        

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

circleAttr :: Int -> Int -> Int -> M.Map T.Text T.Text
circleAttr x y r = ("cx" =: (toText x) <> "cy" =: (toText y)<> "r" =: (toText r) <> "fill" =: "yellow")

dynCircle :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) => m ()
dynCircle = do 
  rec  
    dynBool <- toggle False evClick
    let dynAttrs = attrs <$> dynBool
    elDynAttr "circle" dynAttrs blank
    evClick <- button "Move circle"  
  return ()

attrs :: Bool -> M.Map T.Text T.Text
attrs True = circleAttr 100 100 100
attrs False = circleAttr 200 100 100