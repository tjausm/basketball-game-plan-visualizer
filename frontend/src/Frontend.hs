{-# LANGUAGE OverloadedStrings #-}

module Frontend where

import Common.Route ( FrontendRoute )
import Obelisk.Frontend ( Frontend(..) )
import Obelisk.Route ( R )
import Reflex.Dom.Core ( prerender_, blank, el, text )
import qualified GameplanViewer


frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "View gameplan",
      _frontend_body = do

        -- pass a [Movement] here and the render function takes care of the rest
        prerender_ blank (GameplanViewer.render GameplanViewer.exampleAnimation)
        
        blank
    }

