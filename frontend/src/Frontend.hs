{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


menu :: DomBuilder t m => m ()
menu = do
  el "div" $ do
    el "ul" $ do
      el "li" (text "Item 1")
      el "li" (text "Item 2")
      el "li" (text "Item 3")
      el "li" (text "Item 4")
      el "li" (text "Item 5")
      el "li" (text "Item 6")

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Erro 404 - Página infelizmente encontrada"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      menu
      el "h1" $ text "Passei? Passei? Passei... Finalmente!"
      el "p" $ text $ "Minha cara quando eu ver minha nota em Haskell: "
      
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"meme.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ "Se você chegou até aqui, favor se retirar... De nada!"
      return ()
  }
