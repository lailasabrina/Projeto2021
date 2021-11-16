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
import Text.Read (readMaybe)
import Data.Maybe

import Reflex.Dom.Core

import Common.Api
import Common.Route

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5
   
clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)
    
menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- elAttr "ul" ("class" =: "menu") $ do
        p1 <- clickLi Pagina1 "Lista"
        p2 <- clickLi Pagina2 "Palavra reversa"
        p3 <- clickLi Pagina3 "Somar"
        p4 <- clickLi Pagina4 "Página 4"
        p5 <- clickLi Pagina5 "Página 5"
        return (leftmost [p1,p2,p3,p4,p5])
    holdDyn Pagina0 evs    
    
currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => Pagina -> m ()
currPag p = 
    case p of
         Pagina0 -> blank
         Pagina1 -> lista
         Pagina2 -> bttnEvt
         Pagina3 -> sumEvt
         Pagina4 -> el "h1" $ text "Esta é a página 4"
         Pagina5 -> el "h1" $ text "Esta é a página 5"
         
mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => m ()
mainPag = do
    pag <- el "div" menuLi
    dyn_ $ currPag <$> pag

numberInput :: DomBuilder t m => m (Dynamic t Double)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n

caixaSoma :: (DomBuilder t m, PostBuild t m) => m ()
caixaSoma = do
    n1 <- numberInput -- m (Dynamic t Double)
    text " "
    n2 <- numberInput -- m (Dynamic t Double)
    dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))
   
caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = do
    t1 <- inputElement def -- m (Dynamic Text)
    t2 <- inputElement def -- m (Dynamic Text)
    text " "
    dynText (zipDynWith (<>) (_inputElement_value t1) (_inputElement_value t2))

revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))
   
buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Event t T.Text)
buttonClick = do
    t <- inputElement def
    (e,_) <- el' "button" (text "OK")
    return $ attachPromptlyDynWith const 
                                   (fmap revText (_inputElement_value t)) 
                                   (domEvent Click e)            

bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
bttnEvt = do
    el "h1" $ text "Palavra reversa"
    evt <- buttonClick
    hl <-  holdDyn "" evt -- Event -> Dynamic 
    el "div" (dynText hl)
    

sumButton :: (DomBuilder t m, PostBuild t m, MonadHold t m) 
          => m (Event t Double)
sumButton = do
    n1 <- numberInput
    text " "
    n2 <- numberInput
    text " "
    (e,_) <- el' "button" (text "OK")
    let dynDouble = zipDynWith (+) n1  n2
    return $ attachPromptlyDynWith const    
                                   dynDouble 
                                   (domEvent Click e)

sumEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
sumEvt = do
    el "h1" $ text "Soma de dois números"
    evt <- sumButton
    s <- holdDyn 0 evt 
    el "div" (dynText $ fmap (T.pack . show) s)  


lista :: DomBuilder t m => m ()
lista = do
  el "h1" $ text "Listinha básica"
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
      mainPag

      el "p" $ text $ "Minha cara quando eu ver minha nota em Haskell: "
          
      elAttr "img" ("src" =: static @"meme.jpg") blank

  }
