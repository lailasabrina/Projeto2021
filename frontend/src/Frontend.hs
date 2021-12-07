{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Control.Monad.Fix
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Data.Map (Map)
import Reflex.Dom.Core
import Text.Read
import Data.Maybe
import Common.Api
import Common.Route
import Data.Aeson

-------------------------------- BACKEND --------------------------------

getPath :: R BackendRoute -> T.Text
getPath r = renderBackendRoute checFullREnc r

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def

getFilmReq :: Int -> XhrRequest ()
getFilmReq pid = xhrRequest "GET" (getPath (BackendRoute_Buscar :/ pid)) def

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

deleteReq :: Int -> XhrRequest ()
deleteReq pid = xhrRequest "DELETE" (getPath (BackendRoute_Apagar :/ pid)) def


reqFilm :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
reqFilm = do
    el "h1" (text "Adicionar Filmes")
    el "p" (text "")
    el "p" (text "Nome: ")
    nome <- inputElement def
    el "p" (text "Genêro: ")
    genero <- inputElement def
    el "p" (text "Ano de Lançamento: ")
    ano <- dateInput
    let film = fmap (\((a,n),g) -> Filme 0 n g a) (zipDyn (zipDyn ano (_inputElement_value nome))(_inputElement_value genero))
    (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-primary") (text "Inserir")
    let click = domEvent Click submitBtn
    let filmEvt = tag (current film) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Filme :/ ()) <$> filmEvt))
    return () 

req :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
req = do
    inputEl <- inputElement def
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let nm = tag (current $ _inputElement_value inputEl) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> nm))
    return () 

data Acao = Perfil Int | Editar Int | Apagar Int

tabRegistro :: (PostBuild t m, DomBuilder t m) => Dynamic t Filme-> m (Event t Acao)
tabRegistro pr = do 
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . filmeId) pr)
        el "td" (dynText $ fmap (T.pack . show . filmeNome) pr)
        el "td" (dynText $ fmap (T.pack . show . filmeGenero) pr)
        el "td" (dynText $ fmap (T.pack . show . filmeAno) pr)  
        evt <- fmap (fmap (const Perfil)) (button "perfil")
        evt2 <- fmap (fmap (const Editar)) (button "editar")
        evt3 <- fmap (fmap (const Apagar)) (button "apagar")
        return (attachPromptlyDynWith (flip ($)) (fmap filmeId pr) (leftmost [evt,evt2,evt3]))

reqTabela :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabela = Workflow $ do
    btn <- button "Listar"
    films :: Dynamic t (Event t (Maybe [Filme])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> btn))
    evt <- return (fmap (fromMaybe []) $ switchDyn films)
    dynP <- foldDyn (++) [] evt
    tb <- el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "Gênero")
                el "th" (text "Ano de Lançamento")
                el "th" (text "")
                el "th" (text "")
                el "th" (text "")
        
        el "tbody" $ do
             simpleList dynP tabRegistro
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
        escolherPag (Perfil pid) = pagPerfil pid
        escolherPag (Editar pid) = editarPerfil pid
        escolherPag (Apagar pid) = delPerfil pid
      
pagPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil pid = Workflow $ do
    btn <- button "mostrar"
    film :: Dynamic t (Event t (Maybe Filme)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getFilmReq pid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn film)
    dynP <- return ((fromMaybe (Filme 0 "" "" 0)) <$> mdyn)
    el "div" $ do
        el "div" (dynText $ fmap filmeNome dynP)
        el "div" (dynText $ fmap (T.pack . show . filmeGenero) dynP)
        el "div" (dynText $ fmap (T.pack . show . filmeAno) dynP)
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show pid), reqTabela <$ ret) 

delPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
delPerfil  pid = Workflow $ do
    el "div" $ do 
        el "h1" (text "Apagar filme")
        el "p" (text "Deseja realmente apagar esse filme e todos seus dados?")        
    (btnDel,film) <- elAttr' "button" ("class"=: "btn btn-success") (text "Deletar")

    let delEvt = domEvent Click btnDel
    film :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_Apagar :/ pid) 
            <$> delEvt))    

    return ("" <> "", reqTabela <$ delEvt)
    

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) =>
               Event t a -> m (Dynamic t a)
numberInputDyn p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)


editarPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil pid = Workflow $ do
    btn <- button "mostrar"
    film :: Dynamic t (Event t (Maybe Filme)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync
           (const (getFilmReq pid) <$> btn))
    mdyn <- return (switchDyn film)
    dynE <- return ((fromMaybe (Filme 0 "" "" 0)) <$> mdyn)
    el "p" (text "Nome: ")
    nome <- inputElement $ 
         def & inputElementConfig_setValue .~ (fmap filmeNome dynE)
    el "p" (text "Gênero: ")
    genero <- inputElement $ 
         def & inputElementConfig_setValue .~ (fmap filmeGenero dynE)
    el "p" (text "Ano de Lançamento: ")
    ano <- numberInputDyn (fmap filmeAno dynE)
    
    let film = fmap (\((a,n),g) -> Filme 0 n g a) (zipDyn (zipDyn ano (_inputElement_value nome))(_inputElement_value genero))
        
    submitBtn <- button "Editar"
    let filmEvt = tag (current film) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_Editar :/ pid) 
            <$> filmEvt)) 
    return ("Perfil: " <> (T.pack $ show pid), reqTabela <$ submitBtn)  
                 
reqLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
    r <- workflow reqTabela
    el "div" (dynText r)
----------------------------------------------------------------------

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 
   
clickLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return $ (const p) <$> (domEvent Click ev)
    
menuLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- elAttr "ul" ("class" =: "menu") $ do
        p1 <- clickLi Pagina1 "Home"
        p2 <- clickLi Pagina2 "Inserir Filmes"
        p3 <- clickLi Pagina3 "Listar Filmes"
        p4 <- clickLi Pagina4 "Sobre"
        return (leftmost [p1,p2,p3,p4])
    holdDyn Pagina0 evs    
    
currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = 
    case p of
         Pagina0 -> blank
         Pagina1 -> home
         Pagina2 -> reqFilm
         Pagina3 -> reqLista
         Pagina4 -> sobre
         
mainPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => m ()
mainPag = do
    pag <- el "div" menuLi
    dyn_ $ currPag <$> pag

numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig 
        . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)

dateInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
dateInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "1890"
        & inputElementConfig_elementConfig 
        . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)

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

home :: (DomBuilder t m, PostBuild t m) => m ()
home = do
    el "h1" $ text "Bem - Vindo (a)"
    el "p" $ blank
    el "div" $ do 
        el "p" (text "Trabalho desenvolvido para a matéria de Tópicos Especiais em Sistemas para a Internet III, do curso de Sistemas para Internet, objetivando a obtenção de nota para compor a média.")
        el "p" $ blank
        el "p" $ blank
        el "p" (text "Alunos: ")
        el "p" (text "Danilo dos Santos Gomes")
        el "p" (text "Laila Sabrina Alves Silva de Lima")
        el "p" (text "Marcos Vinicius Sousa do Rosario")
        el "p" $ blank
        el "p" $ blank
        el "p" (text "Mais informações na página sobre.")

sobre :: (DomBuilder t m, PostBuild t m) => m ()
sobre = do
    el "h1" $ text "Sobre o Projeto"
    el "p" $ blank
    el "div" $ do 
        el "p" (text "O projeto tem como principal objetivo explorar as noções de desenvolvimento e as habilidades dos alunos explorando de forma prática o que foi aprendido em sala de aula. ")
        el "p" $ blank
        el "p" $ blank
        el "p" (text "A ideia desse projeto é ser um crude simples para demonstração de como funciona o desenvolvimento em Haskel, para isto foi pensando em um tema que inclua as rotas para adição, edição, listagem de dados no banco de dados. A temática então gira em torno de filmes e seus elementos principais (nome, genêro e ano de lançamento).")


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Cine Especial"
      elAttr "link" ("href" =: static @"bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "meta" ("charset" =: "utf-8") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank            
      
    
  , _frontend_body = do
      mainPag

      --el "h1" $ text $ "Meme para alegrar a vida" 
      --el "p" $ text $ "Minha cara quando eu ver minha nota em Haskell: "
      --elAttr "img" ("src" =: static @"meme.jpg") blank

  }
