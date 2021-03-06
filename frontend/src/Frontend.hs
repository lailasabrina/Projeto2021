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

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def

getFilmReq :: Int -> XhrRequest ()
getFilmReq pid = xhrRequest "GET" (getPath (BackendRoute_Buscar :/ pid)) def

deleteReq :: Int -> XhrRequest ()
deleteReq pid = xhrRequest "DELETE" (getPath (BackendRoute_Apagar :/ pid)) def

getListSReq :: XhrRequest ()
getListSReq = xhrRequest "GET" (getPath (BackendRoute_ListarS :/ ())) def

getSerieReq :: Int -> XhrRequest ()
getSerieReq pid = xhrRequest "GET" (getPath (BackendRoute_BuscarS :/ pid)) def

deleteSReq :: Int -> XhrRequest ()
deleteSReq pid = xhrRequest "DELETE" (getPath (BackendRoute_ApagarS :/ pid)) def

getListNReq :: XhrRequest ()
getListNReq = xhrRequest "GET" (getPath (BackendRoute_ListarN :/ ())) def

getNovelReq :: Int -> XhrRequest ()
getNovelReq pid = xhrRequest "GET" (getPath (BackendRoute_BuscarN :/ pid)) def

deleteNReq :: Int -> XhrRequest ()
deleteNReq pid = xhrRequest "DELETE" (getPath (BackendRoute_ApagarN :/ pid)) def

data Acao = Perfil Int | Editar Int | Apagar Int | PerfilS Int | EditarS Int | ApagarS Int | PerfilN Int | EditarN Int | ApagarN Int

--- Backend Filmes ---

reqFilm :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
reqFilm = do
    el "h1" (text "Adicionar filmes")
    elAttr "div" ("class" =: "container") $ do
        elAttr "div" ("class" =: "row") $ do
            elAttr "div" ("class" =: "col-md-8 order-md-1") $ do
                el "p" (text "")
                elAttr "div" ("class" =: "mb-3") $ do
                    elAttr "p" ("class" =: "form-label") (text "Nome: ")
                    nome <- inputElement def 
                    elAttr "p" ("class" =: "form-label")(text "G??nero: ")
                    genero <- inputElement def
                    elAttr "p" ("class" =: "form-label")(text "Ano de Lan??amento: ")
                    ano <- dateInput
                    let film = fmap (\((a,n),g) -> Filme 0 n g a) (zipDyn (zipDyn ano (_inputElement_value nome))(_inputElement_value genero))
                    elAttr "div" ("class" =: "mb-3") $ do
                        (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-dark") (text "Inserir")
                        let click = domEvent Click submitBtn
                        let filmEvt = tag (current film) click
                        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                            (pure never)
                            (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Filme :/ ()) <$> filmEvt))
                        return ()

tabRegistro :: (PostBuild t m, DomBuilder t m) => Dynamic t Filme-> m (Event t Acao)
tabRegistro pr = do 
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . filmeId) pr)
        el "td" (dynText $ fmap (T.pack . show . filmeNome) pr)
        el "td" (dynText $ fmap (T.pack . show . filmeGenero) pr)
        el "td" (dynText $ fmap (T.pack . show . filmeAno) pr)  
        evt <- elAttr "td" ("class" =: "get") $ fmap (fmap (const Perfil)) (button "perfil")
        evt2 <- elAttr "td" ("class" =: "edit") $ fmap (fmap (const Editar)) (button "editar")
        evt3 <- elAttr "td" ("class" =: "delete") $ fmap (fmap (const Apagar)) (button "apagar")
        return (attachPromptlyDynWith (flip ($)) (fmap filmeId pr) (leftmost [evt,evt2,evt3]))

reqTabela :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabela = Workflow $ do
    el "h1" (text "Listar filmes")
    elAttr "div" ("class" =: "container") $ do
                btn <- button "Abrir lista"
                films :: Dynamic t (Event t (Maybe [Filme])) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> btn))
                evt <- return (fmap (fromMaybe []) $ switchDyn films)
                dynP <- foldDyn (++) [] evt
                tb <- elAttr "table" ("class" =: "table")$ do
                    el "thead" $ do
                        el "tr" $ do
                            elAttr "th" ("scope" =: "col")(text "Id")
                            elAttr "th" ("scope" =: "col")(text "Nome")
                            elAttr "th" ("scope" =: "col")(text "G??nero")
                            elAttr "th" ("scope" =: "col")(text "Ano de Lan??amento")
                            elAttr "th" ("scope" =: "col")(text "")
                            elAttr "th" ("scope" =: "col")(text "")
                            elAttr "th" ("scope" =: "col")(text "")
                            
                    
                    el "tbody" $ do
                        simpleList dynP tabRegistro
                tb' <- return $ switchDyn $ fmap leftmost tb
                return ("", escolherPag <$> tb')
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
    el "h1" (text "Perfil")
    elAttr "div" ("class" =: "container") $ do
        btn <- button "mostrar"
        film :: Dynamic t (Event t (Maybe Filme)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync (const (getFilmReq pid) <$> btn))
        mdyn <- holdDyn Nothing (switchDyn film)
        dynP <- return ((fromMaybe (Filme 0 "" "" 0)) <$> mdyn)
        elAttr "div" ("class" =: "card-body") $ do
            el "div" (dynText $ fmap filmeNome dynP)
            el "div" (dynText $ fmap (T.pack . show . filmeGenero) dynP)
            el "div" (dynText $ fmap (T.pack . show . filmeAno) dynP)
        ret <- button "voltar"
        return ("Id: " <> (T.pack $ show pid), reqTabela <$ ret) 

delPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
delPerfil  pid = Workflow $ do
    el "h1" (text "Apagar filme")
    elAttr "div" ("class" =: "container") $ do
    el "div" $ do
        el "p" (text "Deseja realmente apagar esse filme e todos seus dados?")        
    (btnDel,film) <- elAttr' "button" ("class"=: "btn btn-danger") (text "Deletar")

    let delEvt = domEvent Click btnDel
    film :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_Apagar :/ pid) 
            <$> delEvt))    

    return ("" <> "", reqTabela <$ delEvt)

editarPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil pid = Workflow $ do
    el "h1" (text "Editar filme")
    elAttr "div" ("class" =: "container") $ do
        btn <- button "mostrar"
        film :: Dynamic t (Event t (Maybe Filme)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync
            (const (getFilmReq pid) <$> btn))
        mdyn <- return (switchDyn film)
        dynE <- return ((fromMaybe (Filme 0 "" "" 0)) <$> mdyn)
        elAttr "div" ("class" =: "row") $ do
            elAttr "div" ("class" =: "col-md-8 order-md-1") $ do
                el "p" (text "")
                elAttr "div" ("class" =: "mb-3") $ do
                    elAttr "p" ("class" =: "form-label") (text "Nome: ")
                    nome <- inputElement $ 
                        def & inputElementConfig_setValue .~ (fmap filmeNome dynE)
                    elAttr "p" ("class" =: "form-label") (text "G??nero: ")
                    genero <- inputElement $ 
                        def & inputElementConfig_setValue .~ (fmap filmeGenero dynE)
                    elAttr "p" ("class" =: "form-label") (text "Ano de lan??amento: ")
                    ano <- numberInputDyn (fmap filmeAno dynE)
                    
                    let film = fmap (\((a,n),g) -> Filme 0 n g a) (zipDyn (zipDyn ano (_inputElement_value nome))(_inputElement_value genero))
                    el "p" (text "")    
                    submitBtn <- button "editar"
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

--- Backend S??ries ---

reqSerie :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
reqSerie = do
    el "h1" (text "Adicionar s??ries")
    elAttr "div" ("class" =: "container") $ do
        elAttr "div" ("class" =: "row") $ do
            elAttr "div" ("class" =: "col-md-8 order-md-1") $ do
                el "p" (text "")
                elAttr "div" ("class" =: "mb-3") $ do
                    elAttr "p" ("class" =: "form-label") (text "Nome: ")
                    nome <- inputElement def 
                    elAttr "p" ("class" =: "form-label")(text "G??nero: ")
                    genero <- inputElement def
                    elAttr "p" ("class" =: "form-label")(text "Temporada(s): ")
                    temp <- numberInput
                    let ser = fmap (\((t,n),g) -> Serie 0 n g t) (zipDyn (zipDyn temp (_inputElement_value nome))(_inputElement_value genero))
                    elAttr "div" ("class" =: "mb-3") $ do
                        (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-dark") (text "Inserir")
                        let click = domEvent Click submitBtn
                        let serEvt = tag (current ser) click
                        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                            (pure never)
                            (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Serie :/ ()) <$> serEvt))
                        return ()

tabRegistroS :: (PostBuild t m, DomBuilder t m) => Dynamic t Serie-> m (Event t Acao)
tabRegistroS pr = do 
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . serieId) pr)
        el "td" (dynText $ fmap (T.pack . show . serieNome) pr)
        el "td" (dynText $ fmap (T.pack . show . serieGenero) pr)
        el "td" (dynText $ fmap (T.pack . show . serieTemp) pr)  
        evt <- elAttr "td" ("class" =: "get") $ fmap (fmap (const PerfilS)) (button "perfil")
        evt2 <- elAttr "td" ("class" =: "edit") $ fmap (fmap (const EditarS)) (button "editar")
        evt3 <- elAttr "td" ("class" =: "delete") $ fmap (fmap (const ApagarS)) (button "apagar")
        return (attachPromptlyDynWith (flip ($)) (fmap serieId pr) (leftmost [evt,evt2,evt3]))

reqTabelaS :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabelaS = Workflow $ do
    el "h1" (text "Listar s??ries")
    elAttr "div" ("class" =: "container") $ do
                btn <- button "Abrir lista"
                series :: Dynamic t (Event t (Maybe [Serie])) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (const getListSReq <$> btn))
                evt <- return (fmap (fromMaybe []) $ switchDyn series)
                dynP <- foldDyn (++) [] evt
                tb <- elAttr "table" ("class" =: "table")$ do
                    el "thead" $ do
                        el "tr" $ do
                            elAttr "th" ("scope" =: "col")(text "Id")
                            elAttr "th" ("scope" =: "col")(text "Nome")
                            elAttr "th" ("scope" =: "col")(text "G??nero")
                            elAttr "th" ("scope" =: "col")(text "Temporada(s)")
                            elAttr "th" ("scope" =: "col")(text "")
                            elAttr "th" ("scope" =: "col")(text "")
                            elAttr "th" ("scope" =: "col")(text "")
                            
                    
                    el "tbody" $ do
                        simpleList dynP tabRegistroS
                tb' <- return $ switchDyn $ fmap leftmost tb
                return ("", escolherPagS <$> tb')
                where
                    escolherPagS (PerfilS pid) = pagPerfilS pid
                    escolherPagS (EditarS pid) = editarPerfilS pid
                    escolherPagS (ApagarS pid) = delPerfilS pid
      
pagPerfilS :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilS pid = Workflow $ do
    el "h1" (text "Perfil")
    elAttr "div" ("class" =: "container") $ do
        btn <- button "mostrar"
        ser:: Dynamic t (Event t (Maybe Serie)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync (const (getSerieReq pid) <$> btn))
        mdyn <- holdDyn Nothing (switchDyn ser)
        dynP <- return ((fromMaybe (Serie 0 "" "" 0)) <$> mdyn)
        elAttr "div" ("class" =: "card-body") $ do
            el "div" (dynText $ fmap serieNome dynP)
            el "div" (dynText $ fmap (T.pack . show . serieGenero) dynP)
            el "div" (dynText $ fmap (T.pack . show . serieTemp) dynP)
        ret <- button "voltar"
        return ("Id: " <> (T.pack $ show pid), reqTabelaS <$ ret) 

delPerfilS :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
delPerfilS  pid = Workflow $ do
    el "h1" (text "Apagar S??rie")
    elAttr "div" ("class" =: "container") $ do
    el "div" $ do
        el "p" (text "Deseja realmente apagar essa s??rie e todos seus dados?")        
    (btnDel,ser) <- elAttr' "button" ("class"=: "btn btn-danger") (text "Deletar")

    let delEvt = domEvent Click btnDel
    ser :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_ApagarS :/ pid) 
            <$> delEvt))    

    return ("" <> "", reqTabelaS <$ delEvt) 

editarPerfilS :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilS pid = Workflow $ do
    el "h1" (text "Editar s??rie")
    elAttr "div" ("class" =: "container") $ do
        btn <- button "mostrar"
        ser :: Dynamic t (Event t (Maybe Serie)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync
            (const (getSerieReq pid) <$> btn))
        mdyn <- return (switchDyn ser)
        dynE <- return ((fromMaybe (Serie 0 "" "" 0)) <$> mdyn)
        elAttr "div" ("class" =: "row") $ do
            elAttr "div" ("class" =: "col-md-8 order-md-1") $ do
                el "p" (text "")
                elAttr "div" ("class" =: "mb-3") $ do
                    elAttr "p" ("class" =: "form-label") (text "Nome: ")
                    nome <- inputElement $ 
                        def & inputElementConfig_setValue .~ (fmap serieNome dynE)
                    elAttr "p" ("class" =: "form-label") (text "G??nero: ")
                    genero <- inputElement $ 
                        def & inputElementConfig_setValue .~ (fmap serieGenero dynE)
                    elAttr "p" ("class" =: "form-label") (text "Temporada(s): ")
                    temp <- numberInputDyn (fmap serieTemp dynE)
                    
                    let ser = fmap (\((s,n),g) -> Serie 0 n g s) (zipDyn (zipDyn temp (_inputElement_value nome))(_inputElement_value genero))
                    el "p" (text "")    
                    submitBtn <- button "editar"
                    let serEvt = tag (current ser) submitBtn
                    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                        (pure never)
                        (fmap decodeXhrResponse <$> 
                            performRequestAsync (sendRequest (BackendRoute_EditarS :/ pid) 
                            <$> serEvt)) 
                    return ("Perfil: " <> (T.pack $ show pid), reqTabelaS <$ submitBtn) 

reqListaS :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaS = do
    r <- workflow reqTabelaS
    el "div" (dynText r)

--- Backend Novelas ---

reqNovel :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
reqNovel = do
    el "h1" (text "Adicionar novelas")
    elAttr "div" ("class" =: "container") $ do
        elAttr "div" ("class" =: "row") $ do
            elAttr "div" ("class" =: "col-md-8 order-md-1") $ do
                el "p" (text "")
                elAttr "div" ("class" =: "mb-3") $ do
                    elAttr "p" ("class" =: "form-label") (text "Nome: ")
                    nome <- inputElement def 
                    elAttr "p" ("class" =: "form-label")(text "Emissora: ")
                    emissora <- inputElement def
                    elAttr "p" ("class" =: "form-label")(text "Ano da primeira exibi????o: ")
                    ano <- dateInput
                    let novel = fmap (\((a,n),e) -> Novela 0 n e a) (zipDyn (zipDyn ano (_inputElement_value nome))(_inputElement_value emissora))
                    elAttr "div" ("class" =: "mb-3") $ do
                        (submitBtn,_) <- elAttr' "button" ("class"=:"btn btn-dark") (text "Inserir")
                        let click = domEvent Click submitBtn
                        let filmEvt = tag (current novel) click
                        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                            (pure never)
                            (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Novela :/ ()) <$> filmEvt))
                        return ()

tabRegistroN :: (PostBuild t m, DomBuilder t m) => Dynamic t Novela-> m (Event t Acao)
tabRegistroN pr = do 
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . novelaId) pr)
        el "td" (dynText $ fmap (T.pack . show . novelaNome) pr)
        el "td" (dynText $ fmap (T.pack . show . novelaEmissora) pr)
        el "td" (dynText $ fmap (T.pack . show . novelaAno) pr)  
        evt <- elAttr "td" ("class" =: "get") $ fmap (fmap (const PerfilN)) (button "perfil")
        evt2 <- elAttr "td" ("class" =: "edit") $ fmap (fmap (const EditarN)) (button "editar")
        evt3 <- elAttr "td" ("class" =: "delete") $ fmap (fmap (const ApagarN)) (button "apagar")
        return (attachPromptlyDynWith (flip ($)) (fmap novelaId pr) (leftmost [evt,evt2,evt3]))

reqTabelaN :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabelaN = Workflow $ do
    el "h1" (text "Listar novelas")
    elAttr "div" ("class" =: "container") $ do
                btn <- button "Abrir lista"
                novelas :: Dynamic t (Event t (Maybe [Novela])) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (const getListNReq <$> btn))
                evt <- return (fmap (fromMaybe []) $ switchDyn novelas)
                dynP <- foldDyn (++) [] evt
                tb <- elAttr "table" ("class" =: "table")$ do
                    el "thead" $ do
                        el "tr" $ do
                            elAttr "th" ("scope" =: "col")(text "Id")
                            elAttr "th" ("scope" =: "col")(text "Nome")
                            elAttr "th" ("scope" =: "col")(text "Emissora")
                            elAttr "th" ("scope" =: "col")(text "Ano da primeira exibi????o")
                            elAttr "th" ("scope" =: "col")(text "")
                            elAttr "th" ("scope" =: "col")(text "")
                            elAttr "th" ("scope" =: "col")(text "")
                            
                    
                    el "tbody" $ do
                        simpleList dynP tabRegistroN
                tb' <- return $ switchDyn $ fmap leftmost tb
                return ("", escolherPagN <$> tb')
                where
                    escolherPagN (PerfilN pid) = pagPerfilN pid
                    escolherPagN (EditarN pid) = editarPerfilN pid
                    escolherPagN (ApagarN pid) = delPerfilN pid
      
pagPerfilN :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfilN pid = Workflow $ do
    el "h1" (text "Perfil")
    elAttr "div" ("class" =: "container") $ do
        btn <- button "mostrar"
        novel :: Dynamic t (Event t (Maybe Novela)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync (const (getNovelReq pid) <$> btn))
        mdyn <- holdDyn Nothing (switchDyn novel)
        dynP <- return ((fromMaybe (Novela 0 "" "" 0)) <$> mdyn)
        elAttr "div" ("class" =: "card-body") $ do
            el "div" (dynText $ fmap novelaNome dynP)
            el "div" (dynText $ fmap (T.pack . show . novelaEmissora) dynP)
            el "div" (dynText $ fmap (T.pack . show . novelaAno) dynP)
        ret <- button "voltar"
        return ("Id: " <> (T.pack $ show pid), reqTabelaN <$ ret) 

delPerfilN :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
delPerfilN  pid = Workflow $ do
    el "h1" (text "Apagar novela")
    elAttr "div" ("class" =: "container") $ do
    el "div" $ do
        el "p" (text "Deseja realmente apagar essa novela e todos seus dados?")        
    (btnDel,novel) <- elAttr' "button" ("class"=: "btn btn-danger") (text "Deletar")

    let delEvt = domEvent Click btnDel
    novel :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_ApagarN :/ pid) 
            <$> delEvt))    

    return ("" <> "", reqTabelaN <$ delEvt) 

editarPerfilN :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfilN pid = Workflow $ do
    el "h1" (text "Editar novela")
    elAttr "div" ("class" =: "container") $ do
        btn <- button "mostrar"
        novel :: Dynamic t (Event t (Maybe Novela)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync
            (const (getNovelReq pid) <$> btn))
        mdyn <- return (switchDyn novel)
        dynE <- return ((fromMaybe (Novela 0 "" "" 0)) <$> mdyn)
        elAttr "div" ("class" =: "row") $ do
            elAttr "div" ("class" =: "col-md-8 order-md-1") $ do
                el "p" (text "")
                elAttr "div" ("class" =: "mb-3") $ do
                    elAttr "p" ("class" =: "form-label") (text "Nome: ")
                    nome <- inputElement $ 
                        def & inputElementConfig_setValue .~ (fmap novelaNome dynE)
                    elAttr "p" ("class" =: "form-label") (text "Emissora: ")
                    emissora <- inputElement $ 
                        def & inputElementConfig_setValue .~ (fmap novelaEmissora dynE)
                    elAttr "p" ("class" =: "form-label") (text "Ano da primeira exibi????o: ")
                    ano <- numberInputDyn (fmap novelaAno dynE)
                    
                    let novel = fmap (\((a,n),e) -> Novela 0 n e a) (zipDyn (zipDyn ano (_inputElement_value nome))(_inputElement_value emissora))
                    el "p" (text "")    
                    submitBtn <- button "editar"
                    let novelEvt = tag (current novel) submitBtn
                    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                        (pure never)
                        (fmap decodeXhrResponse <$> 
                            performRequestAsync (sendRequest (BackendRoute_EditarN :/ pid) 
                            <$> novelEvt)) 
                    return ("Perfil: " <> (T.pack $ show pid), reqTabelaN <$ submitBtn) 

reqListaN :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaN = do
    r <- workflow reqTabelaN
    el "div" (dynText r)

-----------------------------------

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
----------------------------------------------------------------------

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6 | Pagina7 | Pagina8
   
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
        p4 <- clickLi Pagina4 "Inserir S??ries"
        p5 <- clickLi Pagina5 "Listar S??ries"
        p6 <- clickLi Pagina6 "Inserir Novelas"
        p7 <- clickLi Pagina7 "Listar Novelas"
        p8 <- clickLi Pagina8 "Sobre"
        return (leftmost [p1,p2,p3,p4,p5,p6,p7,p8])
    holdDyn Pagina0 evs    
    
currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = 
    case p of
         Pagina0 -> home
         Pagina1 -> home
         Pagina2 -> reqFilm
         Pagina3 -> reqLista
         Pagina4 -> reqSerie
         Pagina5 -> reqListaS
         Pagina6 -> reqNovel
         Pagina7 -> reqListaN
         Pagina8 -> sobre
         
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

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) =>
               Event t a -> m (Dynamic t a)
numberInputDyn p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
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
    el "h1" $ text "Soma de dois n??meros"
    evt <- sumButton
    s <- holdDyn 0 evt 
    el "div" (dynText $ fmap (T.pack . show) s)  


lista :: DomBuilder t m => m ()
lista = do
  el "h1" $ text "Listinha b??sica"
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
    el "h1" $ text "Alunos envolvidos nesse projeto"
    el "p" $ blank
    el "div" $ do 
        el "p" (text "")
        el "p" (text "")
        el "p" (text "")
        elAttr "p" ("class" =: "name")(text "Danilo dos Santos Gomes")
        elAttr "p" ("class" =: "name")(text "Laila Sabrina Alves Silva de Lima")
        elAttr "p" ("class" =: "name")(text "Marcos Vinicius Sousa do Rosario")
        elAttr "p" ("class" =: "name")(text "")
        el "p" (text "")
        elAttr "p" ("class" =: "espace")(text "Mais informa????es na p??gina sobre.")

sobre :: (DomBuilder t m, PostBuild t m) => m ()
sobre = do
    el "h1" $ text "Sobre o projeto"
    el "p" $ blank
    el "div" $ do 
        elAttr "p" ("class" =: "sobre")(text "O projeto tem como principal objetivo explorar as no????es de desenvolvimento e as habilidades dos alunos explorando de forma pr??tica o que foi aprendido em sala de aula. ")
        el "p" $ blank
        el "p" $ blank
        elAttr "p" ("class" =: "sobre")(text "A ideia desse projeto ?? ser um crude simples para demonstra????o de como funciona o desenvolvimento em Haskel, para isto foi pensando em um tema que inclua as rotas para adi????o, edi????o, listagem de dados no banco de dados. A tem??tica ent??o gira em torno de filmes, s??ries e novelas, e seus elementos principais (nome, g??nero, temporadas, emissora e ano de lan??amento ou de exibi????o).")


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

      elAttr "div" ("class"=:"footer") $ do 
        elAttr "p" ("class"=:"center") $ text $ "Bem - Vindo(a)!" 
        elAttr "p" ("class"=:"center") $ text $ "Trabalho desenvolvido para a mat??ria de T??picos Especiais em Sistemas para a Internet III, do curso de Sistemas para Internet, objetivando a obten????o de nota para compor a m??dia."
        elAttr "img" ("src" =: static @"meme.jpg") blank

  }
