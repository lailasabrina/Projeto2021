{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, GADTs #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Common.Api
import Data.Aeson.Text

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-34-203-114-67.compute-1.amazonaws.com" -- host
            5432  -- porta
            "gvhmrakcjvtvux" -- usuario
            "6f8974ad96840a34bff3b23f388fe9510e31d297a09cce9212a8f2b1ef1c0ab7" -- senha
            "d4v46ksjsv3kg6" -- banco

migrate :: Query
migrate = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"
 
migrateFilm :: Query
migrateFilm = "CREATE TABLE IF NOT EXISTS filme (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, genero TEXT NOT NULL, ano DATE NOT NULL)"

migrateSerie :: Query
migrateSerie = "CREATE TABLE IF NOT EXISTS serie (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, genero TEXT NOT NULL, temp INTEGER NOT NULL)"
 

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do 
      dbcon <- connect getConn
      serve $ do
          \case 
            BackendRoute_Editar :/ pid -> method POST $ do
                film <- A.decode <$> readRequestBody 2000
                case film of
                    Just filme -> do
                        liftIO $ do
                            execute_ dbcon migrateFilm
                            execute dbcon "UPDATE filme SET nome = ?, genero = ?, ano = ? WHERE id = ?" 
                                       (filmeNome filme, filmeGenero filme, filmeAno filme,pid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_Listar :/ () -> method GET $ do
                res :: [Filme] <- liftIO $ do
                        execute_ dbcon migrateFilm
                        query_ dbcon "SELECT * from filme"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText res)
            BackendRoute_Apagar :/ pid -> method POST $ do 
                res :: [Filme] <- liftIO $ do
                        execute_ dbcon migrateFilm
                        query dbcon "SELECT * from filme where id=?" (Only (pid :: Int))
                if res /= [] then do
                    liftIO $ do
                        execute_ dbcon migrateFilm
                        execute dbcon "DELETE from filme where id=?" (Only (pid :: Int))
                    modifyResponse $ setResponseStatus 200 "OK"   
                else
                    modifyResponse $ setResponseStatus 404 "NOT FOUND"  
            BackendRoute_Buscar :/ pid -> method GET $ do 
                res :: [Filme] <- liftIO $ do
                        execute_ dbcon migrateFilm
                        query dbcon "SELECT * from filme where id=?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"  
            BackendRoute_Filme :/ () -> method POST $ do
                filme <- A.decode <$> readRequestBody 2000
                case filme of
                     Just film -> do
                         liftIO $ do
                             execute_ dbcon migrateFilm
                             execute dbcon 
                                     "INSERT INTO filme(nome,genero,ano) VALUES (?,?,?)" 
                                     (filmeNome film, filmeGenero film, filmeAno film)
                         modifyResponse $ setResponseStatus 200 "OK"    
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
            BackendRoute_EditarS :/ pid -> method POST $ do
                ser <- A.decode <$> readRequestBody 2000
                case ser of
                    Just serie -> do
                        liftIO $ do
                            execute_ dbcon migrateSerie
                            execute dbcon "UPDATE serie SET nome = ?, genero = ?, temp = ? WHERE id = ?" 
                                       (serieNome serie, serieGenero serie, serieTemp serie,pid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_ListarS :/ () -> method GET $ do
                res :: [Serie] <- liftIO $ do
                        execute_ dbcon migrateSerie
                        query_ dbcon "SELECT * from serie"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText res)
            BackendRoute_ApagarS :/ pid -> method POST $ do 
                res :: [Serie] <- liftIO $ do
                        execute_ dbcon migrateSerie
                        query dbcon "SELECT * from serie where id=?" (Only (pid :: Int))
                if res /= [] then do
                    liftIO $ do
                        execute_ dbcon migrateSerie
                        execute dbcon "DELETE from serie where id=?" (Only (pid :: Int))
                    modifyResponse $ setResponseStatus 200 "OK"   
                else
                    modifyResponse $ setResponseStatus 404 "NOT FOUND"  
            BackendRoute_BuscarS :/ pid -> method GET $ do 
                res :: [Serie] <- liftIO $ do
                        execute_ dbcon migrateSerie
                        query dbcon "SELECT * from serie where id=?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"  
            BackendRoute_Serie :/ () -> method POST $ do
                serie <- A.decode <$> readRequestBody 2000
                case serie of
                     Just ser -> do
                         liftIO $ do
                             execute_ dbcon migrateSerie
                             execute dbcon 
                                     "INSERT INTO serie(nome,genero,temp) VALUES (?,?,?)" 
                                     (serieNome ser, serieGenero ser, serieTemp ser)
                         modifyResponse $ setResponseStatus 200 "OK"    
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
            BackendRoute_Cliente :/ () -> do
                Just nome <- A.decode <$> readRequestBody 2000
                liftIO $ do 
                     execute_ dbcon migrate
                     execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
                modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }