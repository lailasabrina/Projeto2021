{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Database.PostgreSQL.Simple 
import Snap.Core
import qualified Data.Aeson as A
import Data.Text
import Control.Monad.IO.Class (liftIO)

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS cliente\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-34-203-114-67.compute-1.amazonaws.com" -- host
					  5432  -- porta
					  "gvhmrakcjvtvux" -- usuario
					  "6f8974ad96840a34bff3b23f388fe9510e31d297a09cce9212a8f2b1ef1c0ab7" -- senha
					  "d4v46ksjsv3kg6" -- banco

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
        dbcon <- connect getConn
        serve $ \case 
            BackendRoute_Cliente :/ () -> do
                Just nome <- A.decode <$> readRequestBody 2000
                liftIO $ do 
                     execute_ dbcon migration
                     execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
                modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
        return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
