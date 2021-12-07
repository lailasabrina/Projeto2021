{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

-- Definindo as tabelas e suas características

data Cliente = Cliente Text deriving (Generic, ToJSON, FromJSON)

data Filme = Filme {
    filmeId :: Int,
    filmeNome :: Text,
    filmeGenero :: Text,
    filmeAno :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Serie = Serie {
    serieId :: Int,
    serieNome :: Text,
    serieGenero :: Text,
    serieTemp :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)
