{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text,unpack)
import Data.Functor.Identity
import Data.Function
import Obelisk.Route
import Obelisk.Route.TH

checFullREnc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & 
    \case 
          Left err -> error $ unpack err
          Right encoder -> encoder

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Cliente :: BackendRoute ()
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Filme :: BackendRoute ()
  BackendRoute_Serie :: BackendRoute ()
  BackendRoute_Novela :: BackendRoute () 
  BackendRoute_Listar  :: BackendRoute ()
  BackendRoute_ListarS  :: BackendRoute ()
  BackendRoute_ListarN  :: BackendRoute ()  
  BackendRoute_Buscar  :: BackendRoute Int
  BackendRoute_BuscarS  :: BackendRoute Int
  BackendRoute_BuscarN  :: BackendRoute Int
  BackendRoute_Apagar :: BackendRoute Int
  BackendRoute_ApagarS :: BackendRoute Int
  BackendRoute_ApagarN :: BackendRoute Int
  BackendRoute_Editar :: BackendRoute Int
  BackendRoute_EditarS :: BackendRoute Int
  BackendRoute_EditarN :: BackendRoute Int
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Cliente -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_Filme -> PathSegment "filme" $ unitEncoder mempty
      BackendRoute_Serie -> PathSegment "serie" $ unitEncoder mempty
      BackendRoute_Novela -> PathSegment "novela" $ unitEncoder mempty
      BackendRoute_Listar  -> PathSegment "listar" $ unitEncoder mempty
      BackendRoute_ListarS  -> PathSegment "listars" $ unitEncoder mempty
      BackendRoute_ListarN  -> PathSegment "listarn" $ unitEncoder mempty
      BackendRoute_Buscar  -> PathSegment "buscar" readShowEncoder
      BackendRoute_BuscarS  -> PathSegment "buscars" readShowEncoder
      BackendRoute_BuscarN  -> PathSegment "buscarn" readShowEncoder
      BackendRoute_Apagar -> PathSegment "apagar" readShowEncoder
      BackendRoute_ApagarS -> PathSegment "apagars" readShowEncoder
      BackendRoute_ApagarN -> PathSegment "apagarn" readShowEncoder
      BackendRoute_Editar -> PathSegment "editar" readShowEncoder
      BackendRoute_EditarS -> PathSegment "editars" readShowEncoder 
      BackendRoute_EditarN -> PathSegment "editarn" readShowEncoder
      )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
