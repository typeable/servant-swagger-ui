{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.OpenApi.UI
-- Copyright   :  (C) 2016-2018 Oleg Grenrus
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Provides 'OpenApiSchemaUI' and corresponding 'openApiSchemaUIServer' to embed
-- <http://swagger.io/swagger-ui/ Swagger UI> into the application.
--
-- All of the UI files are embedded into the binary.
--
-- /An example:/
--
-- @
-- -- | Actual API.
-- type BasicAPI = Get '[PlainText, JSON] Text
--     :\<|> "cat" :> Capture ":name" CatName :> Get '[JSON] Cat
--
-- -- | API type with bells and whistles, i.e. schema file and openapi-ui.
-- type API = 'OpenApiSchemaUI' "openapi-ui" "openapi.json"
--     :\<|> BasicAPI
--
-- -- | Servant server for an API
-- server :: Server API
-- server = 'openApiSchemaUIServer' openApiDoc
--     :\<|> (pure "Hello World" :\<|> catEndpoint)
--   where
--     catEndpoint name = pure $ Cat name False
-- @

module Servant.OpenApi.UI (
    -- * OpenApi UI API
    OpenApiSchemaUI,
    OpenApiSchemaUI',
    openApiSchemaUIServer,
    openApiSchemaUIServer',

    -- ** Official OpenAPI ui
    openApiUiIndexTemplate,
    openApiUiFiles,
    ) where

import Servant.OpenApi.UI.Core

import Data.ByteString (ByteString)
import Data.OpenApi    (OpenApi)
import Data.Text       (Text)
import FileEmbedLzma
import Servant

-- | Serve OpenApi UI on @/dir@ using @api@ as a OpenApi spec source.
--
-- @
-- openApiSchemaUIServer :: OpenApi -> Server (OpenApiSchemaUI schema dir)
-- @
openApiSchemaUIServer
    :: (Server api ~ Handler OpenApi)
    => OpenApi -> Server (OpenApiSchemaUI' dir api)
openApiSchemaUIServer =
    openApiSchemaUIServerImpl openApiUiIndexTemplate openApiUiFiles

-- | Use a custom server to serve the OpenApi spec source.
--
-- This allows even more control over how the spec source is served.
-- It allows, for instance, serving the spec source with authentication,
-- customizing the response based on the client or serving a openapi.yaml
-- instead.
openApiSchemaUIServer'
    :: Server api -> Server (OpenApiSchemaUI' dir api)
openApiSchemaUIServer' =
    openApiSchemaUIServerImpl' openApiUiIndexTemplate openApiUiFiles

openApiUiIndexTemplate :: Text
openApiUiIndexTemplate = $(embedText "index.html.tmpl")

openApiUiFiles :: [(FilePath, ByteString)]
openApiUiFiles = $(embedRecursiveDir "swagger-ui-dist-3.36.1")
