{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
--
-- Provides 'OpenApiUI' and corresponding 'openApiUIServer' to embed
-- <http://swagger.io/swagger-ui/ Swagger UI> into the application.
--
-- All of UI files are embedded into the binary.
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

module Servant.OpenApi.UI.Core (
    -- * OpenApi UI API
    OpenApiSchemaUI,
    OpenApiSchemaUI',

    -- * Implementation details
    OpenApiUiHtml(..),
    openApiSchemaUIServerImpl,
    openApiSchemaUIServerImpl',
    Handler,
    ) where

import Data.ByteString                (ByteString)
import Data.OpenApi                   (OpenApi)
import GHC.TypeLits                   (KnownSymbol, Symbol, symbolVal)
import Network.Wai.Application.Static (embeddedSettings, staticApp)
import Servant
import Servant.HTML.Blaze             (HTML)
import Text.Blaze                     (ToMarkup (..))

import qualified Data.Text as T

-- | OpenAPI schema + ui api.
--
-- @openApiSchemaUI "openapi-ui" "openapi.json"@ will result into following hierarchy:
--
-- @
-- \/openapi.json
-- \/openapi-ui
-- \/openapi-ui\/index.html
-- \/openapi-ui\/...
-- @
--
type OpenApiSchemaUI (dir :: Symbol) (schema :: Symbol) =
    OpenApiSchemaUI' dir (schema :> Get '[JSON] OpenApi)

-- | Use 'OpenApiSchemaUI'' when you need even more control over
-- where @openapi.json@ is served (e.g. subdirectory).
type OpenApiSchemaUI' (dir :: Symbol) (api :: *) =
    api
    :<|> dir :>
        ( Get '[HTML] (OpenApiUiHtml dir api)
        :<|> "index.html" :> Get '[HTML] (OpenApiUiHtml dir api)
        :<|> Raw
        )

-- | Index file for OpenAPI ui.
--
-- It's configured by the location of OpenAPI schema and directory it lives under.
--
-- Implementation detail: the @index.html@ is prepopulated with parameters
-- to find schema file automatically.
data OpenApiUiHtml (dir :: Symbol) (api :: *) = OpenApiUiHtml T.Text

instance (KnownSymbol dir, HasLink api, Link ~ MkLink api Link, IsElem api api)
    => ToMarkup (OpenApiUiHtml dir api)
  where
    toMarkup (OpenApiUiHtml template) = preEscapedToMarkup
        $ T.replace "SERVANT_OPENAPI_UI_SCHEMA" schema
        $ T.replace "SERVANT_OPENAPI_UI_DIR" dir
        $ template
      where
        schema = T.pack $ uriPath . linkURI $ safeLink proxyApi proxyApi
        dir    = T.pack $ symbolVal (Proxy :: Proxy dir)
        proxyApi = Proxy :: Proxy api

openApiSchemaUIServerImpl
    :: (Server api ~ Handler OpenApi)
    => T.Text -> [(FilePath, ByteString)]
    -> OpenApi -> Server (OpenApiSchemaUI' dir api)
openApiSchemaUIServerImpl indexTemplate files openapi
  = openApiSchemaUIServerImpl' indexTemplate files $ return openapi

-- | Use a custom server to serve the OpenApi spec source.
openApiSchemaUIServerImpl'
    :: T.Text -> [(FilePath, ByteString)]
    -> Server api -> Server (OpenApiSchemaUI' dir api)
openApiSchemaUIServerImpl' indexTemplate files server
       = server
    :<|> return (OpenApiUiHtml indexTemplate)
    :<|> return (OpenApiUiHtml indexTemplate)
    :<|> rest
  where
    rest = Tagged $ staticApp $ embeddedSettings files
