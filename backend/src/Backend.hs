module Backend where

import Data.Text (Text)

import Obelisk.Backend as Ob
import Obelisk.Route

import Common.Route
import Frontend

backend :: Either Text (IO ())
backend = do
  let backendConfig = BackendConfig
        { _backendConfig_frontend = frontend
        , _backendConfig_routeEncoder = obeliskRouteEncoder routeComponentEncoder routeRestEncoder --TODO: Factor this out so that it isn't partially redundant with _frontend_routeEncoder
        }
  Ob.backend backendConfig
