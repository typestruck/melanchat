module Server.Configuration where

import Prelude
import Server.Effect(Configuration)

import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Traversable as DT
import Effect (Effect)
import Effect.Exception as EE
import Environment (production)
import Node.Process as NP
import Shared.Unsafe as SU

-- | Reads environment variables
-- | The process will fail if any of them are missing on production, dummy values will be used for development
readConfiguration ∷ Effect Configuration
readConfiguration =
      if production then do
            port ← parsePort <$> NP.lookupEnv "PORT"
            variables ← DT.traverse getVariable [ "CAPTCHA_SECRET", "TOKEN_SECRET", "SALT", "DATABASE_HOST", "ADMIN_SECRET" ]
            case variables of
                  [ captchaSecret, tokenSecret, salt, host, adminSecret ] →
                        pure $
                              { port
                              , databaseHost: Just host
                              , captchaSecret
                              , tokenSecret
                              , salt
                              , adminSecret
                              }
                  _ → EE.throw "Wrong number of environment variables"
      else do
            databaseHost ← NP.lookupEnv "DATABASE_HOST"
            pure
                  { port: 8000
                  , databaseHost
                  , captchaSecret: ""
                  , tokenSecret: "so nice, so nice, I got you"
                  , salt: "put it back together"
                  , adminSecret: ""
                  }
      where
      getVariable name = do
            value ← NP.lookupEnv name
            case value of
                  Nothing → EE.throw $ "missing configuration: " <> name
                  Just v → pure v

      parsePort value = SU.fromJust do
            v ← value
            DI.fromString v
