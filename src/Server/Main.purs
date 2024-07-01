module Server.Main where

import Prelude

import Data.HashMap as DH
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Ref as ER
import Effect.Timer as ET
import Payload.Server (defaultOpts)
import Payload.Server as PS
import Server.Configuration as CF
import Server.Database as SD
import Server.Guard (guards)
import Server.Handler as SH
import Server.Effect (Configuration)
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events (aliveDelay)
import Server.WebSocket.Events as SWE
import Shared.Options.WebSocket (port)
import Shared.Spec (spec)

main ∷ Effect Unit
main = do
      configuration ← CF.readConfiguration
      startWebSocketServer configuration
      startHttpServer configuration

startWebSocketServer ∷ Configuration → Effect Unit
startWebSocketServer configuration = do
      userAvailability ← ER.new DH.empty
      webSocketServer ← SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "Web socket now up on ws://localhost:" <> show port)
      SW.onServerError webSocketServer SWE.handleError
      pool ← SD.newPool configuration
      SW.onConnection webSocketServer (SWE.handleConnection configuration pool userAvailability)
      intervalId ← ET.setInterval aliveDelay (SWE.checkLastSeen userAvailability *> SWE.persistLastSeen { pool, userAvailability })
      SW.onServerClose webSocketServer (const (ET.clearInterval intervalId))

startHttpServer ∷ Configuration → Effect Unit
startHttpServer configuration@{ port } = do
      pool ← SD.newPool configuration
      EA.launchAff_ $ void do
            PS.startGuarded (defaultOpts { port = port }) spec
                  { guards: guards { configuration, pool }
                  , handlers: SH.handlers { configuration, pool }
                  }
      EC.log $ "HTTP now up on http://localhost:" <> show port
