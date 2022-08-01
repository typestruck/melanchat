module Server.Main where

import Prelude

import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Console as EC
import Effect.Ref as ER
import Effect.Timer as ET
import Environment (development)
import Payload.Server (defaultOpts)
import Payload.Server as PS
import Server.Configuration as CF
import Server.Database as SD
import Server.File as SF
import Server.Guard (guards)
import Server.Handler as SH
import Server.Types (Configuration)
import Server.WebSocket (Port(..))
import Server.WebSocket as SW
import Server.WebSocket.Events (aliveDelay)
import Server.WebSocket.Events as SWE
import Shared.Options.WebSocket (port)
import Shared.Spec (spec)

main ∷ Effect Unit
main = do
      configuration@{storageApplicationKeyId, storageApplicationKey} ← CF.readConfiguration
      when (not development) $ SF.init storageApplicationKeyId storageApplicationKey
      startWebSocketServer configuration
      startHTTPServer configuration

startWebSocketServer ∷ Configuration → Effect Unit
startWebSocketServer configuration = do
      allConnections ← ER.new DH.empty
      availability ← ER.new DH.empty
      webSocketServer ← SW.createWebSocketServerWithPort (Port port) {} $ const (EC.log $ "Web socket now up on ws://localhost:" <> show port)
      SW.onServerError webSocketServer SWE.handleError
      pool ← SD.newPool configuration
      SW.onConnection webSocketServer (SWE.handleConnection configuration pool allConnections availability)
      let reading = { pool, availability}
      intervalId ← ET.setInterval aliveDelay (SWE.checkLastSeen allConnections availability *> SWE.persistLastSeen reading)
      SW.onServerClose webSocketServer (const (EC.log "closing websocket server" *> ET.clearInterval intervalId *> SWE.persistLastSeen reading))

startHTTPServer ∷ Configuration → Effect Unit
startHTTPServer configuration@{ port } = do
      pool ← SD.newPool configuration
      EA.launchAff_ $ void do
            PS.startGuarded (defaultOpts { port = port }) spec
                  { guards: guards configuration
                  , handlers: SH.handlers { configuration, pool, session: { userId: Nothing } }
                  }
      EC.log $ "HTTP now up on http://localhost:" <> show port
