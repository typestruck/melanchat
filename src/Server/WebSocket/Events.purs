module Server.WebSocket.Events where

import Prelude

import Browser.Cookies.Internal as BCI
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DDT
import Data.Either (Either(..))
import Data.Enum as DEN
import Data.Foldable as DF
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.Time.Duration (Hours)
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Debug (spy)
import Droplet.Driver (Pool)
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EC
import Effect.Exception (Error)
import Effect.Now as EN
import Effect.Ref (Ref)
import Effect.Ref as ER
import Foreign as F
import Foreign.Object as FO
import Node.HTTP (Request)
import Node.HTTP as NH
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Cookies (cookieName)
import Server.Database.KarmaLeaderboard as SIKL
import Server.Database.Privileges as SIP
import Server.Database.Users as SBU
import Server.Effect (BaseEffect, BaseReader, Configuration)
import Server.Effect as SE
import Server.Im.Action as SIA
import Server.Im.Database as SID
import Server.Settings.Action as SSA
import Server.Token as ST
import Server.WebSocket (CloseCode, CloseReason, WebSocketConnection, WebSocketMessage(..))
import Server.WebSocket as SW
import Shared.Availability (Availability(..))
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SDT
import Shared.Im.Types (AfterLogout(..), FullWebSocketPayloadClient(..), MessageError(..), MessageStatus, OutgoingRecord, WebSocketPayloadClient(..), WebSocketPayloadServer(..))
import Shared.Json as SJ
import Shared.Resource (updateHash)
import Shared.ResponseError (DatabaseError, ResponseError(..))
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))
import Simple.JSON (class WriteForeign)
import Simple.JSON as SJS

-- | Keep each users web socket/availability in a ref
type UserAvailability =
      { connections ∷ HashMap String WebSocketConnection
      , lastSeen ∷ DateTime
      , availability ∷ Availability
      }

type WebSocketEffect = BaseEffect WebSocketReader Unit

type WebSocketReader = BaseReader
      ( loggedUserId ∷ Int
      , token ∷ String --use this to tell connections apart
      , configuration ∷ Configuration
      , allUsersAvailabilityRef ∷ Ref (HashMap Int UserAvailability)
      )

type WebSocketReaderLite = BaseReader
      ( allUsersAvailabilityRef ∷ Ref (HashMap Int UserAvailability)
      )

-- | Wrapper so we can serialize dates in a way postgresql understands
newtype DT = DT DateTime

-- | How often should availability be serialized
aliveDelay ∷ Int
aliveDelay = 1000 * 60 * aliveDelayMinutes

aliveDelayMinutes ∷ Int
aliveDelayMinutes = 5

-- | How often do we check for inactive connections
inactiveDelay ∷ Int
inactiveDelay = 1000 * 60 * 60 * inactiveHours

inactiveHours ∷ Int
inactiveHours = 1

handleConnection ∷ Configuration → Pool → Ref (HashMap Int UserAvailability) → WebSocketConnection → Request → Effect Unit
handleConnection configuration pool allUsersAvailabilityRef connection request = EA.launchAff_ do
      maybeUserId ← SE.poolEffect pool do
            userId ← parseUserId
            isIt ← DM.maybe (pure false) SBU.isUserBanned userId
            pure $ if isIt then Nothing else userId
      liftEffect $ case maybeUserId of
            Nothing → do
                  --this can be made more clear for the end user
                  sendWebSocketMessage connection $ CloseConnection LoginPage
                  EC.log "terminated due to auth error"
            Just loggedUserId → do
                  now ← EN.nowDateTime
                  ER.modify_ (DH.alter (upsertUserAvailability now) loggedUserId) allUsersAvailabilityRef
                  SW.onError connection handleError
                  SW.onClose connection (handleClose token loggedUserId allUsersAvailabilityRef)
                  SW.onMessage connection (runMessageHandler loggedUserId)
      where
      token = DM.fromMaybe "" do
            uncooked ← FO.lookup "cookie" $ NH.requestHeaders request
            map (_.value <<< DN.unwrap) <<< DA.find ((cookieName == _) <<< _.key <<< DN.unwrap) $ BCI.bakeCookies uncooked
      parseUserId = ST.userIdFromToken configuration.tokenSecret token

      upsertUserAvailability date =
            case _ of
                  Nothing → Just $ makeUserAvailabity (DH.fromArray [ Tuple token connection ]) (Right token) true date None --could also query the db
                  Just userAvailability → Just $ makeUserAvailabity (DH.insert token connection userAvailability.connections) (Right token) true date userAvailability.availability

      runMessageHandler loggedUserId (WebSocketMessage message) = do
            case SJ.fromJson message of
                  Right payload → do
                        let
                              run =
                                    R.runBaseAff'
                                          <<< RE.catch (\e → reportError payload (checkInternalError e) e)
                                          <<<
                                                RR.runReader { allUsersAvailabilityRef, configuration, pool, loggedUserId, token } $
                                          handleMessage payload
                        EA.launchAff_ <<< EA.catchError run $ reportError payload Nothing
                  Left error → do
                        SW.terminate connection
                        EC.log $ "terminated due to serialization error: " <> error

      reportError ∷ ∀ a b. MonadEffect b ⇒ WebSocketPayloadServer → Maybe DatabaseError → a → b Unit
      reportError origin context _ = sendWebSocketMessage connection <<< Content $ PayloadError { origin, context }

      checkInternalError = case _ of
            InternalError { context } → context
            _ → Nothing

handleError ∷ Error → Effect Unit
handleError = EC.log <<< show

handleClose ∷ String → Int → Ref (HashMap Int UserAvailability) → CloseCode → CloseReason → Effect Unit
handleClose token loggedUserId allUsersAvailabilityRef _ _ = do
      now ← EN.nowDateTime
      ER.modify_ (DH.update (removeConnection now) loggedUserId) allUsersAvailabilityRef
      where
      removeConnection date userAvailability = Just $ makeUserAvailabity userAvailability.connections (Left token) false date userAvailability.availability --this may set an incorrect last seen but it is preferable to saying an user is online when they are not

handleMessage ∷ WebSocketPayloadServer → WebSocketEffect
handleMessage payload = do
      context ← RR.ask
      allUsersAvailability ← liftEffect $ ER.read context.allUsersAvailabilityRef
      case payload of
            Ping ping → sendPong context.token context.loggedUserId context.allUsersAvailabilityRef ping
            OutgoingMessage message → sendOutgoingMessage context.token context.loggedUserId allUsersAvailability message
            ChangeStatus changes → sendStatusChange context.token context.loggedUserId allUsersAvailability changes
            Typing { id } → sendTyping context.loggedUserId allUsersAvailability id
            UpdatePrivileges → sendUpdatedPrivileges context.loggedUserId allUsersAvailability
            UpdateHash → sendUpdatedHash context.loggedUserId allUsersAvailability
            UnavailableFor { id } → sendUnavailability context.loggedUserId allUsersAvailability id
            Ban { id } → sendBan allUsersAvailability id

sendBan ∷ HashMap Int UserAvailability → Int → WebSocketEffect
sendBan allUsersAvailability userId = do
      let userAvailability = DH.lookup userId allUsersAvailability
      SSA.changePrivacySettings userId
            { messageTimestamps: true
            , onlineStatus: true
            , readReceipts: true
            , typingStatus: true
            , profileVisibility: TemporarilyBanned
            }
      withConnections userAvailability send
      where
      send connection = sendWebSocketMessage connection $ CloseConnection Banned

sendUpdatedPrivileges ∷ Int → HashMap Int UserAvailability → WebSocketEffect
sendUpdatedPrivileges loggedUserId allUsersAvailability = do
      karma ← SIKL.fetchKarma loggedUserId
      privileges ← SIP.fetchPrivileges karma
      let userAvailability = DH.lookup loggedUserId allUsersAvailability
      withConnections userAvailability (send { karma, privileges })
      where
      send payload connection = sendWebSocketMessage connection <<< Content $ CurrentPrivileges payload

sendUpdatedHash ∷ Int → HashMap Int UserAvailability → WebSocketEffect
sendUpdatedHash loggedUserId allUsersAvailability = do
      let userAvailability = DH.lookup loggedUserId allUsersAvailability
      withConnections userAvailability send
      where
      send connection = sendWebSocketMessage connection <<< Content $ CurrentHash updateHash

sendTyping ∷ Int → HashMap Int UserAvailability → Int → WebSocketEffect
sendTyping loggedUserId allUsersAvailability userId = do
      let userAvailability = DH.lookup userId allUsersAvailability
      withConnections userAvailability send
      where
      send connection = sendWebSocketMessage connection <<< Content $ ContactTyping { id: loggedUserId }

--pings fulfill three purposes
-- keep the connection alive
-- maintain online status
-- get contacts/suggestions online status
sendPong ∷ String → Int → Ref (HashMap Int UserAvailability) → { isActive ∷ Boolean, statusFor ∷ Array Int } → WebSocketEffect
sendPong token loggedUserId allUsersAvailabilityRef ping = do
      allUsersAvailability ← liftEffect $ ER.read allUsersAvailabilityRef
      let userAvailability = SU.fromJust $ DH.lookup loggedUserId allUsersAvailability
      Tuple users missing ← makeAvailability allUsersAvailability
      liftEffect do
            now ← EN.nowDateTime
            ER.modify_ (DH.insert loggedUserId (makeUserAvailabity userAvailability.connections (Right token) ping.isActive now userAvailability.availability)) allUsersAvailabilityRef
            ER.modify_ (\avl → DH.union avl $ makeMissingAvailability missing) allUsersAvailabilityRef
      sendWebSocketMessage (SU.fromJust $ DH.lookup token userAvailability.connections) $ Pong { status: users }
      where
      makeAvailability avl = do
            let
                  users = map (\id → { id, status: DM.maybe None _.availability $ DH.lookup id avl }) ping.statusFor
                  nones = map _.id $ DA.filter ((None == _) <<< _.status) users
            case DAN.fromArray nones of
                  Nothing → pure $ Tuple users []
                  Just missing → do
                        lastSeens ← DH.fromArrayBy _.who _.date <$> SID.queryLastSeen missing
                        let
                              records = map
                                    ( \r →
                                            { id: r.id
                                            , status: case r.status of
                                                    None → DM.maybe None (LastSeen <<< DateTimeWrapper) $ DH.lookup r.id lastSeens
                                                    _ → r.status
                                            }
                                    )
                                    users
                        pure <<< Tuple records $ DH.toArrayBy Tuple lastSeens

      makeMissingAvailability missing = DH.fromArray $ map newUserAvailability missing
      newUserAvailability (Tuple id date) = Tuple id $ makeUserAvailabity DH.empty (Left token) false date (LastSeen $ DateTimeWrapper date)

sendUnavailability ∷ Int → HashMap Int UserAvailability → Int → WebSocketEffect
sendUnavailability loggedUserId allUsersAvailability userId = do
      let userAvailability = DH.lookup userId allUsersAvailability
      withConnections userAvailability send
      where
      send connection = sendWebSocketMessage connection <<< Content $ ContactUnavailable
            { userId: loggedUserId
            , temporaryMessageId: Nothing
            }

sendStatusChange ∷ String → Int → HashMap Int UserAvailability → { ids ∷ Array (Tuple Int (Array Int)), status ∷ MessageStatus } → WebSocketEffect
sendStatusChange token loggedUserId allUsersAvailability changes = do
      SID.changeStatus loggedUserId changes.status $ DA.concatMap DT.snd changes.ids
      DF.traverse_ sendReceipients changes.ids
      DF.traverse_ sendLoggedUser changes.ids
      where
      send connection messageIds userId =
            sendWebSocketMessage connection <<< Content $ ServerChangedStatus
                  { ids: messageIds
                  , status: changes.status
                  , userId
                  }

      sendReceipients (Tuple userId messageIds) = do
            let userAvailability = DH.lookup userId allUsersAvailability
            withConnections userAvailability $ \connection → send connection messageIds loggedUserId

      --other connections opened by this user need also to be alerted
      sendLoggedUser (Tuple userId messageIds) = do
            let loggedUserConnections = DM.maybe [] (DH.values <<< DH.filterKeys (token /= _) <<< _.connections) $ DH.lookup loggedUserId allUsersAvailability
            DF.traverse_ (\connection → send connection messageIds userId) loggedUserConnections

-- | Send a message or another user or sync a message sent from another connection
sendOutgoingMessage ∷ String → Int → HashMap Int UserAvailability → OutgoingRecord → WebSocketEffect
sendOutgoingMessage token loggedUserId allUsersAvailability outgoing = do
      processed ← SIA.processMessage loggedUserId outgoing.userId outgoing.content
      case processed of
            Right (Tuple messageId content) → do
                  now ← R.liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let receipientUserAvailability = DH.lookup outgoing.userId allUsersAvailability
                  withConnections receipientUserAvailability (sendRecipient messageId content now)

                  let loggedUserConnections = (SU.fromJust loggedUserAvailability).connections
                  let senderConnection = DH.values $ DH.filterKeys (token == _) loggedUserConnections
                  DF.traverse_ (acknowledgeMessage messageId) senderConnection

                  let otherConnections = DH.values $ DH.filterKeys (token /= _) loggedUserConnections
                  DF.traverse_ (sendRecipient messageId content now) otherConnections

                  DM.maybe (pure unit) (SIA.processKarma loggedUserId outgoing.userId) outgoing.turn
            Left UserUnavailable →
                  sendLoggedUser <<< Content $ ContactUnavailable
                        { userId: outgoing.userId
                        , temporaryMessageId: Just outgoing.id
                        }
            Left InvalidMessage →
                  sendLoggedUser <<< Content $ BadMessage
                        { userId: outgoing.userId
                        , temporaryMessageId: Just outgoing.id
                        }
      where
      sendRecipient messageId content date connection =
            sendWebSocketMessage connection <<< Content $ NewIncomingMessage
                  { id: messageId
                  , senderId: loggedUserId
                  , recipientId: outgoing.userId
                  , content
                  , date
                  }

      loggedUserAvailability = DH.lookup loggedUserId allUsersAvailability

      acknowledgeMessage messageId connection = sendWebSocketMessage connection <<< Content $ ServerReceivedMessage
            { previousId: outgoing.id
            , id: messageId
            , userId: outgoing.userId
            }

      sendLoggedUser payload = do
            withConnections loggedUserAvailability $ \connection → sendWebSocketMessage connection payload

makeUserAvailabity ∷ HashMap String WebSocketConnection → Either String String → Boolean → DateTime → Availability → UserAvailability
makeUserAvailabity connections token isActive lastSeen previousAvailability =
      { lastSeen
      , connections:
              case token of
                    Right t → DH.update (Just <<< SW.lastPing lastSeen) t connections -- ping on the socket is used to determine inactive connections
                    Left t → DH.delete t connections
      , availability:
              if isActive then
                    Online
              else if previousAvailability == Online then
                    LastSeen $ DateTimeWrapper lastSeen
              else
                    previousAvailability
      }

-- | Send a json encoded message
sendWebSocketMessage ∷ ∀ b. MonadEffect b ⇒ WebSocketConnection → FullWebSocketPayloadClient → b Unit
sendWebSocketMessage connection = liftEffect <<< SW.sendMessage connection <<< WebSocketMessage <<< SJ.toJson

-- | Connections are dropped after 1 hour of inactivity
terminateInactive ∷ Ref (HashMap Int UserAvailability) → Effect Unit
terminateInactive allUsersAvailabilityRef = do
      now ← EN.nowDateTime
      allUsersAvailability ← ER.read allUsersAvailabilityRef
      DF.traverse_ (check now) $ DH.toArrayBy Tuple allUsersAvailability
      where
      check now (Tuple id userAvailability) = do
            let
                  expiredConnections
                        | hasExpired now userAvailability.lastSeen = userAvailability.connections
                        | otherwise = DH.filter (not hasExpired now <<< SW.getLastPing) userAvailability.connections
            DF.traverse_ SW.terminate $ DH.values expiredConnections
            when (not $ DH.isEmpty expiredConnections) $
                  ER.modify_ (DH.insert id (makeUserAvailabity (DH.difference userAvailability.connections expiredConnections) (Left "") false userAvailability.lastSeen userAvailability.availability)) allUsersAvailabilityRef

      hasExpired now lastSeen = inactiveHours <= DI.floor (DN.unwrap (DDT.diff now lastSeen ∷ Hours))

withConnections ∷ Maybe UserAvailability → (WebSocketConnection → WebSocketEffect) → WebSocketEffect
withConnections userAvailability handler =
      case userAvailability of
            Just ua → DF.traverse_ handler $ DH.values ua.connections
            Nothing → pure unit

-- | Last seen dates are serialized every 5 minutes
-- |
-- | We don't try to be precise, e.g. users with Online availability are ignored
persistLastSeen ∷ WebSocketReaderLite → Effect Unit
persistLastSeen context = do
      allUsersAvailability ← ER.read context.allUsersAvailabilityRef
      when (not $ DH.isEmpty allUsersAvailability) do
            let run = R.runBaseAff' <<< RE.catch (const (pure unit)) <<< RR.runReader context <<< SID.upsertLastSeen <<< SJS.writeJSON <<< DA.catMaybes $ DH.toArrayBy lastSeens allUsersAvailability
            EA.launchAff_ $ EA.catchError run logError
      where
      lastSeens id = case _ of
            { availability: LastSeen (DateTimeWrapper date) } → Just { who: id, date: DT date }
            _ → Nothing

      logError = liftEffect <<< EC.logShow

instance Newtype DT DateTime

instance WriteForeign DT where
      writeImpl (DT (DateTime dt (Time h m s ms))) = F.unsafeToForeign (SDT.formatIsoDate' dt <> "t" <> time <> "+0000")
            where
            time = show (DEN.fromEnum h) <> ":" <> show (DEN.fromEnum m) <> ":" <> show (DEN.fromEnum s) <> "." <> show (DEN.fromEnum ms)
