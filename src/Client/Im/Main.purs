module Client.Im.Main where

import Debug
import Prelude
import Shared.Availability
import Shared.Im.Types
import Shared.User

import Client.Common.Dom as CCD
import Client.Common.File as CCF
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Network as CCNT
import Client.Common.Network as CNN
import Client.Im.Chat as CIC
import Client.Im.Contacts as CICN
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.Flame as CIF
import Client.Im.History as CIH
import Client.Im.Notification as CIN
import Client.Im.Notification as CIUC
import Client.Im.Pwa as CIP
import Client.Im.Scroll as CISM
import Client.Im.SmallScreen as CISS
import Client.Im.Suggestion as CIS
import Client.Im.UserMenu as CIU
import Client.Im.WebSocket as CIW
import Client.Im.WebSocket.Events as CIWE
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Symbol as DST
import Data.Symbol as TDS
import Data.Time.Duration (Days(..), Milliseconds(..))
import Data.Traversable as DT
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Now as EN
import Effect.Ref as ER
import Effect.Timer as ET
import Effect.Unsafe as EU
import Flame (ListUpdate, QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Flame.Subscription.Document as FSD
import Flame.Subscription.Window as FSW
import Safe.Coerce as SC
import Shared.Element (ElementId(..))
import Shared.Experiments.Types as SET
import Shared.Im.View as SIV
import Shared.Network (RequestStatus(..))
import Shared.Options.MountPoint (experimentsId, imId, profileId)
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Profile.Types as SPT
import Shared.ResponseError (DatabaseError(..))
import Shared.Routes (routes)
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Shared.User as SUR
import Type.Proxy (Proxy(..))
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.Event.Event as WEE
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.File.FileReader as WFR
import Web.HTML as WH
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.HTMLElement as WHHE
import Web.HTML.Window as WHW
import Web.Socket.WebSocket (WebSocket)

main ∷ Effect Unit
main = do
      webSocketRef ← CIWE.startWebSocket
      fileReader ← WFR.fileReader

      --im is server side rendered
      F.resumeMount (QuerySelector $ show Im) imId
            { view: SIV.view true
            , subscribe:
                    [
                      --display settings/profile/etc page menus
                      FSD.onClick' ToggleUserContextMenu
                    ,
                      --focus event has to be on the window as chrome is a whiny baby about document
                      FSW.onFocus UpdateReadCount
                    ]
            , init: [] -- we use subscription instead of init events
            , update: update { fileReader, webSocketRef }
            }

      smallScreen ← CISS.checkSmallScreen
      pwa ← CIP.checkPwa

      when smallScreen CISS.sendSmallScreen
      when (pwa || not smallScreen) CIN.checkNotifications
      when pwa (CIP.registerServiceWorker >>= CIP.subscribePush)

      --disable the back button on desktop/make the back button go back to previous screen on mobile
      CCD.pushState $ routes.im.get {}
      historyChange smallScreen

      --for drag and drop
      CCF.setUpBase64Reader fileReader (SetSelectedImage <<< Just) imId

      --image upload
      input ← CCD.unsafeGetElementById ImageFileInput
      CCF.setUpFileChange (SetSelectedImage <<< Just) input imId

      --harass temporary users on their last day to make an account
      FS.send imId CheckUserExpiration

update ∷ _ → ListUpdate ImModel ImMessage
update st model =
      case _ of
            --chat
            InsertLink → CIC.insertLink model
            ToggleChatModal modal → CIC.toggleModal modal model
            DropFile event → CIC.catchFile st.fileReader event model
            EnterBeforeSendMessage event → CIC.enterBeforeSendMessage event model
            ForceBeforeSendMessage → CIC.forceBeforeSendMessage model
            ResizeChatInput event → CIC.resizeChatInput event model
            BeforeSendMessage content → CIC.beforeSendMessage content model
            SendMessage content date → CIC.sendMessage webSocket content date model
            SetMessageContent cursor content → CIC.setMessage cursor content model
            SetSelectedImage maybeBase64 → CIC.setSelectedImage maybeBase64 model
            Apply markup → CIC.applyMarkup markup model
            SetEmoji event → CIC.setEmoji event model
            ToggleMessageEnter → CIC.toggleMessageEnter model
            FocusCurrentSuggestion → CIC.focusCurrentSuggestion model
            FocusInput elementId → focusInput elementId model
            QuoteMessage message et → CIC.quoteMessage message et model
            CheckTyping text → CIC.checkTyping text (EU.unsafePerformEffect EN.nowDateTime) webSocket model
            NoTyping id → F.noMessages $ CIC.updateTyping id false model
            TypingId id → F.noMessages model { typingIds = DA.snoc model.typingIds $ SC.coerce id }
            --contacts
            ResumeChat id → CICN.resumeChat id model
            UpdateDelivered → CICN.markDelivered webSocket model
            UpdateReadCount → CICN.markRead webSocket model
            CheckFetchContacts → CICN.checkFetchContacts model
            SpecialRequest (FetchContacts shouldFetch) → CICN.fetchContacts shouldFetch model
            SpecialRequest (DeleteChat tupleId) → CICN.deleteChat tupleId model
            DisplayContacts contacts → CICN.displayContacts contacts model
            DisplayNewContacts contacts → CICN.displayNewContacts contacts model
            ResumeMissedEvents missed → CICN.resumeMissedEvents missed model
            --history
            CheckFetchHistory → CIH.checkFetchHistory model
            SpecialRequest (FetchHistory shouldFetch) → CIH.fetchHistory shouldFetch model
            DisplayHistory overwrite history → CIH.displayHistory overwrite history model
            --suggestion
            FetchMoreSuggestions → CIS.fetchMoreSuggestions model
            ResumeSuggesting → CIS.resumeSuggesting model
            ToggleContactProfile → CIS.toggleContactProfile model
            SpecialRequest PreviousSuggestion → CIS.previousSuggestion model
            SpecialRequest NextSuggestion → CIS.nextSuggestion model
            SpecialRequest (BlockUser id) → CIS.blockUser webSocket id model
            DisplayMoreSuggestions suggestions → CIS.displayMoreSuggestions suggestions model
            --user menu
            ToggleInitialScreen toggle → CIU.toggleInitialScreen toggle model
            Logout after → CIU.logout after model
            ToggleUserContextMenu event → toggleUserContextMenu event model
            SpecialRequest (ToggleModal toggle) → CIU.toggleModal toggle model
            SetModalContents file root html → CIU.setModalContents file root html model
            --main
            SetContextMenuToggle toggle → toggleContextMenu toggle model
            ReloadPage → reloadPage model
            ReceiveMessage payload isFocused → receiveMessage webSocket isFocused payload model
            SetNameFromProfile name → setName name model
            SetAvatarFromProfile base64 → setAvatar base64 model
            AskNotification → askNotification model
            ToggleAskNotification → toggleAskNotification model
            CreateUserFromTemporary → registerUser model
            PreventStop event → preventStop event model
            CheckUserExpiration → checkUserExpiration model
            FinishTutorial → finishTutorial model
            ToggleConnected isConnected → CIWE.toggleConnectedWebSocket isConnected model
            TerminateTemporaryUser → terminateAccount model
            SpecialRequest CheckMissedEvents → checkMissedEvents model
            SetField setter → F.noMessages $ setter model
            ToggleFortune isVisible → toggleFortune isVisible model
            DisplayFortune sequence → displayFortune sequence model
            RequestFailed failure → addFailure failure model
            SpecialRequest (ReportUser userId) → report userId webSocket model
            SetSmallScreen → CISS.setSmallScreen model
            PollPrivileges → CIWE.pollPrivileges webSocket model
            SendPing isActive → CIWE.sendPing webSocket isActive model
            SetRegistered → setRegistered model
            SetPrivacySettings ps → setPrivacySettings ps model
            DisplayAvailability availability → displayAvailability availability model
      where
      { webSocket } = EU.unsafePerformEffect $ ER.read st.webSocketRef -- u n s a f e

toggleContextMenu ∷ ShowContextMenu → ImModel → NoMessages
toggleContextMenu toggle model = F.noMessages model { toggleContextMenu = toggle }

displayAvailability ∷ AvailabilityStatus → ImModel → NoMessages
displayAvailability avl model@{ contacts, suggestions } = F.noMessages $ model
      { contacts = map updateContact contacts
      , suggestions = map updateUser suggestions
      }
      where
      availability = DH.fromArray $ map (\{ id, status } → Tuple id status) avl
      updateContact contact@{ user: { id } } = case DH.lookup id availability of
            Just status → contact { user { availability = status } }
            Nothing → contact
      updateUser user@{ id } = case DH.lookup id availability of
            Just status → user { availability = status }
            Nothing → user

setRegistered ∷ ImModel → NoMessages
setRegistered model = model { user { temporary = false } } /\
      [ do
              liftEffect $ FS.send profileId SPT.AfterRegistration
              pure <<< Just <<< SpecialRequest $ ToggleModal ShowProfile
      ]

registerUser ∷ ImModel → MoreMessages
registerUser model@{ temporaryEmail, temporaryPassword, erroredFields } =
      if invalidEmail then
            F.noMessages $ model { erroredFields = DA.snoc erroredFields $ DST.reflectSymbol (Proxy ∷ _ "temporaryEmail") }
      else if invalidPassword then
            F.noMessages $ model { erroredFields = DA.snoc erroredFields $ DST.reflectSymbol (Proxy ∷ _ "temporaryPassword") }
      else
            model { erroredFields = [] } /\
                  [ do
                          status ← CCN.formRequest (show TemporaryUserSignUpForm) $ request.im.register { body: { email: SU.fromJust temporaryEmail, password: SU.fromJust temporaryPassword } }
                          case status of
                                Failure _ → pure Nothing
                                Success → pure $ Just SetRegistered
                  ]
      where
      invalidEmail = DM.maybe true (\email → DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email)) temporaryEmail
      invalidPassword = DM.maybe true (\password → DS.length password < passwordMinCharacters) temporaryPassword

terminateAccount ∷ ImModel → NextMessage
terminateAccount model = model /\
      [ do
              status ← CNN.formRequest (show ConfirmAccountTerminationForm) $ request.settings.account.terminate { body: {} }
              when (status == Success) $ do
                    EA.delay $ Milliseconds 3000.0
                    liftEffect <<< CCL.setLocation $ routes.login.get {}
              pure Nothing
      ]

checkUserExpiration ∷ ImModel → MoreMessages
checkUserExpiration model@{ user: { temporary, joined } }
      | temporary && SUR.temporaryUserExpiration joined <= Days 1.0 = model /\ [ pure <<< Just <<< SpecialRequest $ ToggleModal ShowProfile ]
      | otherwise = F.noMessages model

setPrivacySettings ∷ PrivacySettings → ImModel → NextMessage
setPrivacySettings { readReceipts, typingStatus, profileVisibility, onlineStatus, messageTimestamps } model =
      model
            { user
                    { profileVisibility = profileVisibility
                    , readReceipts = readReceipts
                    , typingStatus = typingStatus
                    , onlineStatus = onlineStatus
                    , messageTimestamps = messageTimestamps
                    }
            } /\ [ pure $ Just FetchMoreSuggestions ]

finishTutorial ∷ ImModel → NextMessage
finishTutorial model@{ toggleModal } = model { user { completedTutorial = true } } /\ [ finish, greet ]
      where
      sender = 4
      finish = do
            void <<< CCNT.silentResponse $ request.im.tutorial {}
            case toggleModal of
                  Tutorial _ → pure <<< Just <<< SpecialRequest $ ToggleModal HideUserMenuModal
                  _ → pure Nothing
      greet = do
            void <<< CCNT.silentResponse $ request.im.greeting {}
            contact ← CCNT.silentResponse $ request.im.contact { query: { id: sender } }
            pure <<< Just $ DisplayNewContacts contact

report ∷ Int → WebSocket → ImModel → MoreMessages
report userId webSocket model@{ reportReason, reportComment } = case reportReason of
      Just rs →
            CIS.updateAfterBlock userId
                  ( model
                          { reportReason = Nothing
                          , reportComment = Nothing
                          }
                  ) /\
                  [ do
                          result ← CCN.defaultResponse $ request.im.report { body: { userId, reason: rs, comment: reportComment } }
                          case result of
                                Left _ → pure <<< Just $ RequestFailed { request: ReportUser userId, errorMessage: Nothing }
                                _ → do
                                      liftEffect <<< CIW.sendPayload webSocket $ UnavailableFor { id: userId }
                                      pure Nothing
                  ]
      Nothing → F.noMessages $ model
            { erroredFields = [ TDS.reflectSymbol (Proxy ∷ Proxy "reportReason") ]
            }

reloadPage ∷ ImModel → NextMessage
reloadPage model =  model /\ [liftEffect CCL.reload *> pure Nothing]

askNotification ∷ ImModel → MoreMessages
askNotification model =  model { enableNotificationsVisible = false } /\ [liftEffect CCD.requestNotificationPermission *> pure Nothing]

--refactor: all messages like this can be dryed into a single function
toggleAskNotification ∷ ImModel → NoMessages
toggleAskNotification model@{ enableNotificationsVisible } = F.noMessages $ model
      { enableNotificationsVisible = not enableNotificationsVisible
      }

toggleUserContextMenu ∷ Event → ImModel → MoreMessages
toggleUserContextMenu event model@{ toggleContextMenu }
      | toggleContextMenu /= HideContextMenu =
              F.noMessages $ model { toggleContextMenu = HideContextMenu }
      | otherwise =
              model /\
                    [
                      --we cant use node.contains as some of the elements are dynamically created/destroyed
                      liftEffect do
                            let
                                  element = SU.fromJust $ do
                                        target ← WEE.target event
                                        WDE.fromEventTarget target
                            id ← WDE.id element
                            parent ← WDN.parentElement $ WDE.toNode element
                            parentId ← case parent of
                                  Just e → WDE.id e
                                  Nothing → pure ""
                            pure <<< Just <<< SetContextMenuToggle $ toggle id parentId
                    ]
              where
              toggle elementId parentId
                    | elementId == show UserContextMenu || parentId == show UserContextMenu = ShowUserContextMenu
                    | elementId == show SuggestionContextMenu || parentId == show SuggestionContextMenu = ShowSuggestionContextMenu
                    | elementId == show CompactProfileContextMenu || parentId == show CompactProfileContextMenu = ShowCompactProfileContextMenu
                    | elementId == show FullProfileContextMenu || parentId == show FullProfileContextMenu = ShowFullProfileContextMenu
                    | otherwise = HideContextMenu

focusInput ∷ ElementId → ImModel → NextMessage
focusInput elementId model = model /\
      [ liftEffect do
              element ← CCD.getElementById elementId
              WHHE.focus $ SU.fromJust do
                    e ← element
                    WHHE.fromElement e
              pure Nothing
      ]

addFailure ∷ RequestFailure → ImModel → NoMessages
addFailure failure@{ request } model@{ failedRequests, errorMessage } = F.noMessages $ model
      { failedRequests = failure : failedRequests
      , errorMessage = case request of
              BlockUser _ → "Could not block user. Please try again"
              ReportUser _ → "Could not report user. Please try again"
              PreviousSuggestion → suggestionsError
              NextSuggestion → suggestionsError
              _ → errorMessage
      }
      where
      suggestionsError = "Could not fetch suggestions. Please try again"

toggleFortune ∷ Boolean → ImModel → MoreMessages
toggleFortune isVisible model
      | isVisible = model /\ [ Just <<< DisplayFortune <$> CCNT.silentResponse (request.im.fortune {}) ]
      | otherwise = F.noMessages $ model
              { fortune = Nothing
              }

displayFortune ∷ String → ImModel → NoMessages
displayFortune sequence model = F.noMessages $ model
      { fortune = Just sequence
      }

receiveMessage ∷ WebSocket → Boolean → WebSocketPayloadClient → ImModel → MoreMessages
receiveMessage webSocket isFocused payload model = case payload of
      CurrentPrivileges kp → receivePrivileges kp model
      CurrentHash newHash → receiveHash newHash model
      ContactTyping tp → receiveTyping tp model
      ServerReceivedMessage rm → receiveAcknowledgement rm model
      ServerChangedStatus cs → receiveStatusChange cs model
      ContactUnavailable cu → receiveUnavailable cu model
      BadMessage bm → receiveBadMessage bm model
      NewIncomingMessage ni → receiveIncomingMessage webSocket isFocused ni model
      PayloadError p → receivePayloadError p model

receivePayloadError received model = case received.origin of
      OutgoingMessage { id, userId } → F.noMessages $ model
            { contacts =
                    --assume that it is because the other user no longer exists
                    if received.context == Just MissingForeignKey then
                          markContactUnavailable model.contacts userId
                    else
                          markErroredMessage model.contacts userId id
            }
      _ → F.noMessages model

receiveIncomingMessage webSocket isFocused received model
      | DA.elem received.recipientId model.blockedUsers = F.noMessages model
      | otherwise =
              let
                    model' = unsuggest received.recipientId model
              in
                    case processIncomingMessage received model' of
                          Left userId → model' /\ [ CCNT.retryableResponse CheckMissedEvents DisplayNewContacts $ request.im.contact { query: { id: received.recipientId } } ]
                          --mark it as read if we received a message from the current chat
                          -- or as delivered otherwise
                          Right
                                updatedModel@
                                      { chatting: Just index
                                      , contacts
                                      } | isFocused && isChatting received.recipientId updatedModel →
                                let
                                      Tuple furtherUpdatedModel messages = CICN.updateStatus updatedModel
                                            { sessionUserId: model.user.id
                                            , contacts
                                            , newStatus: Read
                                            , webSocket
                                            , index
                                            }
                                in
                                      furtherUpdatedModel /\ (CISM.scrollLastMessage' : messages)
                          Right
                                updatedModel@
                                      { contacts
                                      } →
                                let
                                      Tuple furtherUpdatedModel messages = CICN.updateStatus updatedModel
                                            { index: SU.fromJust $ DA.findIndex (findContact received.recipientId) contacts
                                            , sessionUserId: model.user.id
                                            , newStatus: Delivered
                                            , contacts
                                            , webSocket
                                            }
                                in
                                      furtherUpdatedModel /\ (CIUC.notify' furtherUpdatedModel [ received.recipientId ] : messages)
              where
              isChatting senderId { contacts, chatting } =
                    let
                          { user: { id: recipientId } } = contacts !@ SU.fromJust chatting
                    in
                          recipientId == senderId

receiveBadMessage received model = F.noMessages model
      { contacts = case received.temporaryMessageId of
              Nothing → model.contacts
              Just id → markErroredMessage model.contacts received.userId id
      }

receiveUnavailable received model =
      F.noMessages $ unsuggest received.userId model
            { contacts = case received.temporaryMessageId of
                    Nothing → updatedContacts
                    Just id → markErroredMessage updatedContacts received.userId id
            }
      where
      updatedContacts = markContactUnavailable model.contacts received.userId

receiveStatusChange received model = F.noMessages model
      { contacts = updateStatus model.contacts received.userId received.ids received.status
      }

receiveAcknowledgement received model = F.noMessages model
      { contacts = updateTemporaryId model.contacts received.userId received.previousId received.id
      }

receiveTyping received model = CIC.updateTyping received.id true model /\
      [ liftEffect do
              DT.traverse_ (ET.clearTimeout <<< SC.coerce) model.typingIds
              newId ← ET.setTimeout 1000 <<< FS.send imId $ NoTyping received.id
              pure <<< Just $ TypingId newId
      ]

receiveHash newHash model = F.noMessages model
      { imUpdated = newHash /= model.hash
      }

receivePrivileges received model =
      model
            { user
                    { karma = received.karma
                    , privileges = received.privileges
                    }
            } /\
            [ do
                    liftEffect do
                          FS.send profileId $ SPT.UpdatePrivileges received
                          FS.send experimentsId $ SET.UpdatePrivileges received
                    pure Nothing
            ]

unsuggest ∷ Int → ImModel → ImModel
unsuggest userId model = model
      { suggestions = updatedSuggestions
      , suggesting = updatedSuggesting
      }
      where
      unsuggestedIndex = DA.findIndex ((userId == _) <<< _.id) model.suggestions
      updatedSuggestions = DM.fromMaybe model.suggestions do
            i ← unsuggestedIndex
            DA.deleteAt i model.suggestions
      updatedSuggesting
            | unsuggestedIndex /= Nothing && unsuggestedIndex < model.suggesting = (max 0 <<< (_ - 1)) <$> model.suggesting
            | otherwise = model.suggesting

processIncomingMessage ∷ ClientMessagePayload → ImModel → Either Int ImModel
processIncomingMessage payload model = case findAndUpdateContactList of
      Just contacts →
            Right model
                  { contacts = contacts
                  }
      Nothing → Left payload.recipientId
      where
      updateHistory contact =
            contact
                  { lastMessageDate = payload.date
                  , history = DA.snoc contact.history $
                          { status: Received
                          , sender: payload.senderId
                          , recipient: payload.recipientId
                          , id: payload.id
                          , content: payload.content
                          , date: payload.date
                          }
                  }

      findAndUpdateContactList = do
            index ← DA.findIndex (findContact payload.recipientId) model.contacts
            DA.modifyAt index updateHistory model.contacts

findContact ∷ Int → Contact → Boolean
findContact userId cnt = userId == cnt.user.id

updateTemporaryId ∷ Array Contact → Int → Int → Int → Array Contact
updateTemporaryId contacts userId previousMessageID messageId = updateContactHistory contacts userId updateTemporary
      where
      updateTemporary history@({ id })
            | id == previousMessageID = history { id = messageId, status = Received }
            | otherwise = history

updateStatus ∷ Array Contact → Int → Array Int → MessageStatus → Array Contact
updateStatus contacts userId ids status = updateContactHistory contacts userId updateSt
      where
      updateSt history@({ id })
            | DA.elem id ids = history { status = status }
            | otherwise = history

markErroredMessage ∷ Array Contact → Int → Int → Array Contact
markErroredMessage contacts userId messageId = updateContactHistory contacts userId updateStatus
      where
      updateStatus history@{ id }
            | messageId == id = history { status = Errored }
            | otherwise = history

--refactor: should be abstract with updateReadCount
updateContactHistory ∷ Array Contact → Int → (HistoryMessage → HistoryMessage) → Array Contact
updateContactHistory contacts userId f = updateContact <$> contacts
      where
      updateContact contact@{ user: { id }, history }
            | id == userId = contact
                    { history = f <$> history
                    }
            | otherwise = contact

markContactUnavailable ∷ Array Contact → Int → Array Contact
markContactUnavailable contacts userId = updateContact <$> contacts
      where
      updateContact contact@{ user: { id } }
            | id == userId = contact
                    { user
                            { availability = Unavailable
                            }
                    }
            | otherwise = contact

checkMissedEvents ∷ ImModel → MoreMessages
checkMissedEvents model =
      model /\
            [ do
                    let { lastSentMessageId, lastReceivedMessageId } = findLastMessages model.contacts model.user.id

                    if DM.isNothing lastSentMessageId && DM.isNothing lastReceivedMessageId then
                          pure Nothing
                    else
                          CCNT.retryableResponse CheckMissedEvents ResumeMissedEvents (request.im.missedEvents { query: { lastSenderId: lastSentMessageId, lastRecipientId: lastReceivedMessageId } })
            ]

findLastMessages ∷ Array Contact → Int → { lastSentMessageId ∷ Maybe Int, lastReceivedMessageId ∷ Maybe Int }
findLastMessages contacts sessionUserID =
      { lastSentMessageId: findLast (\h → sessionUserID == h.sender && h.status == Received)
      , lastReceivedMessageId: findLast ((sessionUserID /= _) <<< _.sender)
      }
      where
      findLast f = do
            index ← DA.findLastIndex f allHistories
            { id } ← allHistories !! index
            pure id

      allHistories = DA.sortBy byID $ DA.concatMap _.history contacts
      byID { id } { id: anotherID } = compare id anotherID

setName ∷ String → ImModel → NoMessages
setName name model =
      F.noMessages $ model
            { user
                    { name = name
                    }
            }

setAvatar ∷ Maybe String → ImModel → NoMessages
setAvatar base64 model = F.noMessages $ model
      { user
              { avatar = base64
              }
      }

preventStop ∷ Event → ImModel → NoMessages
preventStop event model = model /\ [liftEffect $ CCD.preventStop event *> pure Nothing]

--refactor use popstate subscription
historyChange ∷ Boolean → Effect Unit
historyChange smallScreen = do
      popStateListener ← WET.eventListener $ const handler
      window ← WH.window
      WET.addEventListener popstate popStateListener false $ WHW.toEventTarget window
      where
      handler = do
            CCD.pushState $ routes.im.get {}
            when smallScreen <<< FS.send imId $ ToggleInitialScreen true

