module Shared.IM.View.Contacts where

import Prelude
import Shared.Experiments.Types
import Shared.IM.Types

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SD
import Shared.Experiments.Impersonation (impersonations)
import Shared.Experiments.Impersonation as SEI
import Shared.IM.View.Profile (backArrow, nextArrow)
import Shared.IM.View.Retry as SIVR
import Shared.Markdown as SM
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))

-- | Users that have exchanged messages with the current logged in user
contactList ∷ Boolean → IMModel → Html IMMessage
contactList isClientRender { failedRequests, chatting, experimenting, contacts, user: { id: loggedUserId, profileVisibility, messageTimestamps } } =
      case profileVisibility of
            Nobody → HE.div' [ HA.id $ show ContactList, HA.class' "contact-list" ]
            _ →
                  HE.div
                        [ HA.id $ show ContactList
                        , HA.onScroll CheckFetchContacts
                        , HA.class' "contact-list"
                        ]
                        $ retryLoadingNewContact : DA.snoc displayContactList retryLoadingContacts
      where
      -- | Contact list sorting is only done for the dom nodes, model.contacts is left unchanged
      displayContactList
            | DA.null contacts = [ suggestionsCall ]
            | otherwise =
                    DA.mapWithIndex displayContactListEntry <<<
                          DA.sortBy compareLastDate
                          $ DA.filter (not <<< DA.null <<< _.history) contacts -- might want to look into this: before sending a message, we need to run an effect; in this meanwhile history is empty

      displayContactListEntry index { history, user, impersonating, typing } =
            let
                  justIndex = Just index
                  --refactor: a neater way to do experiment that don't litter the code with case of
                  userProfile = case impersonating of
                        Just impersonationID → SU.fromJust $ DH.lookup impersonationID impersonations
                        _ → user
                  numberUnreadMessages = countUnread history
                  lastHistoryEntry = SU.fromJust $ DA.last history
            in
                  HE.div
                        [ HA.class' { contact: true, "chatting-contact": chattingId == Just user.id && impersonatingId == impersonating }
                        , HA.onClick <<< ResumeChat $ Tuple user.id impersonating
                        ]
                        [ HE.div (HA.class' "avatar-contact-list-div")
                                [ HE.img [ HA.class' $ "avatar-contact-list" <> SA.avatarColorClass justIndex, HA.src $ SA.avatarForRecipient justIndex userProfile.avatar ]
                                ]
                        , HE.div [ HA.class' "contact-profile" ]
                                [ HE.span (HA.class' "contact-name") userProfile.name
                                , HE.div' [ HA.class' { "contact-list-last-message": true, hidden: typing }, HA.innerHtml $ SM.parseRestricted lastHistoryEntry.content ]
                                , HE.div [ HA.class' { "contact-list-last-message typing": true, hidden: not typing } ] $ HE.p_ "Typing..."
                                ]
                        , HE.div (HA.class' "contact-options")
                                [ HE.span (HA.class' { duller: true, invisible: not isClientRender || not messageTimestamps || not userProfile.messageTimestamps }) <<< SD.ago $ DN.unwrap lastHistoryEntry.date
                                , HE.div (HA.class' { "unread-messages": true, hidden: numberUnreadMessages == 0 }) <<< HE.span (HA.class' "unread-number") $ show numberUnreadMessages
                                , HE.div (HA.class' { duller: true, hidden: numberUnreadMessages > 0 || lastHistoryEntry.sender == user.id }) $ show lastHistoryEntry.status
                                ]
                        ]

      -- | Since on mobile contact list takes most of the screen, show a welcoming message for new users/impersonations
      suggestionsCall =
            let
                  { welcome, first, second } = case experimenting of
                        Just (Impersonation (Just { name })) → SEI.welcomeMessage name
                        _ → welcomeMessage
            in
                  HE.div (HA.class' "suggestions-call")
                        [ HE.div (HA.onClick $ ToggleInitialScreen false) backArrow
                        , HE.div (HA.class' { "suggestions-call-middle": true, "welcome-impersonation": DM.isJust experimenting })
                                [ HE.div (HA.class' "welcome-suggestions-call") $ welcome
                                , HE.div_ first
                                , HE.div_ second
                                ]
                        , HE.div (HA.onClick $ ToggleInitialScreen false) nextArrow
                        ]

      welcomeMessage =
            { welcome: "Welcome!"
            , first: "Tap on either of the arrows to see "
            , second: "your chat suggestions"
            }

      compareLastDate contact anotherContact = compare (getDate anotherContact.history) (getDate contact.history)

      getDate history = do
            { date: DateTimeWrapper md } ← DA.last history
            pure md

      countUnread = DF.foldl unread 0

      unread total { status, sender } = total + DE.fromEnum (sender /= loggedUserId && status < Read)

      chattingId = do
            index ← chatting
            { user: { id } } ← contacts !! index
            pure id

      impersonatingId = do
            index ← chatting
            { impersonating } ← contacts !! index
            impersonating

      -- | Displayed if loading contact from an incoming message fails
      retryLoadingNewContact = SIVR.retry "Failed to sync contacts. You might have missed messages." CheckMissedEvents failedRequests

      -- | Displayed if loading contact list fails
      retryLoadingContacts = SIVR.retry "Failed to load contacts" (FetchContacts true) failedRequests
