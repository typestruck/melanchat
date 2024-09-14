module Server.Im.Action where

import Debug
import Prelude
import Shared.Im.Types
import Shared.Privilege

import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Nullable as DN
import Data.Set (Set)
import Data.Set as DST
import Data.String as DS
import Data.Tuple (Tuple(..))
import Droplet.Driver (Pool)
import Environment (production)
import Run.Except as RE
import Server.AccountValidation as SA
import Server.Effect (BaseEffect, Configuration, ServerEffect)
import Server.Email (Email(..))
import Server.Email as SE
import Server.File as SF
import Server.Im.Database as SID
import Server.Im.Database.Flat (FlatContactHistoryMessage, fromFlatContact, fromFlatMessage)
import Server.Im.Database.Flat as SIF
import Server.Im.Types (Payload)
import Server.Sanitize as SS
import Server.ThreeK as ST
import Server.Wheel as SW
import Shared.Markdown (Token(..))
import Shared.Markdown as SM
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.ResponseError (ResponseError(..))

im ∷ Int → ServerEffect Payload
im loggedUserId = do
      maybeUser ← SID.presentUser loggedUserId
      case maybeUser of
            --happens if the user has an invalid cookie/was suspended
            Nothing → RE.throw ExpiredSession
            Just user → do
                  suggestions ← suggest loggedUserId 0
                  contacts ← listContacts loggedUserId 0
                  pure
                        { contacts
                        , suggestions
                        , user: SIF.fromFlatUser user
                        }

suggest ∷ Int → Int → ServerEffect (Array Suggestion)
suggest loggedUserId skip = map SIF.fromFlatUser <$> SID.suggest loggedUserId skip

listContacts ∷ Int → Int → ServerEffect (Array Contact)
listContacts loggedUserId skip = presentContacts <$> SID.presentContacts loggedUserId skip

listSingleContact ∷ Int → Int → ServerEffect (Array Contact)
listSingleContact loggedUserId userId = presentContacts <$> SID.presentSingleContact loggedUserId userId 0

resumeChatHistory ∷ Int → Int → Int → ServerEffect (Array HistoryMessage)
resumeChatHistory loggedUserId userId skip = map fromFlatMessage <$> SID.presentSingleContact loggedUserId userId skip

listMissedEvents ∷ Int → Maybe Int → DateTime → ServerEffect MissedEvents
listMissedEvents loggedUserId messageId dt = do
      messages ← SID.presentMissedMessages loggedUserId messageId dt
      pure
            { missedMessages: messages
            }

presentContacts ∷ Array FlatContactHistoryMessage → Array Contact
presentContacts = map chatHistory <<< DA.groupBy sameContact
      where
      sameContact a b = a.id == b.id

      chatHistory records =
            let contact = DAN.head records in (fromFlatContact contact) { history = fromFlatMessage <$> DAN.toArray records }

processMessage ∷ ∀ r. Int → Int → MessageContent → BaseEffect { configuration ∷ Configuration, pool ∷ Pool | r } (Either MessageError (Tuple Int String))
processMessage loggedUserId userId content = do
      isVisible ← SID.isRecipientVisible loggedUserId userId
      if isVisible then do
            privileges ← markdownPrivileges loggedUserId
            sanitized ← processMessageContent content privileges
            if DS.null sanitized then
                  pure $ Left InvalidMessage
            else do
                  id ← SID.insertMessage loggedUserId userId sanitized
                  pure <<< Right $ Tuple id sanitized
      else
            pure $ Left UserUnavailable

markdownPrivileges ∷ ∀ r. Int → BaseEffect { pool ∷ Pool | r } (Set Privilege)
markdownPrivileges loggedUserId = format <$> SID.markdownPrivileges loggedUserId
      where
      format = case _ of
            [] → DST.empty
            [ p ] | p.feature == SendLinks → DST.singleton SendLinks
            [ p ] | p.feature == SendImages → DST.singleton SendImages
            _ → DST.fromFoldable [ SendLinks, SendImages ]

-- | Sanitizes markdown and handle image uploads
processMessageContent ∷ ∀ r. MessageContent → Set Privilege → BaseEffect { configuration ∷ Configuration, pool ∷ Pool | r } String
processMessageContent content privileges = do
      message ← case content of
            Text m | allowed m → pure m
            Image caption base64 | DST.member SendImages privileges → do
                  name ← SF.saveBase64File base64
                  pure $ "![" <> caption <> "](" <> SP.resourcePath (Left $ Upload name) Ignore <> ")"
            _ → pure ""
      pure <<< DS.trim $ SS.sanitize message
      where
      allowed contents = DST.member SendImages privileges || noLinks contents
      noLinks contents =
            let
                  canSendLinks = DST.member SendLinks privileges
            in
                  DM.isNothing <<< DA.find (link canSendLinks) $ SM.lexer contents
      link canSendLinks (Token t) = DM.isJust (DN.toMaybe t.tokens >>= DA.find (isLink canSendLinks))
      isLink canSendLinks (Token child) = child."type" == "image" || not canSendLinks && ((child."type" == "link" || child."type" == "reflink") && child.raw /= child.text)

processKarma ∷ ∀ r. Int → Int → Turn → BaseEffect { pool ∷ Pool | r } Unit
processKarma loggedUserId userId turn = SID.insertKarma loggedUserId userId $ SW.karmaFrom turn

blockUser ∷ Int → Int → ServerEffect Unit
blockUser loggedUserId userId = SID.insertBlock loggedUserId userId

deleteChat ∷ Int → { userId ∷ Int, messageId ∷ Int } → ServerEffect Unit
deleteChat loggedUserId ids@{ userId } = do
      entry ← SID.chatHistoryEntry loggedUserId userId
      case entry of
            Nothing → pure unit
            Just { sender } → SID.markAsDeleted (sender == loggedUserId) loggedUserId ids

reportUser ∷ Int → Report → ServerEffect Unit
reportUser loggedUserId report = do
      id <- SID.insertReport loggedUserId report
      SE.sendEmail loggedUserId id Report

finishTutorial ∷ Int → ServerEffect Unit
finishTutorial loggedUserId = SID.updateTutorialCompleted loggedUserId

--makeshift action so new users have more attention
greet ∷ Int → ServerEffect Unit
greet loggedUserId =
      if production then do
            starter ← ST.generateConversationStarter
            void $ SID.insertMessage sender loggedUserId starter
      else
            pure unit
      where
      sender = 4

registerUser ∷ Int → String → String → ServerEffect Unit
registerUser loggedUserId rawEmail password = do
      email ← SA.validateEmail rawEmail
      hash ← SA.validatePassword password
      SA.validateExistingEmail email
      SID.registerUser loggedUserId email hash
