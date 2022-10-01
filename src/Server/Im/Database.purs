module Server.Im.Database where

import Droplet.Language
import Prelude hiding (not, join)
import Server.Database.Blocks (_blocked, _blocker, blocks)
import Server.Database.Countries (countries)
import Server.Database.Fields (_age, _date, _id, _name, _recipient, _sender, c, completedTutorial, k, l, lu, messageTimestamps, onlineStatus, profileVisibility, readReceipts, tu, typingStatus, u)
import Server.Database.Functions (date_part_age, datetime_part_age, insert_history, utc_now)
import Server.Database.Histories (_first_message_date, _recipient_deleted_to, _sender_deleted_to, histories)
import Server.Database.KarmaHistories (_amount, _target, karma_histories)
import Server.Database.KarmaLeaderboard (_current_karma, _karma, _karmaPosition, _position, _ranker, karma_leaderboard)
import Server.Database.Languages (_languages, languages)
import Server.Database.LanguagesUsers (_language, _speaker, languages_users)
import Server.Database.LastSeen (_who, last_seen)
import Server.Database.Messages (_content, _status, _temporary_id, messages)
import Server.Database.Reports (_comment, _reason, _reported, _reporter, reports)
import Server.Database.Suggestions (_suggested, suggestions)
import Server.Database.Tags (_tags, tags)
import Server.Database.TagsUsers (_creator, _tag, tags_users)
import Server.Database.Users (_avatar, _birthday, _completedTutorial, _country, _description, _email, _gender, _headline, _joined, _messageTimestamps, _onlineStatus, _password, _readReceipts, _temporary, _typingStatus, _visibility, _visibility_last_updated, users)
import Server.Im.Database.Privileges (_feature, _privileges, privileges)
import Server.Types (BaseEffect, ServerEffect)
import Shared.Im.Types (ArrayPrimaryKey(..), MessageStatus(..), Report, TemporaryMessageId)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as DAN
import Data.BigInt as DB
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.Database.Types (Checked(..))
import Server.Im.Database.Flat (FlatContactHistoryMessage, FlatUser, FlatContact)
import Server.Im.Database.PrivilegesUsers (_privilege, _receiver, privileges_users)
import Shared.Options.Page (contactsPerPage, initialMessagesPerPage, messagesPerPage)
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))
import Type.Proxy (Proxy(..))

userPresentationFields =
      (u ... _id # as _id)
            /\ _avatar
            /\ _gender
            /\ (date_part_age ("year" /\ _birthday) # as _age)
            /\ _name
            /\ (_visibility # as profileVisibility)
            /\ (_readReceipts # as readReceipts)
            /\ (_typingStatus # as typingStatus)
            /\ _temporary
            /\ (_onlineStatus # as onlineStatus)
            /\ (_completedTutorial # as completedTutorial)
            /\ (_messageTimestamps # as messageTimestamps)
            /\ (select (array_agg (l ... _name # orderBy (l ... _name)) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)) # orderBy _languages # limit (Proxy ∷ _ 1))
            /\ _joined
            /\ _headline
            /\ _description
            /\ (select _name # from countries # wher (_id .=. u ... _country) # orderBy _id # limit (Proxy ∷ _ 1) # as _country)
            /\ (select (array_agg _feature # as _privileges) # from (((privileges # as l) `join` (privileges_users # as lu)) # on (l ... _id .=. lu ... _privilege .&&. lu ... _receiver .=. u ... _id)) # orderBy _privileges # limit (Proxy ∷ _ 1))
            /\ (select (array_agg (l ... _name # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)) # orderBy _tags # limit (Proxy ∷ _ 1))
            /\ (k ... _current_karma # as _karma)
            /\ (_position # as _karmaPosition)

contactPresentationFields uid = distinct $ (coalesce (_sender /\ uid) # as _chatStarter) /\ (h ... _date # as _lastMessageDate) /\ (datetime_part_age ("day" /\ coalesce (_first_message_date /\ utc_now)) # as _chatAge) /\ userPresentationFields

senderRecipientFilter loggedUserId otherId = wher ((_sender .=. loggedUserId .&&. _recipient .=. otherId) .||. (_sender .=. otherId .&&. _recipient .=. loggedUserId))

usersSource ∷ _
usersSource = join (users # as u) (karma_leaderboard # as k) # on (u ... _id .=. k ... _ranker)

presentUser ∷ Int → ServerEffect (Maybe FlatUser)
presentUser loggedUserId = SD.single $ select userPresentationFields # from usersSource # wher (u ... _id .=. loggedUserId .&&. _visibility .<>. TemporarilyBanned)

suggest ∷ Int → Int → Maybe ArrayPrimaryKey → ServerEffect (Array FlatUser)
suggest loggedUserId skip = case _ of
      Just (ArrayPrimaryKey []) →
            SD.query $ suggestBaseQuery skip baseFilter -- no users to avoid when impersonating
      Just (ArrayPrimaryKey keys) →
            SD.query $ suggestBaseQuery skip (baseFilter .&&. not (in_ (u ... _id) (SU.fromJust $ DAN.fromArray keys))) -- users to avoid when impersonating
      _ →
            SD.query $ suggestBaseQuery skip baseFilter -- default case
      where
      baseFilter = u ... _id .<>. loggedUserId .&&. visibilityFilter .&&. blockedFilter

      visibilityFilter =
            _visibility .=. Everyone .&&. _temporary .=. Checked false .||.
            (_visibility .=. NoTemporaryUsers .||. _temporary .=. Checked true) .&&. (exists $ select (1 # as u) # from users # wher (_id .=. loggedUserId .&&. _temporary .=. Checked false .&&. _visibility .=. Everyone)) .||.
            _temporary .=. Checked true .&&. (exists $ select (1 # as u) # from users # wher (_id .=. loggedUserId .&&. _temporary .=. Checked true))

      blockedFilter = not (exists $ select (1 # as u) # from blocks # wher (_blocker .=. loggedUserId .&&. _blocked .=. u ... _id .||. _blocker .=. u ... _id .&&. _blocked .=. loggedUserId))

-- top level to avoid monomorphic filter
suggestBaseQuery skip filter =
      select star
            # from
                    ( select userPresentationFields
                            # from (join usersSource (suggestions # as s) # on (u ... _id .=. _suggested))
                            # wher filter
                            # orderBy (s ... _id)
                            # limit (Proxy ∷ Proxy 10)
                            # offset skip
                            # as t
                    )
            # orderBy random

presentUserContactFields ∷ String
presentUserContactFields =
      """ h.sender "chatStarter"
      , h.recipient
      , h.sender_deleted_to
      , h.recipient_deleted_to
      , h.last_message_date "lastMessageDate"
      , date_part_age ('day', COALESCE(first_message_date, utc_now())) "chatAge"
      , u.id
      , avatar
      , gender
      , temporary
      , joined
      , completed_tutorial "completedTutorial"
      , date_part_age ('year', birthday) age
      , name
      , visibility "profileVisibility"
      , read_receipts "readReceipts"
      , typing_status "typingStatus"
      , array[]::integer[] as "privileges"
      , online_status "onlineStatus"
      , message_timestamps "messageTimestamps"
      , headline
      , description
      , (SELECT name FROM countries WHERE id = u.country) country
      , (SELECT ARRAY_AGG(l.name ORDER BY name) FROM languages l JOIN languages_users lu ON l.id = lu.language AND lu.speaker = u.id) languages
      , (SELECT ARRAY_AGG(t.name ORDER BY t.id) FROM tags t JOIN tags_users tu ON t.id = tu.tag AND tu.creator = u.id) tags
      , k.current_karma karma
      , position "karmaPosition"
"""

presentMessageContactFields ∷ String
presentMessageContactFields =
      """
      , s.id as "messageId"
      , s.sender
      , s.recipient
      , s.date
      , s.content
      , s.status """

presentContactFields ∷ String
presentContactFields = presentUserContactFields <> presentMessageContactFields

presentContacts ∷ Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentContacts loggedUserId skip = presentNContacts loggedUserId contactsPerPage skip

presentNContacts ∷ Int → Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentNContacts loggedUserId n skip = SD.unsafeQuery query
      { loggedUserId
      , status: Read
      , initialMessages: initialMessagesPerPage
      , contacts: Contacts
      , limit: n
      , offset: skip
      }
      where
      --refactor: paginate over deleted chats in a cleaner way
      query =
            "SELECT * FROM (SELECT" <> presentUserContactFields
                  <>
                        """FROM
      users u
      JOIN karma_leaderboard k ON u.id = k.ranker
      JOIN histories h ON u.id = sender AND recipient = @loggedUserId OR u.id = recipient AND sender = @loggedUserId
      WHERE visibility <= @contacts
            AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
            AND (h.sender = @loggedUserId AND (h.sender_deleted_to IS NULL OR EXISTS(SELECT 1 FROM messages WHERE id > h.sender_deleted_to AND (sender = @loggedUserId AND recipient = h.recipient OR sender = h.recipient AND recipient = @loggedUserId))) OR h.recipient = @loggedUserId AND (h.recipient_deleted_to IS NULL OR EXISTS(SELECT 1 FROM messages WHERE id > h.recipient_deleted_to AND (recipient = @loggedUserId AND sender = h.sender OR recipient = h.sender AND recipient = @loggedUserId))))
      ORDER BY last_message_date DESC LIMIT @limit OFFSET @offset) uh
      , LATERAL (SELECT *
                 FROM (SELECT
                              ROW_NUMBER() OVER (ORDER BY date DESC) n"""
                  <> presentMessageContactFields
                  <>
                        """FROM messages s
                       WHERE (s.sender = uh."chatStarter" AND s.recipient = uh.recipient OR
                              s.sender = uh.recipient AND s.recipient = uh."chatStarter") AND
                              NOT (uh."chatStarter" = @loggedUserId AND uh.sender_deleted_to IS NOT NULL AND s.id <= uh.sender_deleted_to OR
                                   uh.recipient = @loggedUserId AND uh.recipient_deleted_to IS NOT NULL AND s.id <= uh.recipient_deleted_to)
                       ORDER BY date DESC) b
                 WHERE status < @status OR n <= @initialMessages
                 ORDER BY date) s"""

--only for impersonations, we will fix this someday
presentContactOnly :: Int -> Int -> ServerEffect (Array FlatContact)
presentContactOnly loggedUserId userId = SD.unsafeQuery query
      { loggedUserId
      , userId
      , contacts: Contacts
      }
      where query = "SELECT" <> presentUserContactFields <>
                  """FROM users u
            JOIN karma_leaderboard k ON u.id = k.ranker
            JOIN (select @userId::integer sender, @loggedUserId::integer recipient, null sender_deleted_to, null recipient_deleted_to, utc_now() last_message_date, utc_now() first_message_date) h ON true
      WHERE visibility <= @contacts
            AND u.id = @userId
            AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
      """

--refactor: this can use droplet
presentSingleContact ∷ Int → Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentSingleContact loggedUserId userId offset = SD.unsafeQuery query
      { loggedUserId
      , userId
      , contacts: Contacts
      , messagesPerPage
      , offset
      }
      where
      query = "SELECT * FROM (SELECT" <> presentContactFields <>
            """FROM users u
      JOIN karma_leaderboard k ON u.id = k.ranker
      JOIN histories h ON u.id = h.sender AND h.recipient = @loggedUserId OR u.id = h.recipient AND h.sender = @loggedUserId
      JOIN messages s ON s.sender = h.sender AND s.recipient = h.recipient OR s.sender = h.recipient AND s.recipient = h.sender
WHERE visibility <= @contacts
      AND u.id = @userId
      AND NOT (h.sender = @loggedUserId AND h.sender_deleted_to IS NOT NULL AND s.id <= h.sender_deleted_to OR
               h.recipient = @loggedUserId AND h.recipient_deleted_to IS NOT NULL AND s.id <= h.recipient_deleted_to)
      AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
ORDER BY s.date DESC
LIMIT @messagesPerPage
OFFSET @offset) m ORDER BY m.date"""

--refactor: this can use droplet
presentMissedContacts ∷ Int → Int → ServerEffect (Array FlatContactHistoryMessage)
presentMissedContacts loggedUserId lastId = SD.unsafeQuery query
      { loggedUserId
      , status: Delivered
      , contacts: Contacts
      , lastId
      }
      where
      query = "SELECT" <> presentContactFields <>
            """FROM users u
      JOIN karma_leaderboard k ON u.id = k.ranker
      JOIN histories h ON u.id = h.sender AND h.recipient = @loggedUserId OR u.id = h.recipient AND h.sender = @loggedUserId
      JOIN messages s ON s.sender = h.sender OR s.sender = h.recipient
WHERE visibility <= @contacts
      AND NOT EXISTS (SELECT 1 FROM blocks WHERE blocker = h.recipient AND blocked = h.sender OR blocker = h.sender AND blocked = h.recipient)
      AND s.status < @status
      AND s.recipient = @loggedUserId
      AND s.id > @lastId
ORDER BY "lastMessageDate" DESC, s.sender, s.date"""

messageIdsFor ∷ Int → Int → ServerEffect (Array TemporaryMessageId)
messageIdsFor loggedUserId messageId = SD.query $ select (_id /\ (_temporary_id # as (Proxy ∷ _ "temporaryId"))) # from messages # wher (_sender .=. loggedUserId .&&. _id .>. messageId)

countChats ∷ Int → ServerEffect Int
countChats loggedUserId = map (DM.maybe 0 (SU.fromJust <<< DB.toInt <<< _.t)) $ SD.single $ select (count _id # as t) # from histories # wher (_sender .=. loggedUserId .||. _recipient .=. loggedUserId)

isRecipientVisible ∷ ∀ r. Int → Int → BaseEffect { pool ∷ Pool | r } Boolean
isRecipientVisible loggedUserId userId =
      map DM.isJust <<< SD.single $
            select (1 # as c)
                  # from (leftJoin (users # as u) (histories # as h) # on (_sender .=. loggedUserId .&&. _recipient .=. userId .||. _sender .=. userId .&&. _recipient .=. loggedUserId))
                  # wher (u ... _id .=. userId .&&. not (exists $ select (1 # as c) # from blocks # wher (_blocked .=. loggedUserId .&&. _blocker .=. userId)) .&&. (u ... _visibility .=. Everyone .||. u ... _visibility .=. NoTemporaryUsers .&&. exists (select (3 # as c) # from users # wher (_id .=. loggedUserId .&&. _temporary .=. Checked false)) .||. u ... _visibility .=. Contacts .&&. (isNotNull _first_message_date .&&. _visibility_last_updated .>=. _first_message_date)))

insertMessage ∷ ∀ r. Int → Int → Int → String → BaseEffect { pool ∷ Pool | r } Int
insertMessage loggedUserId recipient temporaryId content = SD.withTransaction $ \connection → do
      void $ SD.singleWith connection $ select (insert_history (loggedUserId /\ recipient) # as u)
      _.id <<< SU.fromJust <$> (SD.singleWith connection $ insert # into messages (_sender /\ _recipient /\ _temporary_id /\ _content) # values (loggedUserId /\ recipient /\ temporaryId /\ content) # returning _id)

insertKarma ∷ ∀ r. Int → Int → Tuple Int Int → BaseEffect { pool ∷ Pool | r } Unit
insertKarma loggedUserId otherID (Tuple senderKarma recipientKarma) =
      void <<< SD.execute $ insert # into karma_histories (_amount /\ _target) # values
            [ senderKarma /\ loggedUserId
            , recipientKarma /\ otherID
            ]

changeStatus ∷ ∀ r. Int → MessageStatus → Array Int → BaseEffect { pool ∷ Pool | r } Unit
changeStatus loggedUserId status = case _ of
      [] → pure unit
      ids → SD.execute $ update messages # set (_status .=. status) # wher (_recipient .=. loggedUserId .&&. (_id `in_` (SU.fromJust $ DAN.fromArray ids)))

insertBlock ∷ Int → Int → ServerEffect Unit
insertBlock loggedUserId blocked = SD.execute $ blockQuery loggedUserId blocked

markAsDeleted ∷ Boolean → Int → { userId ∷ Int, messageId ∷ Int } → _
markAsDeleted isSender loggedUserId { userId, messageId }
      | isSender = SD.execute $ update histories # set (_sender_deleted_to .=. Just messageId) # senderRecipientFilter loggedUserId userId
      | otherwise = SD.execute $ update histories # set (_recipient_deleted_to .=. Just messageId) # senderRecipientFilter loggedUserId userId

blockQuery ∷ Int → Int → _
blockQuery blocker blocked = insert # into blocks (_blocker /\ _blocked) # values (blocker /\ blocked)

insertReport ∷ Int → Report → ServerEffect Unit
insertReport loggedUserId { userId, comment, reason } = SD.withTransaction $ \connection → do
      SD.executeWith connection $ blockQuery loggedUserId userId
      SD.executeWith connection $ insert # into reports (_reporter /\ _reported /\ _reason /\ _comment) # values (loggedUserId /\ userId /\ reason /\ comment)

updateTutorialCompleted ∷ Int → ServerEffect Unit
updateTutorialCompleted loggedUserId = SD.execute $ update users # set (_completedTutorial .=. Checked true) # wher (_id .=. loggedUserId)

chatHistoryEntry ∷ Int → Int → _
chatHistoryEntry loggedUserId otherId = SD.single $ select (_sender /\ _recipient) # from histories # senderRecipientFilter loggedUserId otherId

registerUser ∷ Int → String -> String -> ServerEffect Unit
registerUser loggedUserId email password = SD.execute $ update users # set ((_email .=. Just email) /\ (_password .=. Just password) /\ (_temporary .=. Checked false)) # wher (_id .=. loggedUserId)

upsertLastSeen ∷ ∀ r. String → BaseEffect { pool ∷ Pool | r } Unit
upsertLastSeen jsonInput = void $ SD.unsafeExecute "INSERT INTO last_seen(who, date) (SELECT * FROM jsonb_to_recordset(@jsonInput::jsonb) AS y (who integer, date timestamptz)) ON CONFLICT (who) DO UPDATE SET date = excluded.date" { jsonInput }

queryLastSeen ∷ NonEmptyArray Int → _
queryLastSeen ids = SD.query $ select (_who /\ _date) # from last_seen # wher (_who `in_` ids)

_chatStarter ∷ Proxy "chatStarter"
_chatStarter = Proxy

_chatAge ∷ Proxy "chatAge"
_chatAge = Proxy

h ∷ Proxy "h"
h = Proxy

s ∷ Proxy "s"
s = Proxy

t ∷ Proxy "t"
t = Proxy

_lastMessageDate ∷ Proxy "lastMessageDate"
_lastMessageDate = Proxy