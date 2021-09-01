module Server.IM.Database where

import Droplet.Language
import Prelude hiding (join, not)
import Server.Database.Blocks
import Server.Database.Countries
import Server.Database.Fields
import Server.Database.Functions
import Server.Database.Histories
import Server.Database.KarmaLeaderboard
import Server.Database.Languages
import Server.Database.LanguagesUsers
import Server.Database.Messages
import Server.Database.Reports
import Server.Database.Tags
import Server.Database.TagsUsers
import Server.Database.Users
import Server.Types
import Shared.IM.Types
import Shared.Types

import Data.Array as DA
import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe(..))
import Data.String.Common as DS
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Droplet.Driver (Pool)
import Server.Database as SD
import Server.IM.Flat (FlatUser, FlatContact)
import Shared.Options.Page (contactsPerPage, initialMessagesPerPage, messagesPerPage, suggestionsPerPage)
import Shared.Unsafe as SU
import Shared.User (IU)
import Type.Proxy (Proxy(..))

_chatStarter :: Proxy "chatStarter"
_chatStarter = Proxy

_chatAge :: Proxy "chatAge"
_chatAge = Proxy

_karmaPosition :: Proxy "karmaPosition"
_karmaPosition = Proxy

userPresentationFields :: String
userPresentationFields =
      """ u.id,
avatar,
gender,
date_part('year', age(now() at time zone 'utc', birthday)) as age,
name,
headline,
description,
(select name from countries where id = country) country,
(select string_agg(l.name, ','  order by name) from languages l join languages_users lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by l.id) from tags l join tags_users tu on l.id = tu.tag and tu.creator = u.id ) tags,
k.current_karma karma,
k.position """

userPresentationFields2 =
      (u ... _id # as _id)
            /\ _avatar
            /\ _gender
            /\ (date_part_age ("year" /\ _birthday) # as _age)
            /\ _name
            /\ _headline
            /\ _description
            /\ (select _name # from countries # wher (_id .=. u ... _country) # as _country)
            /\ (select (string_agg (l ... _name) (", " # orderBy _name) # as _languages) # from (((languages # as l) `join` (languages_users # as lu)) # on (l ... _id .=. lu ... _language .&&. lu ... _speaker .=. u ... _id)))
            /\ (select (string_agg _name ("\n" # orderBy (l ... _id)) # as _tags) # from (((tags # as l) `join` (tags_users # as tu)) # on (l ... _id .=. tu ... _tag .&&. tu ... _creator .=. u ... _id)))
            /\ (k ... _current_karma # as _karma)
            /\ (_position # as _karmaPosition)

contactPresentationFields = distinct $ (_sender # as _chatStarter) /\ (datetime_part_age ("day" /\ _first_message_date) # as _chatAge) /\ userPresentationFields2

contactsSource :: Int -> _
contactsSource loggedUserID = join usersSource (histories # as h) # on (u ... _id .=. h ... _sender .&&. h ... _recipient .=. loggedUserID .||. u ... _id .=. h ... _recipient .&&. h ... _sender .=. loggedUserID)

usersTable :: String
usersTable = " users u join karma_leaderboard k on u.id = k.ranker "

usersSource :: _
usersSource = join (users # as u) (karma_leaderboard # as k) # on (u ... _id .=. k ... _ranker)

presentUser :: Int -> ServerEffect (Maybe FlatUser)
presentUser loggedUserID = SD.single $ select userPresentationFields2 # from usersSource # wher (u ... _id .=. loggedUserID .&&. _active .=. true)

--refactor: improve this
suggest :: Int -> Int -> Maybe ArrayPrimaryKey -> ServerEffect (Array IMUser)
suggest loggedUserID skip = case _ of
      Just (ArrayPrimaryKey []) ->
            SD.unsafeQuery (select <> rest) { id: loggedUserID, page: suggestionsPerPage, skip }
      Just (ArrayPrimaryKey keys) ->
            SD.unsafeQuery (select <> "and not (u.id = any(@keys))" <> rest) { id: loggedUserID, page: suggestionsPerPage, skip, keys }
      _ ->
            SD.unsafeQuery (select <> "and not exists(select 1 from histories where sender in (@id, u.id) and recipient in (@id, u.id))" <> rest) { id: loggedUserID, page: suggestionsPerPage, skip }
      where
      select = "select * from (select" <> userPresentationFields <> "from" <> usersTable <> "join suggestions s on u.id = suggested where u.id <> @id "
      rest = " and u.active and not exists (select 1 from blocks where blocker in (@id, u.id) and blocked in (@id, u.id)) order by s.id limit @page offset @skip) t order by random()"

presentContacts :: Int -> Int -> ServerEffect (Array FlatContact)
presentContacts loggedUserID skip = SD.query $ select contactPresentationFields # from (contactsSource loggedUserID) # wher (not $ exists (select (1 # as u) # from blocks # wher (_blocker .=. h ... _recipient .&&. _blocked .=. h ... _sender .||. _blocker .=. h ... _sender .&&. _blocked .=. h ... _recipient))) # orderBy (h ... _date # desc) # limit contactsPerPage # offset skip

--needs to handle impersonations
presentSingleContact :: Int -> Int -> ServerEffect FlatContact
presentSingleContact loggedUserID otherID = SU.fromJust <$> SD.unsafeSingle
      ( ( "select coalesce(h.date, now() at time zone 'utc'), coalesce(sender, @otherID), coalesce(first_message_date, now() at time zone 'utc'), " <> userPresentationFields
                <> "from"
                <> usersTable
                <> "left join histories h on (u.id = h.recipient and h.sender = @id or u.id = h.sender and h.recipient = @id) where u.id = @otherID"
        )
      )
      { id: loggedUserID, otherID }

presentSelectedContacts :: Int -> Array Int -> ServerEffect (Array FlatContact)
presentSelectedContacts loggedUserID ids
      | DA.null ids = pure []
      | otherwise = SD.query $ select contactPresentationFields # from (contactsSource loggedUserID) # wher (in_ (u ... _id) ids)

--refactor: improve this
chatHistoryFor :: Int -> Array Int -> ServerEffect (Array HistoryMessage)
chatHistoryFor loggedUserID otherIDs
      | DA.null otherIDs = pure []
      | otherwise = SD.unsafeQuery query { sender: loggedUserID, page: initialMessagesPerPage, status: Delivered }
              where
              query = "select * from (" <> DS.joinWith " union all " (select <$> otherIDs) <> ") r order by date, sender, recipient"
              select n =
                    let
                          parameter = show n
                    in
                          "((select" <> messagePresentationFields <> "from messages where sender = @sender and recipient = " <> parameter <> " or sender = " <> parameter <> " and recipient = @sender order by date desc limit @page) union (select" <> messagePresentationFields <> "from messages where recipient = @sender and sender = " <> parameter <> " and status < @status order by date desc))"
              messagePresentationFields = " id, sender, recipient, date, content, status "

chatHistorySince :: Int -> Int -> ServerEffect (Array HistoryMessage)
chatHistorySince loggedUserID lastID = SD.query $ select (_id /\ _sender /\ _recipient /\ _date /\ _content /\ _status) # from messages # wher (_recipient .=. loggedUserID .&&. _id .>. lastID .&&. _status .<. Delivered) # orderBy (_date /\ _sender)

chatHistoryBetween :: Int -> Int -> Int -> ServerEffect (Array HistoryMessage)
chatHistoryBetween loggedUserID otherID skip = SD.query $ select star # from (select (_id /\ _sender /\ _recipient /\ _date /\ _content /\ _status) # from messages # wher (_sender .=. loggedUserID .&&. _recipient .=. otherID .||. _sender .=. otherID .&&. _recipient .=. loggedUserID) # orderBy (_date # desc) # limit messagesPerPage # offset skip # as c) # orderBy _date

messsageIDsFor :: Int -> Int -> ServerEffect (Array MessageIDTemporary)
messsageIDsFor loggedUserID messageID = SD.query $ select (_id /\ (_temporary_id # as (Proxy :: Proxy "temporaryID"))) # from messages # wher (_sender .=. loggedUserID .&&. _id .>. messageID)

insertMessage :: forall r. Int -> Int -> Int -> String -> BaseEffect { pool :: Pool | r } Int
insertMessage loggedUserID recipient temporaryID content = SD.withTransaction $ \connection -> do
      void $ SD.singleWith connection $ select (insert_history (loggedUserID /\ recipient) # as u)
      _.id <<< SU.fromJust <$> (SD.singleWith connection $ insert # into messages (_sender /\ _recipient /\ _temporary_id /\ _content) # values (loggedUserID /\ recipient /\ temporaryID /\ content) # returning _id)

--refactor: add multiple values to droplet to update here
insertKarma :: forall r. Int -> Int -> Tuple Int Int -> BaseEffect { pool :: Pool | r } Unit
insertKarma loggedUserID otherID (Tuple senderKarma recipientKarma) =
      void $ SD.unsafeExecute "insert into karma_histories(amount, target) values (@senderKarma, @senderID), (@recipientKarma, @recipientID)" ({ senderKarma, senderID: loggedUserID, recipientKarma, recipientID: otherID })

changeStatus :: forall r. Int -> MessageStatus -> Array Int -> BaseEffect { pool :: Pool | r } Unit
changeStatus loggedUserID status ids = SD.execute $ update messages # set (_status /\ status) # wher (_recipient .=. loggedUserID .&&. (_id `in_` ids))

insertBlock :: Int -> Int -> ServerEffect Unit
insertBlock loggedUserID blocked = SD.execute $ blockQuery loggedUserID blocked

blockQuery :: Int -> Int -> _
blockQuery blocker blocked = insert # into blocks (_blocker /\ _blocked) # values (blocker /\ blocked)

insertReport :: Int -> Report -> ServerEffect Unit
insertReport loggedUserID { userID, comment, reason } = SD.withTransaction $ \connection -> do
      SD.executeWith connection $ blockQuery loggedUserID userID
      SD.executeWith connection $ insert # into reports (_reporter /\ _reported /\ _reason /\ _comment) # values (loggedUserID /\ userID /\ reason /\ comment)
