module Server.Landing.Database where

import Server.Types
import Prelude
import Server.Database as SD
import Shared.Unsafe as SU
import Data.Tuple.Nested ((/\))
import Droplet.Language
import Server.Database.Users
import Server.Database.Fields
import Server.Database.KarmaHistories

--refactor: add support on droplet
createUser ∷ { email ∷ String, name ∷ String, password ∷ String, headline ∷ String, description ∷ String } → ServerEffect Int
createUser user = SD.withTransaction $ \connection → do
      userId ← _.id <<< SU.fromJust <$> (SD.singleWith connection (insert # into users (_name /\ _password /\ _email /\ _headline /\ _description) # values (user.name /\ user.password /\ user.email /\ user.headline /\ user.description) # returning _id))
      SD.executeWith connection $ insert # into karma_histories (_amount /\ _target) # values (5 /\ userId)
      SD.unsafeExecuteWith connection ("insert into karma_leaderboard(ranker, current_karma, gained, position) values (@ranker, 5, 0, ((select count(1) from karma_leaderboard) + 1))") { ranker: userId }
      --use the median score as new user suggestion score so they are not thrown to the bottom of the pile
      SD.unsafeExecuteWith connection ("insert into suggestions(suggested, score) values (@suggested, coalesce((select score from suggestions order by id limit 1 offset ((select count(*) from suggestions) / 2)), 0))") { suggested: userId }
      pure userId