module Server.Recover.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields (_active, _id)
import Server.Database.Recoveries (_recoverer, _uuid, recoveries)
import Server.Database.Users (_password, users)
import Server.Effect (BaseEffect, ServerEffect)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Shared.Unsafe as SU

--REFACTOR: consider making the uuid token the primary key
insertRecover ∷ Int → String → ServerEffect Int
insertRecover id token = map (_.id <<< SU.fromJust) $ SD.single $ insert # into recoveries (_uuid /\ _recoverer) # values (token /\ id) # returning _id

--refactor: doesn't need to calculate the date with sql
selectRecoverer ∷ String → ServerEffect (Maybe Int)
selectRecoverer token = do
      result ← SD.unsafeSingle "select recoverer from recoveries where uuid = @token and active = true and created >= (utc_now()) - interval '1 day'" { token } ∷ BaseEffect _ (Maybe { recoverer ∷ Int })
      pure (_.recoverer <$> result)

recoverPassword ∷ String → Int → String → ServerEffect Unit
recoverPassword token id password = SD.withTransaction $ \connection → do
      SD.executeWith connection $ update recoveries # set (_active .=. false) # wher (_uuid .=. token)
      SD.executeWith connection $ update users # set (_password .=. Just password) # wher (_id .=. id)