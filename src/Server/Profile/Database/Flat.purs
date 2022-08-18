module Server.Profile.Database.Flat where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Shared.DateTime (DateWrapper(..))
import Shared.Profile.Types (ProfileUser)
import Shared.Avatar as SA
import Shared.User (Availability(..), Gender)
import Server.Database.Flat as SDF

type FlatProfileUser =
      { avatar ∷ Maybe String
      , birthday ∷ Maybe Date
      , country ∷ Maybe Int
      , description ∷ String
      , gender ∷ Maybe Gender
      , headline ∷ String
      , id ∷ Int
      , karma ∷ Int
      , karmaPosition ∷ Int
      , languages ∷ Maybe (Array Int)
      , name ∷ String
      , tags ∷ Maybe String
      }

fromFlatProfileUser ∷ FlatProfileUser → ProfileUser
fromFlatProfileUser fu =
      { avatar: SA.parseAvatar fu.avatar
      , age: DateWrapper <$> fu.birthday
      , country: fu.country
      , description: fu.description
      , gender: fu.gender
      , headline: fu.headline
      , availability: None
      , id: fu.id
      , karma: fu.karma
      , karmaPosition: fu.karmaPosition
      , languages: DM.fromMaybe [] fu.languages
      , name: fu.name
      , tags: SDF.splitAgg "\\n" fu.tags
      }
