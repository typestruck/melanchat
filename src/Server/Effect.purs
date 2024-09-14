module Server.Effect where

import Droplet.Driver (Pool)
import Run (AFF, Run, EFFECT)
import Prelude
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Shared.ResponseError (ResponseError)
import Type.Row (type (+))
import Effect.Console as EC
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Data.Maybe (Maybe(..))
import Effect.Class(liftEffect)

type Configuration =
      { port ∷ Int
      , captchaSecret ∷ String
      , adminSecret ∷ String
      , tokenSecret ∷ String
      , salt ∷ String
      , databaseHost ∷ Maybe String
      }

type BaseReader extension =
      { pool ∷ Pool
      | extension
      }

type ServerReader = BaseReader
      ( configuration ∷ Configuration
      )

type BaseEffect r a = Run (READER r + EXCEPT ResponseError + AFF + EFFECT + ()) a

type ServerEffect a = BaseEffect ServerReader a

poolEffect pool = R.runBaseAff' <<< RE.catch (\e → liftEffect (EC.logShow e) *> pure Nothing) <<< RR.runReader { pool }