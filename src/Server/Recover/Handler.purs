module Server.Recover.Handler where

import Prelude
import Server.Types
import Shared.ContentType

import Data.Maybe (Maybe)
import Server.Ok
import Server.Recover.Action as SRA
import Server.Recover.Template as SRT
import Server.Response as SR
import Shared.Account (ResetPassword, RecoverAccount)

recover ∷ ∀ r. { query ∷ { token ∷ Maybe String } | r } → ServerEffect Html
recover { query: { token } } = SR.serveTemplate $ SRT.template token

recoverAccount ∷ ∀ r. { body ∷ RecoverAccount | r } → ServerEffect Ok
recoverAccount { body } = SRA.recover body

reset ∷ ∀ r. { body ∷ ResetPassword | r } → ServerEffect Ok
reset { body } = SRA.reset body