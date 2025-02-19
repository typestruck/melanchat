module Server.Settings.Handler where

import Prelude
import Server.Effect

import Payload.ResponseTypes (Response)
import Run as R
import Server.Logout as SL
import Server.Ok (Ok, ok)
import Server.Settings.Action as SSA
import Server.Settings.Database as SSD
import Server.Settings.Template as SST
import Shared.Settings.Types (PrivacySettings)

settings ∷ { guards ∷ { loggedUserId ∷ Int } } → ServerEffect String
settings { guards: { loggedUserId } } = do
      privacySettings ← SSD.privacySettings loggedUserId
      R.liftEffect $ SST.template privacySettings

accountEmail ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { email ∷ String } } → ServerEffect (Response Ok)
accountEmail { guards: { loggedUserId }, body: { email } } = do
      SSA.changeEmail loggedUserId email
      pure SL.expireCookies

accountPassword ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ { password ∷ String } } → ServerEffect (Response Ok)
accountPassword { guards: { loggedUserId }, body: { password } } = do
      SSA.changePassword loggedUserId password
      pure SL.expireCookies

accountTerminate ∷ ∀ r. { guards ∷ { loggedUserId ∷ Int } | r } → ServerEffect (Response Ok)
accountTerminate { guards: { loggedUserId } } = do
      SSA.terminateAccount loggedUserId
      pure SL.expireCookies

changePrivacy ∷ { guards ∷ { loggedUserId ∷ Int }, body ∷ PrivacySettings } → ServerEffect Ok
changePrivacy { guards: { loggedUserId }, body } = do
      SSA.changePrivacySettings loggedUserId body
      pure ok