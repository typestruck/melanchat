--missing tests
module Server.Recover.Action where

import Prelude
import Server.Effect

import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.UUID as DU
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Run as R
import Server.AccountValidation as SA
import Server.Captcha as SC
import Server.Database.Users (By(..))
import Server.Database.Users as SDU
import Server.Email (Email(..))
import Server.Email as SE
import Server.Recover.Database as SRD
import Server.Response as SR
import Server.Token as ST
import Shared.Account (ResetPassword, RecoverAccount)
import Shared.Options.Domain (domain)
import Shared.Routes (routes)

accountNotFound ∷ String
accountNotFound = "Could not find an account with this email"

invalidRecovery ∷ String
invalidRecovery = "Invalid recovery link"

recover ∷ RecoverAccount → ServerEffect Unit
recover rec = do
      SC.validateCaptcha rec.captchaResponse
      email ← SA.validateEmail rec.email
      record ← SDU.userBy $ Email email
      case record of
            Nothing → SR.throwBadRequest accountNotFound
            Just user → do
                  token ← R.liftEffect (DU.toString <$> DU.genUUID)
                  id <- SRD.insertRecover user.id token
                  SE.sendEmail user.id id Reset

reset ∷ ResetPassword → ServerEffect Unit
reset { token, password } = do
      void $ SA.validatePassword password
      maybeId ← SRD.selectRecoverer token
      case maybeId of
            Nothing → SR.throwBadRequest invalidRecovery
            Just id → do
                  hashedPassword ← ST.hashPassword password
                  SRD.recoverPassword token id hashedPassword
