module Server.Landing.Template where

import Prelude

import Data.String as DS
import Effect (Effect)
import Environment (production)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (defaultParameters, externalFooter)
import Server.Template as ST
import Shared.Element (ElementId(..))
import Shared.Im.Svg as SIA
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (Bundle(..), Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Routes (routes)

template ∷ Effect String
template = do
      contents ← ST.template defaultParameters
            { content = content
            , javascript = javascript
            , css = css
            , bundled = production
            }
      FRS.render contents
      where
      css
            | production = [ HE.style [ HA.type' "text/css" ] "<% style.css %>" ] --used to inline stylesheets for production
            | otherwise =
                    [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath External Css ]
                    , HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.bundlePath Landing Css ]
                    ]
      javascript =
            [ HE.script' [ HA.type' "text/javascript", HA.src $ SP.bundlePath Landing Js ]
            , HE.script' [ HA.createAttribute "async" "true", HA.src "https://www.google.com/recaptcha/api.js?onload=initCaptchas&render=explicit" ]
            ]
      content =
            [ HE.div (HA.class' "landing")
                    [ HE.div (HA.class' "header")
                            [ HE.a [ HA.href $ routes.landing {}, HA.class' "logo" ] $ HE.img
                                    [ HA.createAttribute "srcset" $ DS.joinWith " " [ SP.mediaPath Logo3Small Png, "180w,", SP.mediaPath Logo Png, "250w,", SP.mediaPath LogoSmall Png, "210w" ]
                                    , HA.createAttribute "sizes" "(max-width: 1365px) 180px, (max-width: 1919px) 210px, 250px"
                                    , HA.src $ SP.mediaPath Logo Png
                                    ]
                            , HE.div [ HA.class' "login-box" ] $ HE.a [ HA.class' "login", HA.href $ routes.login.get {} ] "Login"
                            ]
                    , HE.div (HA.class' "green-area")
                            [ HE.h1 (HA.id "headline") "Friendly. Random. Chat."
                            , HE.h4 (HA.class' "subheadline")
                                    [ HE.text "Create an account and chat right away with"
                                    , HE.i_ " interesting "
                                    , HE.text "people"
                                    ]
                            , HE.div (HA.class' "form-up")
                                    [ HE.div [ HA.id "email-input", HA.class' "input" ]
                                            [ HE.label_ "Email"
                                            , HE.input [ HA.type' "text", HA.id "email", HA.maxlength emailMaxCharacters ]
                                            , HE.span (HA.class' "error-message") "Please enter a valid email"
                                            ]
                                    , HE.div [ HA.id "password-input", HA.class' "input" ]
                                            [ HE.label_ "Password"
                                            , HE.input [ HA.type' "password", HA.maxlength passwordMaxCharacters, HA.autocomplete "new-password", HA.id "password" ]
                                            , HE.span (HA.class' "error-message") $ "Password must be " <> show passwordMinCharacters <> " characters or more"
                                            ]
                                    , HE.div [ HA.class' "input" ]
                                            [ HE.div' [ HA.id $ show CaptchaRegularUser, HA.class' "hidden" ]
                                            , HE.input [ HA.type' "button", HA.value "Create account" ]
                                            , HE.span' [ HA.class' "request-error-message error-message" ]
                                            ]
                                    , HE.div [ HA.id $ show TemporaryUserSignUp ]
                                            [ HE.div' [ HA.id $ show CaptchaTemporaryUser, HA.class' "hidden" ]
                                            , HE.text "I want to chat without an account"
                                            , HE.svg [ HA.class' "svg-arrow", HA.viewBox "0 0 16 16" ]
                                                    [ HE.g [ HA.transform "rotate(180,7.6,8)" ]
                                                            [ HE.line' [ HA.strokeWidth "3px", HA.x1 "15.98", HA.y1 "8", HA.x2 "1.61", HA.y2 "8" ]
                                                            , HE.polygon' [ HA.strokeWidth "1px", HA.points "6.43 2.05 7.42 3.12 2.17 8 7.42 12.88 6.43 13.95 0.03 8 6.43 2.05" ]
                                                            ]
                                                    ]
                                            ]
                                    ]
                            ]
                    , HE.h2_
                            [ HE.text "MelanChat is made with human connection and"
                            , HE.br
                            , HE.text "conversation in mind"
                            ]
                    , HE.div (HA.class' "first-points")
                            [ HE.div (HA.class' "point-column")
                                    [ HE.div (HA.class' "point")
                                            [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Point1 Png ]
                                            , HE.br
                                            , HE.text "No seedy people!"
                                            , HE.br
                                            , HE.text "Talk to folks who want to"
                                            , HE.br
                                            , HE.text "have meaningful conversations"
                                            ]
                                    , HE.div (HA.class' "point")
                                            [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Point4 Png ]
                                            , HE.br
                                            , HE.text "Community driven:"
                                            , HE.br
                                            , HE.text "feature available based on"
                                            , HE.br
                                            , HE.text "how trusted an user is"
                                            ]
                                    ]
                            , HE.div (HA.class' "point-column")
                                    [ HE.div (HA.class' "point")
                                            [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Point2 Png ]
                                            , HE.br
                                            , HE.text "Set the privacy level you"
                                            , HE.br
                                            , HE.text "feel comfortable with."
                                            , HE.br
                                            , HE.text "Safe, anonymous and ad free"
                                            ]
                                    , HE.div (HA.class' "point")
                                            [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Point5 Png ]
                                            , HE.br
                                            , HE.text "Ultra fancy algorithms"
                                            , HE.br
                                            , HE.text "make the chat matches."
                                            , HE.br
                                            , HE.text "Random, but in an interesting way"
                                            ]
                                    ]
                            , HE.div (HA.class' "point-column")
                                    [ HE.div (HA.class' "point")
                                            [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Point3 Png ]
                                            , HE.br
                                            , HE.text "Feeling uninspired?"
                                            , HE.br
                                            , HE.text "Let the app create your profile,"
                                            , HE.br
                                            , HE.text "or suggest what to say"
                                            ]
                                    , HE.div (HA.class' "point")
                                            [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Point6 Png ]
                                            , HE.br
                                            , HE.text "Send text, image & audio messages."
                                            , HE.br
                                            , HE.text "Choose to take part in "
                                            , HE.br
                                            , HE.text "novel chat experiments"
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "second-point")
                            [ HE.div (HA.class' "point")
                                    [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Point7 Png ]
                                    , HE.br
                                    , HE.text "Full of watermelons!"
                                    ]
                            ]
                    , HE.h2_
                            [ HE.text "This is how it works:"
                            ]
                    , HE.div (HA.class' "third-points")
                            [ HE.div (HA.class' "point")
                                    [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Works1 Png ]
                                    , HE.br
                                    , HE.text "New users randomized account needs Karma to access"
                                    , HE.br
                                    , HE.text "features and tools. Karma is earned by making"
                                    , HE.br
                                    , HE.text "great conversations, and being a good user."
                                    , HE.br
                                    , HE.br
                                    , HE.text "Creeps get weeded out; interesting folks get more visibility"
                                    ]
                            , HE.div (HA.class' "point")
                                    [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Works2 Png ]
                                    , HE.br
                                    , HE.text "Whenever you feel like chatting, the system matches"
                                    , HE.br
                                    , HE.text "you with a random user- and vice versa,"
                                    , HE.br
                                    , HE.text "ensuring no one gets ignored."
                                    , HE.br
                                    , HE.br
                                    , HE.text "You decide what to share and who to share it with"
                                    ]
                            , HE.div (HA.class' "point")
                                    [ HE.img [ HA.class' "point-melon", HA.src $ SP.mediaPath Works3 Png ]
                                    , HE.br
                                    , HE.text "Want to take a break, or delete your account forever?"
                                    , HE.br
                                    , HE.text "No worries, your personal data will never be used"
                                    , HE.br
                                    , HE.text "for spam, harvesting or any shady dealings."
                                    , HE.br
                                    , HE.br
                                    , HE.text "Reactivate your account whenever you want"
                                    ]
                            ]
                    , HE.div (HA.class' "second-green-box")
                            [ HE.div (HA.class' "point-2")
                                    [ HE.text "MelanChat helps you talk, interact with people and form real friendships -- "
                                    , HE.br
                                    , HE.text "without fear of the dreaded ASL questions or getting stalked"
                                    ]
                            , HE.div (HA.class' "point-2")
                                    [ HE.text "MelanChat is not affiliated with any Big Tech companies; nor does it track or spy on you."
                                    , HE.br
                                    , HE.text "Neither it is a dating website. Why upload duck-face pictures when you can talk about"
                                    , HE.br
                                    , HE.text "dancing plagues of the 16th century?"
                                    ]
                            , HE.div (HA.class' "point-2")
                                    [ HE.text "Lastly, did I mention that watermelons are everywhere?"
                                    , HE.br
                                    , HE.text "It can't get better (or healthier) than this"
                                    ]
                            , HE.a [ HA.class' "form-up-call", HA.href "#headline" ] $ "... and it is free! Click here to create an account and chat right away #melanchat"
                            ]
                    , externalFooter
                    ]
            ]
