module Server.Landing.Template where

import Prelude

import Data.Either (Either(..))
import Data.String as DS
import Effect (Effect)
import Environment (production)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (defaultParameters, externalFooter)
import Server.Template as ST
import Shared.Element (ElementId(..))
import Shared.Landing.Svg as SLS
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
                            [ HE.div (HA.class' "big-logo")
                                    [ SLS.logo

                                    ]
                            , HE.div (HA.class' "menu-merochat")
                                    [ HE.div (HA.class' "menu")
                                            [ HE.a_ "Home"
                                            , HE.a_ "FAQ"
                                            , HE.a_ "Support us"
                                            , HE.a [ HA.class' "login-link" ] "Login"
                                            , HE.div (HA.class' "theme-switcher")
                                                    [ SLS.sun
                                                    , SLS.moon
                                                    ]
                                            ]
                                    , HE.div (HA.class' "merochat")
                                            [ HE.h1 (HA.class' "name") "MeroChat"
                                            , HE.div (HA.class' "tagline") "Random chat without the sleaze"
                                            , HE.div (HA.class' "subtagline") "(Friendly only!)"
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "sign-up")
                            [ HE.div (HA.class' "blurb")
                                    [ HE.text "Feeling chatty? In search of new friends? Bored?"
                                    , HE.br
                                    , HE.text " MeroChat connects you with random people who"
                                    , HE.br
                                    , HE.text "are here just for chatting. 1 on 1, text only,"
                                    , HE.br
                                    , HE.text "old school random chat!"
                                    ]
                            , HE.div (HA.class' "sign-up-form")
                                    [ HE.input [ HA.type' "text", HA.placeholder "Email" ]
                                    , HE.input [ HA.type' "text", HA.placeholder "Password" ]
                                    , HE.input [ HA.type' "button", HA.value "Create account" ]
                                    , HE.a_ "Continue as guest"
                                    ]
                            ]
                    , HE.div (HA.class' "features")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Safe and friendly"
                                            , HE.span_ "Strict moderation + a Karma system = no weirdos, just nice chats. (Not a dating app, btw!)"
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature1
                                            ]
                                    ]
                            , HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Privacy first"
                                            , HE.span_ "No spam, no AI training, no selling your soul. Delete or take a break anytime."
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature2
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "features")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Anonymously you"
                                            , HE.span_ "Share as much (or as little) as you want. No ads, no pressure."
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature3
                                            ]
                                    ]
                            , HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Human only"
                                            , HE.span_ "No bots, AI or spammers"
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature4
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "features")
                            [ HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Smart matching"
                                            , HE.span_ "Our ultra-fancy algorithms pair you with people in an interesting way."
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature5
                                            ]
                                    ]
                            , HE.div (HA.class' "feature-blurb")
                                    [ HE.div (HA.class' "feature-blurb-left")
                                            [ HE.h1_ "Text only but"
                                            , HE.span_ "Send images, audio, and try experimental chat features."
                                            ]
                                    , HE.div (HA.class' "feature-blurb-right")
                                            [ SLS.feature6
                                            ]
                                    ]
                            ]
                    , HE.div (HA.class' "sign-up-again")
                            [ HE.div (HA.class' "logo-name")
                                    [ SLS.invertedLogo
                                    , HE.h1_ "MeroChat"
                                    , HE.span_ "Friendly Random Chat"
                                    ]
                            , HE.a_ "Try it out"
                            ]
                    , HE.div (HA.class' "features-again")
                            [ HE.div_
                                    [ SLS.feature7
                                    ]
                            , HE.div_
                                    [ HE.h1_ "MeroChat lets you chat, connect, and maybe even make a real friend — without the dreaded “ASL?” interrogation or accidental stalkers."
                                    , HE.span_ "No Big Tech snooping here! We don’t track, spy, or sell your secrets. Also, not a dating site — so forget the duck-face selfies and let’s talk about the real important stuff… like the dancing plagues of the 16th century. 💃🕺"
                                    ]
                            ]
                    , HE.div (HA.class' "footer")
                            [ HE.a_ "Terms and conditions"
                            , HE.a_ "Privacy policy"
                            , HE.a_ "Source code"
                            , HE.a_ "Support us"
                            ]
                    ]
            ]
