module Server.Faq where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

faq ∷ ∀ m. Html m
faq =
      HE.div (HA.class' "terms")
            [ HE.ul (HA.class' "bulleted no-padding")
                    [ HE.li (HA.class' "no-padding") $ HE.a (HA.href "#whatsmerochat") "What is MeroChat?"
                    , HE.li_ $ HE.a (HA.href "#canifilter") "Can I filter suggestions by gender/location/etc?"
                    , HE.li_ $ HE.a (HA.href "#dating") "Is MeroChat a dating/hookup app?"
                    , HE.li_ $ HE.a (HA.href "#isitfree") "Is MeroChat free?"
                    , HE.li_ $ HE.a (HA.href "#whatskarma") "What is karma?"
                    , HE.li_ $ HE.a (HA.href "#gibberishprofile") "What is that gibberish on my profile?"
                    , HE.li_ $ HE.a (HA.href "#profileprivate") "Is my profile private?"
                    , HE.li_ $ HE.a (HA.href "#groupchats") "Are group chats available?"
                    , HE.li_ $ HE.a (HA.href "#chatexperiments") "What are chat experiments?"
                    , HE.li_ $ HE.a (HA.href "#mero") "What does Mero in MeroChat stand for?"
                    , HE.li_ $ HE.a (HA.href "#canihelp") "Wow, I love it. How can I help?"
                    ]
            , HE.h2 [ HA.id "whatsmerochat" ] "What is MeroChat?"
            , HE.p_ "MeroChat is a random chat. That means the app suggests you new people to talk to. You may choose to fill in your profile and voila! Friends"
            , HE.h2 [ HA.id "canifilter" ] "Can I filter suggestions by gender/location/etc?"
            , HE.p_ "You may skip suggestions, but it is not possible to filter them in any way. MeroChat tries its best to give you quality people to talk to, but the fun is in discovering"
            , HE.h2 [ HA.id "dating" ] "Is MeroChat a dating/hookup app?"
            , HE.p_ "No. MeroChat is for friendly conversations only, there is already plenty of other places for try getting laid"
            , HE.h2 [ HA.id "isitfree" ] "Is MeroChat free?"
            , HE.p_ "Yes! The app is totally free and there is no ads. MeroChat runs on donations from people who like it enough (or might just be after the rewards, who can tell)"
            , HE.h2 [ HA.id "whatskarma" ] "What is karma?"
            , HE.p_ "Think of it as a score of how much other users trust you. You gain karma points by starting good conversations. As a moderation tool, some privileges (like sending pictures) are locked until you have enough karma"
            , HE.h2 [ HA.id "gibberishprofile" ] "What is that gibberish on my profile?"
            , HE.p_ "The app automatically fills in some fields (like name, headline or bio) for a new profile. Sometimes it is amusing, sometimes just corny. You may leave as it is (or also generate some new gibberish!) or edit it to your liking"
            , HE.h2 [ HA.id "profileprivate" ] "Is my profile private?"
            , HE.p_ "Your profile can only be viewed by other users inside of the app, limited by your privacy settings. There is absolutely no need to share personal details on your profile -- you are in control of what to share or not"
            , HE.h2 [ HA.id "groupchats" ] "Are group chats available?"
            , HE.p_ "No, MeroChat is for one on one, private conversations"
            , HE.h2 [ HA.id "chatexperiments" ] "What are chat experiments?"
            , HE.p_ "These are gimmicks/games/events for novel chatting. For example: chat in character as a historical figure, find someone to debate your latest hot take, send messages in paper planes, etc. Chat experiments are optional to join and users are randomly invited to join them"
            , HE.h2 [ HA.id "mero" ] "What does Mero in MeroChat stand for?"
            , HE.p_ "Until someone finds a backronym, it is the Guarani word for (water)melons. Not surprisingly, the app is full of them"
            , HE.h2_ "Recommend me some music"
            , HE.p_
                    [ HE.text "Listen to "
                    , HE.a [ HA.href "https://open.spotify.com/track/4klGcqccAwciiLlPL136Kl?si=kYRoPuV6S2utxNCT4j2h_Q", HA.target "_blank" ] "Metá Metá"
                    ]
            , HE.h2 [ HA.id "canihelp" ] "Wow, I love it. How can I help?"
            , HE.p_
                    [ HE.text "Right?! If you can spare some, consider backing MeroChat. The entire "
                    , HE.a [ HA.href "https://github.com/typestruck/merochat", HA.target "_blank" ] "source code"
                    , HE.text " is also open source in case this is your thing. That being said, just letting others know about MeroChat is already a huge help. Reporting bugs, bad user behavior or other issues is also highly appreciated"
                    ]
            ]