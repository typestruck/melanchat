module Server.Email (sendEmail, Email(..)) where

import Prelude

import Effect.Uncurried (EffectFn4)
import Effect.Uncurried as EU
import Environment (production)
import Run as R
import Run.Reader as RR
import Server.Effect (ServerEffect)

data Email = Feedback | Report | Reset

sendEmail ∷ Int -> Int -> Email → ServerEffect Unit
sendEmail userId recordId email = pure unit

