module Server.Email (sendEmail, Email(..)) where

import Prelude

import Effect.Class (liftEffect)
import Node.ChildProcess as NC
import Node.ChildProcess (defaultSpawnOptions)
import Server.Effect (ServerEffect)

data Email = Feedback | Report | Reset

sendEmail ∷ Int -> Int -> Email → ServerEffect Unit
sendEmail userId recordId email = void <<< liftEffect $ NC.spawn "sprout-out" [show userId, show recordId, show emailOption] defaultSpawnOptions
    where emailOption = case email of
            Feedback -> 1
            Report -> 2
            Reset -> 3
