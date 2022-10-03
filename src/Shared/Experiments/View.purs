module Shared.Experiments.View where

import Prelude

import Data.Maybe (Maybe(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types
import Shared.Experiments.Impersonation as SEI

view ∷ ChatExperimentModel → Html ChatExperimentMessage
view model@{ experiments, current } = case current of
      Just (Impersonation (Just profile)) →
            --likely to be the same for all experiments
            HE.div (HA.class' "chat-experiments") $ SEI.joined profile
      _ →
            HE.div (HA.class' "chat-experiments") <<<
                  HE.div (HA.class' "all-experiments") $ map toDiv experiments
      where
      toDiv { name, description, code } = HE.div (HA.class' "experiment")
            [ HE.span (HA.class' "experiment-name") name
            , HE.span (HA.class' "duller") description
            , HE.fragment $ extra model code
            ]

extra ∷ ChatExperimentModel → ExperimentData → Html ChatExperimentMessage
extra model = case _ of
      Impersonation _ → SEI.view model