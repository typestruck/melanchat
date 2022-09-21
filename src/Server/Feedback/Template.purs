module Server.Feedback.Template where

import Prelude
import Server.Types

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Feedback.View as SFV

template ∷ Effect String
template =
      F.preMount (QuerySelector $ "#" <> show FeedbackRoot)
            { view: SFV.view
            , init:
                    {
                    }
            }
