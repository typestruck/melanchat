module Server.InternalHelp.Template where

import Server.Types
import Shared.Types

import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.InternalHelp.Types (DisplayHelpSection(..))
import Shared.InternalHelp.View as SIHV

template ∷ Effect String
template =
      F.preMount (QuerySelector ".internal-help")
            { view: SIHV.view
            , init:
                    { toggleHelp: FAQ
                    }
            }
