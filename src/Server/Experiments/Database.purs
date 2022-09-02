module Server.Experiments.Database where

import Prelude
import Shared.Experiments.Types
import Server.Database as SD
import Droplet.Language
import Data.Tuple.Nested ((/\))
import Server.Database.Fields
import Server.Types (ServerEffect)
import Server.Database.Experiments

--could also order by popularity
fetchExperiments ∷ ServerEffect (Array ChatExperiment)
fetchExperiments = SD.query $ select (_id /\ _code /\ _name /\ _description) # from experiments # orderBy (_added # desc)
