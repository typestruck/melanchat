module Shared.Newtype where

import Prelude
import Shared.ContentType

import Data.Newtype (class Newtype)
import Data.Newtype as DN

unwrapAll ∷ ∀ f g v w. Functor f ⇒ Functor g ⇒ Newtype w v ⇒ f (g w) → f (g v)
unwrapAll = map (map DN.unwrap)