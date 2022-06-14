{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "melanchat"
, license = "AGPL-3.0-or-later"
, repository = "https://github.com/melanchat/melanchat"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "arrays"
  , "bigints"
  , "browser-cookies"
  , "console"
  , "control"
  , "crypto"
  , "datetime"
  , "debug"
  , "droplet"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "flame"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "form-urlencoded"
  , "functions"
  , "http-methods"
  , "integers"
  , "js-date"
  , "js-timers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-http"
  , "node-process"
  , "node-url"
  , "now"
  , "nullable"
  , "numbers"
  , "ordered-collections"
  , "payload"
  , "prelude"
  , "random"
  , "read"
  , "record"
  , "refs"
  , "run"
  , "safe-coerce"
  , "simple-json"
  , "simple-jwt"
  , "spec"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unfoldable"
  , "unordered-collections"
  , "unsafe-coerce"
  , "uuid"
  , "web-dom"
  , "web-events"
  , "web-file"
  , "web-html"
  , "web-socket"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
