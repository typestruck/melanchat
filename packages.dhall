{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210826/packages.dhall sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f
  with purescript-droplet = ../purescript-droplet/spago.dhall as Location

let overrides = {=}

let additions =
      { payload =
        { dependencies =
          [ "aff"
          , "affjax"
          , "console"
          , "debug"
          , "effect"
          , "foreign-generic"
          , "node-fs"
          , "node-fs-aff"
          , "node-http"
          , "prelude"
          , "psci-support"
          , "record"
          , "simple-json"
          , "stringutils"
          , "test-unit"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/hoodunit/purescript-payload.git"
        , version = "15b4954e086336fa2840da3d9738d25f7cb02d6d"
        }
      , browser-cookies =
        { dependencies =
          [ "prelude"
          , "console"
          , "effect"
          , "maybe"
          , "foldable-traversable"
          , "strings"
          , "js-date"
          ]
        , repo = "https://github.com/vilu/purescript-browser-cookies.git"
        , version = "v0.0.1"
        }
      , droplet =
        { dependencies =
          [ "aff"
          , "arrays"
          , "bifunctors"
          , "console"
          , "datetime"
          , "debug"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "integers"
          , "bigints"
          , "maybe"
          , "newtype"
          , "nullable"
          , "partial"
          , "prelude"
          , "profunctor"
          , "record"
          , "strings"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/easafe/purescript-droplet.git"
        , version = "dfcc99584b7fad648f10418fa180769027d62dec"
        }
      }

in  upstream // overrides // additions
