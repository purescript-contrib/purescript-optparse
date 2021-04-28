let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210409/packages.dhall sha256:e81c2f2ce790c0e0d79869d22f7a37d16caeb5bd81cfda71d46c58f6199fd33f
      with open-memoize =
          { dependencies =
              [ "console"
              , "effect"
              , "psci-support"
              , "strings"
              , "lists"
              , "either"
              , "integers"
              , "lazy"
              , "maybe"
              , "partial"
              , "prelude"
              , "tuples"
              ]
          , repo =
              "https://github.com/purescript-open-community/purescript-open-memoize.git"
          , version =
              "v6.1.0"
          }

in  upstream
