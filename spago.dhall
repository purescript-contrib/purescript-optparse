{ name = "affjax"
, dependencies =
  [ "prelude"
  , "effect"
  , "exitcodes"
  , "strings"
  , "ordered-collections"
  , "arrays"
  , "console"
  , "memoize"
  , "transformers"
  , "exists"
  , "node-process"
  , "free"
  , "quickcheck"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
