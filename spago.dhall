{ name = "uri"
, dependencies =
  [ "aff"
  , "arrays"
  , "assert"
  , "integers"
  , "js-uri"
  , "numbers"
  , "parsing"
  , "prelude"
  , "profunctor-lenses"
  , "quickcheck"
  , "these"
  , "transformers"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
