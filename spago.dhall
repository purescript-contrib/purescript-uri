{ name = "uri"
, dependencies =
  [ "aff"
  , "arrays"
  , "assert"
  , "generics-rep"
  , "integers"
  , "js-uri"
  , "numbers"
  , "parsing"
  , "profunctor-lenses"
  , "quickcheck"
  , "these"
  , "transformers"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
