{ name = "uri"
, dependencies =
  [ "aff"
  , "arrays"
  , "generics-rep"
  , "integers"
  , "js-uri"
  , "numbers"
  , "parsing"
  , "profunctor-lenses"
  , "quickcheck"
  , "spec"
  , "these"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
