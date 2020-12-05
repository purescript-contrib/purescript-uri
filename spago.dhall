{ name = "uri"
, dependencies =
  [ "aff"
  , "arrays"
  , "generics-rep"
  , "numbers"
  , "integers"
  , "parsing"
  , "profunctor-lenses"
  , "quickcheck"
  , "these"
  , "unfoldable"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
