{ name = "uri"
, dependencies =
  [ "aff"
  , "arrays"
  , "generics-rep"
  , "globals"
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
