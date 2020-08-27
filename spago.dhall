{ name = "uri"
, dependencies =
  [ "arrays"
  , "generics-rep"
  , "globals"
  , "integers"
  , "parsing"
  , "profunctor-lenses"
  , "these"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
