let config = ./spago.dhall
in config
    with dependencies = (config.dependencies # [ "spec", "aff", "quickcheck" ])
    with sources = (config.sources # [ "test/**/*.purs" ])
