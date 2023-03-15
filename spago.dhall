{ name = "liminal"
, license = "BSD-3-Clause"
, repository = "https://github.com/jstoxrocky/purescript-liminal"
, dependencies = 
    [ "console"
    , "effect"
    , "prelude"
    , "transformation-matrix"
    , "arrays"
    , "foldable-traversable"
    , "maybe" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
