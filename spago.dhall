{ name = "liminal"
, license = "BSD-3-Clause"
, repository = "https://github.com/jstoxrocky/purescript-liminal"
, dependencies = 
    [ "aff"
    , "arrays"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "maybe"
    , "numbers"
    , "prelude"
    , "spec"
    , "transformation-matrix"
    , "transformers"
    , "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
