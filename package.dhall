{ name =
    "coloured-petri-nets"
, version =
    "0.1.0.0"
, synopsis =
    "none"
, homepage =
    "none"
, author =
    "none"
, maintainer =
    "none"
, other-extensions =
    "BangPatterns"
, dependencies =
    [ "base", "containers" ]
, ghc-options =
    [ "-Wall"
    , "-Werror"
    , "-Wincomplete-uni-patterns"
    , "-Wcompat"
    , "-Widentities"
    , "-Wredundant-constraints"
    , "-fhide-source-paths"
    ]
, library =
    { source-dirs =
        "src"
    , dependencies =
        [ "base"
        , "random"
        , "parsec"
        , "template-haskell"
        , "haskell-src-meta"
        , "containers"
        , "multiset"
        ]
    , exposed-modules =
        "Chromar"
    , when =
        [ { condition = "impl(ghc >= 8.10.0)", source-dirs = "src-ghc-8.10" }
        , { condition =
              "impl(ghc >= 8.8.0) && impl(ghc < 8.10.0)"
          , source-dirs =
              "src-ghc-8.8"
          }
        , { condition =
              "impl(ghc >= 8.6.0) && impl(ghc < 8.8.0)"
          , source-dirs =
              "src-ghc-8.6"
          }
        ]
    }
, tests =
    { gdiff =
        { dependencies =
            [ "base", "coloured-petri-nets" ]
        , other-modules =
            [] : List Text
        , main =
            "gdiff.hs"
        , source-dirs =
            "models"
        }
    , market =
        { dependencies =
            [ "base", "coloured-petri-nets", "random", "normaldistribution" ]
        , other-modules =
            [] : List Text
        , main =
            "Market.hs"
        , source-dirs =
            "models/market"
        }
    , plant =
        { dependencies =
            [ "base", "coloured-petri-nets" ]
        , other-modules =
            [] : List Text
        , main =
            "plant.hs"
        , source-dirs =
            "models"
        }
    , simple =
        { dependencies =
            [ "base", "coloured-petri-nets" ]
        , other-modules =
            [] : List Text
        , main =
            "simple.hs"
        , source-dirs =
            "models"
        }
    , germ =
        { dependencies =
            [ "base"
            , "coloured-petri-nets"
            , "text"
            , "random"
            , "normaldistribution"
            ]
        , other-modules =
            [ "SeedsModel.Env", "SeedsModel.Germ" ]
        , main =
            "germ.hs"
        , source-dirs =
            "models"
        }
    , utils =
        { dependencies =
            [ "base"
            , "coloured-petri-nets"
            , "text"
            , "random"
            , "normaldistribution"
            ]
        , other-modules =
            [ "SeedsModel.Env", "SeedsModel.Utils" ]
        , main =
            "utils.hs"
        , source-dirs =
            "models"
        }
    }
}
