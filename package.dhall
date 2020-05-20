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
        [ "Chromar.Core"
        , "Chromar.RuleQuotes"
        , "Chromar.Fluent"
        , "Chromar.Observables"
        , "Chromar.Multiset"
        , "Chromar.MAttrs"
        , "Chromar.MRuleParser"
        , "Chromar"
        ]
    }
, tests =
    { gdiff =
        { dependencies =
            [ "base", "coloured-petri-nets" ]
        , main =
            "gdiff.hs"
        , source-dirs =
            "models"
        }
    , plant =
        { dependencies =
            [ "base", "coloured-petri-nets" ]
        , main =
            "plant.hs"
        , source-dirs =
            "models"
        }
    , simple =
        { dependencies =
            [ "base", "coloured-petri-nets" ]
        , main =
            "simple.hs"
        , source-dirs =
            "models"
        }
    }
}
