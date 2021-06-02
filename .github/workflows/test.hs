import System.Environment
import System.Process

main =
  getEnv "ghc" >>= \ghc ->
    case ghc of
      "8.2.2"  -> callProcess "cabal" ["test", "all"
                  , "--constraint=containers == 0.5.10.2"
                  , "--constraint=hedgehog == 0.5.3"
                  , "--constraint=neat-interpolation == 0.3.2.1"
                  , "--constraint=optparse-applicative == 0.14.2.0"
                  , "--constraint=parsec == 3.1.13.0"
                  , "--constraint=text == 1.2.2.2"
                  , "--enable-tests"
                  ]
      "8.4.3"  -> callProcess "cabal" ["test", "all"]
      "8.6.1"  -> callProcess "cabal" ["test", "all"]
      "8.8.1"  -> callProcess "cabal" ["test", "all"]
      "8.10.2" -> callProcess "cabal" ["test", "all"]
      "9.0.1"  -> callProcess "cabal" ["test", "all"
                  , "--constraint=containers == 0.6.4.1"
                  , "--constraint=hedgehog == 1.0.5"
                  , "--constraint=neat-interpolation == 0.3.2.6"
                  , "--constraint=optparse-applicative == 0.15.1.0"
                  , "--constraint=parsec == 3.1.14.0"
                  , "--constraint=text == 1.2.4.1"
                  ]
