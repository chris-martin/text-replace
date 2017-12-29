{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

import Text.Replace

-- base
import           Control.Arrow  ((>>>))
import           Control.Monad  (unless)
import           Data.Foldable  (for_)
import           Data.Semigroup ((<>))
import qualified System.Exit    as Exit
import qualified System.IO      as IO

-- hedgehog
import           Hedgehog     (Property, forAll, property, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen

-- neat-interpolation
import NeatInterpolation (text)

-- test
import           Data.Text (Text)
import qualified Data.Text as Text

main :: IO ()
main = do
  for_ [IO.stdout, IO.stderr] $ \h -> do
    IO.hSetEncoding h IO.utf8
    IO.hSetBuffering h IO.LineBuffering
  success <- Hedgehog.checkParallel $$(Hedgehog.discover)
  unless success Exit.exitFailure

f :: [Replace] -> Text
f = replaceMapFromList >>> makeTrie >>> drawTrie >>> Text.pack

prop_drawTrie :: Property
prop_drawTrie = property $ do

  replacements <- forAll $ Gen.shuffle
    [ Replace "aft"   "1"
    , Replace "after" "2"
    , Replace "apply" "3"
    , Replace "brain" "4"
    , Replace "broke" "5"
    ]

  f replacements === [text|

    a
    |
    +- ft - "1"
    |  |
    |  `- er - "2"
    |
    `- pply - "3"

    br
    |
    +- ain - "4"
    |
    `- oke - "5"

  |] <> "\n"
