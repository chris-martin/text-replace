{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

import Text.Replace

-- base
import           Control.Arrow  ((>>>))
import           Control.Monad  (unless)
import           Data.Foldable  (for_)
import qualified System.Exit    as Exit
import qualified System.IO      as IO

-- hedgehog
import           Hedgehog     (Property, forAll, property, withTests, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen

-- neat-interpolation
import qualified NeatInterpolation as N

-- text
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

main :: IO ()
main = do
  for_ [IO.stdout, IO.stderr] $ \h -> do
    IO.hSetEncoding h IO.utf8
    IO.hSetBuffering h IO.LineBuffering
  success <- Hedgehog.checkParallel $$(Hedgehog.discover)
  unless success Exit.exitFailure

prop_replace_1 :: Property
prop_replace_1 = withTests 1 $ property $
  let
    xs = [ Replace "a" "b" ]
  in
    replaceWithList xs "banana" === "bbnbnb"

prop_replace_swap :: Property
prop_replace_swap = withTests 1 $ property $
  let
    xs = [ Replace "a" "b"
         , Replace "b" "a" ]
  in
    replaceWithList xs "banana" === "abnbnb"

prop_replace_overlap :: Property
prop_replace_overlap = withTests 1 $ property $
  let
    xs = [ Replace "-" "1"
         , Replace "--" "2"
         , Replace "---" "3" ]
  in
    replaceWithList xs "-_--_---_----_-----" === "1_2_3_31_32"

drawReplacementsTrie :: [Replace] -> LT.Text
drawReplacementsTrie = listToTrie >>> drawTrie

prop_drawTrie :: Property
prop_drawTrie = property $ do

  replacements <- forAll $ Gen.shuffle
    [ Replace "aft"   "1"
    , Replace "after" "2"
    , Replace "apply" "3"
    , Replace "brain" "4"
    , Replace "broke" "5"
    ]

  drawReplacementsTrie replacements === LT.fromStrict (T.append [N.trimming|

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

  |] "\n\n")

prop_shuffle :: Property
prop_shuffle = property $ do

  replacements <- forAll $ Gen.shuffle
    [ Replace "aft"   "1"
    , Replace "after" "2"
    , Replace "apply" "3"
    , Replace "brain" "4"
    , Replace "broke" "5"
    ]

  replaceWithList replacements "afteraftbrainapple" === "214apple"
