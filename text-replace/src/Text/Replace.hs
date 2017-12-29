module Text.Replace
  (
  -- * Performing replacement
    replaceWithList, replaceWithMap, replaceWithTrie

  -- * Specifying replacements
  , Replace (..), ReplaceMap, listToMap

  -- * Replacements in trie structure
  , Trie, Trie' (..), mapToTrie, drawTrie

  -- * Non-empty string
  , String' (..), string'fromString, string'head, string'tail

  ) where

-- base
import           Control.Arrow      (first, (>>>))
import qualified Data.Foldable      as Foldable
import           Data.Function      (on)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.String        (IsString (..))

-- containers
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree       (Tree)
import qualified Data.Tree       as Tree

replaceWithList :: [Replace] -> String -> String
replaceWithList = listToMap >>> replaceWithMap

replaceWithMap :: ReplaceMap -> String -> String
replaceWithMap = mapToTrie >>> replaceWithTrie

replaceWithTrie :: Trie -> String -> String
replaceWithTrie trie = go
  where
    go [] = []
    go xs@(x : xs') =
      case replaceWithTrie1 trie xs of
        Nothing -> x : go xs'
        Just (r, xs'') -> r ++ go xs''

replaceWithTrie1 :: Trie -> String -> Maybe (String, String)
replaceWithTrie1 _ [] = Nothing
replaceWithTrie1 trie (x : xs) =
  case Map.lookup x trie of
    Nothing                  -> Nothing
    Just (Trie' Nothing bs)  -> replaceWithTrie1 bs xs
    Just (Trie' (Just r) bs) -> case replaceWithTrie1 bs xs of
                                  Nothing -> Just (r, xs)
                                  longerMatch -> longerMatch

newtype String' = String' (NonEmpty Char)
  deriving (Eq, Ord)

instance Show String'
  where
    showsPrec i (String' x) = showsPrec i (NonEmpty.toList x)

-- | 🌶️ 'string'fromString'
instance IsString String'
  where
    fromString = string'fromString

-- | 🌶️
string'fromString :: String -> String'
string'fromString = NonEmpty.fromList >>> String'

string'head :: String' -> Char
string'head (String' x) = NonEmpty.head x

string'tail :: String' -> String
string'tail (String' x) = NonEmpty.tail x

data Replace =
  Replace
    { replaceFrom :: String' -- ^ A string we're looking for
    , replaceTo   :: String  -- ^ A string we're replacing it with
    }
    deriving (Eq, Show)

type ReplaceMap = Map String' String

listToMap :: [Replace] -> ReplaceMap
listToMap = fmap toTuple >>> Map.fromList
  where
    toTuple x = (replaceFrom x, replaceTo x)

data Trie' =
  Trie'
    { trieRoot     :: Maybe [Char]
    , trieBranches :: Trie
    }
  deriving (Eq, Show)

type Trie = Map Char Trie'

drawTrie :: Trie -> String
drawTrie = trieForest >>> Tree.drawForest

trieForest :: Trie -> Tree.Forest String
trieForest =
  Map.toAscList >>>
  fmap (\(c, t) -> trieTree [c] t)

trieTree :: String -> Trie' -> Tree String
trieTree c (Trie' r bs) =
  case (r, Map.toAscList bs) of
    (Nothing, [(c', t)]) -> trieTree (c ++ [c']) t
    _ -> Tree.Node (c ++ maybe "" (\rr -> " - " ++ show rr) r)
                   (trieForest bs)

mapToTrie :: ReplaceMap -> Trie
mapToTrie = Map.toAscList >>> listToTrie

listToTrie
  :: Foldable f
  => f (String', String)  -- ^ 🌶️ Must be in ascending order by the left side
                          --   of the tuple (this precondition is not checked)
  -> Trie
listToTrie =
  NonEmpty.groupBy ((==) `on` (fst >>> string'head)) >>>
  fmap (\xs -> (firstChar xs, subtrie xs)) >>>
  Map.fromAscList
  where
    firstChar = NonEmpty.head >>> fst >>> string'head
    subtrie = fmap (\(x, y) -> (string'tail x, y)) >>> listToTrie'

listToTrie'
  :: Foldable f
  => f (String, String)  -- ^ 🌶️ Must be in ascending order by the left side
                         --   of the tuple (this precondition is not checked)
  -> Trie'
listToTrie' = Foldable.toList >>> f
  where
    f :: [(String, String)] -> Trie'
    f (([], x) : xs) = Trie' (Just x) (g xs)
    f xs             = Trie' Nothing (g xs)

    g :: (Foldable f, Functor f) => f (String, String) -> Trie
    g = fmap (first string'fromString) >>> listToTrie
