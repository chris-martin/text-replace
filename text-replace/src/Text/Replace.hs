module Text.Replace
  (
  -- * Performing replacement
    replaceWithList, replaceWithMap, replaceWithTrie

  -- * Specifying replacements
  , Replace (..), ReplaceMap, listToMap, mapToAscList

  -- * Replacements in trie structure
  , Trie, Trie' (..), listToTrie, ascListToTrie, mapToTrie, drawTrie

  -- * Non-empty text
  , Text' (..), text'fromString, text'fromText, text'head, text'tail

  ) where

-- base
import           Control.Arrow      ((>>>))
import qualified Data.Foldable      as Foldable
import           Data.Function      (on)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.String        (IsString (..))

-- containers
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree       (Tree)
import qualified Data.Tree       as Tree

-- text
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

{- | Apply a list of replacement rules to a string. The search for strings to replace is performed left-to-right, preferring longer matches to shorter ones.

Internally, the list will be converted to a 'ReplaceMap' using 'listToMap'. If the list contains more than one replacement for the same search string, the last mapping is used, and earlier mappings are ignored.

If you are going to be applying the same list of rules to multiple input strings, you should first convert the list to a 'Trie' using 'listToTrie' and then use 'replaceWithTrie' instead. -}
replaceWithList
  :: Foldable f
  => f Replace -- ^ List of replacement rules
  -> LT.Text   -- ^ Input string
  -> LT.Text   -- ^ Result after performing replacements on the input string
replaceWithList = listToTrie >>> replaceWithTrie

{- | Apply a map of replacement rules to a string. The search for strings to replace is performed left-to-right, preferring longer matches to shorter ones.

If you are going to be applying the same list of rules to multiple input strings, you should first convert the 'Map' to a 'Trie' using 'mapToTrie' and then use 'replaceWithTrie' instead. -}
replaceWithMap
  :: ReplaceMap -- ^ Map of replacement rules
  -> LT.Text    -- ^ Input string
  -> LT.Text    -- ^ Result after performing replacements on the input string
replaceWithMap = mapToTrie >>> replaceWithTrie

{- | Apply a trie of replacement rules to a string. The search for strings to replace is performed left-to-right, preferring longer matches to shorter ones.

To construct a 'Trie', you may use 'listToTrie' or 'mapToTrie'. -}
replaceWithTrie
  :: Trie    -- ^ Map of replacement rules, represented as a trie
  -> LT.Text -- ^ Input string
  -> LT.Text -- ^ Result after performing replacements on the input string
replaceWithTrie trie = go
  where
    go xs =
      case LT.uncons xs of
        Nothing -> LT.empty
        Just (x, xs') ->
          case replaceWithTrie1 trie xs of
            Nothing -> LT.cons x (go xs')
            Just (r, xs'') -> LT.append (LT.fromStrict r) (go xs'')

replaceWithTrie1 :: Trie -> LT.Text -> Maybe (T.Text, LT.Text)
replaceWithTrie1 trie xs =
  case LT.uncons xs of
    Nothing -> Nothing
    Just (x, xs') ->
      case Map.lookup x trie of
        Nothing                  -> Nothing
        Just (Trie' Nothing bs)  -> replaceWithTrie1 bs xs'
        Just (Trie' (Just r) bs) -> case replaceWithTrie1 bs xs' of
                                      Nothing -> Just (r, xs')
                                      longerMatch -> longerMatch

-- | Non-empty text.
data Text' = Text' Char T.Text
  deriving (Eq, Ord)

instance Show Text'
  where
    showsPrec i (Text' x xs) = showsPrec i (LT.cons x (LT.fromStrict xs))

{- | @'fromString' = 'text'fromString'@

🌶️ Warning: @('fromString' "" :: 'Text'') = ⊥@ -}
instance IsString Text'
  where
    fromString = text'fromString

{- | Convert an ordinary 'String' to a non-empty 'Text''.

🌶️ Warning: @text'fromString "" = ⊥@ -}
text'fromString :: String -> Text'
text'fromString [] = error "Text' cannot be empty"
text'fromString (x : xs) = Text' x (T.pack xs)

{- | Convert an ordinary 'T.Text' to a non-empty 'Text''.

🌶️ Warning: @text'fromText "" = ⊥@ -}
text'fromText :: T.Text -> Text'
text'fromText t =
  case T.uncons t of
    Nothing -> error "Text' cannot be empty"
    Just (x, xs) -> Text' x xs

{- | The first character of a non-empty string. -}
text'head :: Text' -> Char
text'head (Text' x _) = x

{- | All characters of a non-empty string except the first. -}
text'tail :: Text' -> T.Text
text'tail (Text' _ x) = x

{- | A replacement rule.

> Replace "abc" "xyz"

means

/When you encounter the string __@abc@__ in the input text, replace it with __@xyz@__./

The first argument must be a non-empty string, because there is no sensible way to interpret "replace all occurrences of the empty string." -}
data Replace =
  Replace
    { replaceFrom :: Text' -- ^ A string we're looking for
    , replaceTo   :: T.Text  -- ^ A string we're replacing it with
    }
    deriving (Eq, Show)

{- | A map where the keys are strings we're looking for, and the values are strings with which we're replacing a key that we find.

You may use 'listToMap' to construct a 'ReplaceMap' from a list of replacement rules, and you may use 'mapToAscList' to convert back to a list. -}
type ReplaceMap = Map Text' T.Text

{- | Construct a 'ReplaceMap' from a list of replacement rules.

If the list contains more than one replacement for the same search string, the last mapping is used, and earlier mappings are ignored. -}
listToMap :: Foldable f => f Replace -> ReplaceMap
listToMap = Foldable.toList >>> fmap toTuple >>> Map.fromList
  where
    toTuple x = (replaceFrom x, replaceTo x)

{- | Convert a replacement map to a list of replacement rules. The rules in the list will be sorted according to their 'replaceFrom' field in ascending order. -}
mapToAscList :: ReplaceMap -> [Replace]
mapToAscList = Map.toAscList >>> fmap (\(x, y) -> Replace x y)

{- | A representation of a 'ReplaceMap' designed for efficient lookups when we perform the replacements in 'replaceWithTrie'.

You may construct a 'Trie' using 'listToTrie' or 'mapToTrie'. -}
type Trie = Map Char Trie'

{- | A variant of 'Trie' which may contain a value at the root of the tree. -}
data Trie' =
  Trie'
    { trieRoot     :: Maybe T.Text
    , trieBranches :: Trie
    }
  deriving (Eq, Show)

{- | Draws a text diagram of a trie; useful for debugging. -}
drawTrie :: Trie -> LT.Text
drawTrie = trieForest >>> fmap (fmap T.unpack) >>> Tree.drawForest >>> LT.pack

trieForest :: Trie -> Tree.Forest T.Text
trieForest =
  Map.toAscList >>>
  fmap (\(c, t) -> trieTree (T.singleton c) t)

trieTree :: T.Text -> Trie' -> Tree T.Text
trieTree c (Trie' r bs) =
  case (r, Map.toAscList bs) of
    (Nothing, [(c', t)]) -> trieTree (T.snoc c c') t
    _ -> Tree.Node (T.append c (maybe T.empty (\rr -> T.pack (" - " ++ show rr)) r))
                   (trieForest bs)

{- | Convert a replacement map to a trie, which is used to efficiently implement 'replaceWithTrie'. -}
mapToTrie :: ReplaceMap -> Trie
mapToTrie = mapToAscList >>> ascListToTrie

{- | Convert a list of replacement rules to a trie, which is used to efficiently implement 'replaceWithTrie'.

If the list contains more than one replacement for the same search string, the last mapping is used, and earlier mappings are ignored. -}
listToTrie :: Foldable f => f Replace -> Trie
listToTrie = listToMap >>> mapToTrie

{- | Convert a list of replacement rules to a 'Trie', where the rules must be sorted in ascending order by the 'replaceFrom' field.

🌶️ Warning: this precondition is not checked. If you are not sure, it is safer to use 'listToTrie' instead. -}
ascListToTrie
  :: Foldable f
  => f Replace  -- ^ This list must be sorted according to the 'replaceFrom'
                --   field in ascending order
                --
                -- 🌶️ Warning: this precondition is not checked
  -> Trie
ascListToTrie =
  NonEmpty.groupBy ((==) `on` (replaceFrom >>> text'head)) >>>
  fmap (\xs -> (firstChar xs, subtrie xs)) >>>
  Map.fromAscList
  where
    firstChar = NonEmpty.head >>> replaceFrom >>> text'head
    subtrie = fmap (\(Replace x y) -> (text'tail x, y)) >>> ascListToTrie'

ascListToTrie'
  :: Foldable f
  => f (T.Text, T.Text)  -- ^ This list must be sorted according to the left
                         --   field of the tuple in ascending order
                         --
                         -- 🌶️ Warning: this precondition is not checked
  -> Trie'
ascListToTrie' = Foldable.toList >>> f
  where
    f :: [(T.Text, T.Text)] -> Trie'
    f ((a, x) : xs') | T.null a = Trie' (Just x) (g xs')
    f xs                        = Trie' Nothing (g xs)

    g :: (Foldable f, Functor f) => f (T.Text, T.Text) -> Trie
    g = fmap (\(x, y) -> Replace (text'fromText x) y) >>> ascListToTrie
