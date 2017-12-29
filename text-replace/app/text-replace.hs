import Text.Replace

-- base
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Function ((&))
import Data.Functor (void)
import qualified System.IO as IO

-- optparse-applicative
import qualified Options.Applicative as Opt

-- parsec
import           Text.Parsec      ((<?>))
import qualified Text.Parsec      as P
import           Text.Parsec.String as P (Parser)

optsParserInfo :: Opt.ParserInfo Opts
optsParserInfo = Opt.info (Opt.helper <*> optsParser) $
  Opt.header "Perform simple replacements in a text file, using a list \
             \of search/replace pairs" <>
  Opt.footer "All streams are assumed to be UTF-8 encoded."

main :: IO ()
main = do
  opts <- Opt.execParser optsParserInfo
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8

data Opts =
  Opts
    { inFile :: Maybe FilePath
    , outFile :: Maybe FilePath
    , mapping :: Maybe String
    , mapFile :: Maybe FilePath
    , delimiter :: Delimiter
    }

optsParser :: Opt.Parser Opts
optsParser =
  Opts
    <$> inFileParser
    <*> outFileParser
    <*> mappingParser
    <*> mapFileParser
    <*> delimiterParser

inFileParser :: Opt.Parser (Maybe FilePath)
inFileParser = Opt.optional $ Opt.strOption $
  Opt.metavar "FILEPATH" <>
  Opt.long "in-file" <>
  Opt.short 'i' <>
  Opt.help "Input file to read (optional, defaults to stdin)"

outFileParser :: Opt.Parser (Maybe FilePath)
outFileParser = Opt.optional $ Opt.strOption $
  Opt.metavar "FILEPATH" <>
  Opt.long "out-file" <>
  Opt.short 'o' <>
  Opt.help "Output file to write (optional, defaults to stdout)"

mappingParser :: Opt.Parser (Maybe String)
mappingParser = Opt.optional $ Opt.strOption $
  Opt.metavar "MAPPING" <>
  Opt.long "mapping" <>
  Opt.short 'm' <>
  Opt.help "A list of search/replace pairs, separated by DELIMITER"

mapFileParser :: Opt.Parser (Maybe FilePath)
mapFileParser = Opt.optional $ Opt.strOption $
  Opt.metavar "FILEPATH" <>
  Opt.long "map-file" <>
  Opt.short 'f' <>
  Opt.help "A file containing a list of search/replace pairs, \
           \separated by DELIMITER"

type Delimiter = String

delimiterParser :: Opt.Parser Delimiter
delimiterParser = fmap (fromMaybe " ") $ Opt.optional $ Opt.strOption $
  Opt.metavar "DELIMITER" <>
  Opt.long "delimiter" <>
  Opt.short 'd' <>
  Opt.help "The delimiter that separates search/replace strings in \
           \--mapping and in the contents of --map-file; defaults to space"

parseReplacementList :: Delimiter -> P.SourceName -> String
                     -> Either String [Replace]
parseReplacementList delim sourceName input =
  let
    delimP :: P.Parser ()
    delimP = void (P.try (P.string delim))

    strP :: P.Parser String
    strP = P.manyTill P.anyChar delimP

    strP' :: P.Parser String'
    strP' = do
      x <- P.anyChar
      xs <- P.manyTill P.anyChar (delimP <|> P.eof)
      pure $ String' (x :| xs)

    replaceP :: P.Parser Replace
    replaceP = Replace <$> strP' <*> strP

  in
    P.parse (P.many replaceP <* P.eof) sourceName input & first show
