-- base
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

-- optparse-applicative
import qualified Options.Applicative as Opt

optsParserInfo :: Opt.ParserInfo Opts
optsParserInfo = Opt.info (Opt.helper <*> optsParser) $
  Opt.header "Perform simple replacements in a text file, using a list \
             \of search/replace pairs"

main :: IO ()
main = do
  opts <- Opt.execParser optsParserInfo
  undefined

data Opts =
  Opts
    { inFile :: Maybe FilePath
    , outFile :: Maybe FilePath
    , mapping :: Maybe String
    , mapFile :: Maybe FilePath
    , delimiter :: String
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

delimiterParser :: Opt.Parser String
delimiterParser = fmap (fromMaybe " ") $ Opt.optional $ Opt.strOption $
  Opt.metavar "DELIMITER" <>
  Opt.long "delimiter" <>
  Opt.short 'd' <>
  Opt.help "The delimiter that separates search/replace strings in \
           \--mapping and in the contents of --map-file; defaults to space"
