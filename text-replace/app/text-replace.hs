import Text.Replace

-- base
import           Control.Applicative ((<|>))
import           Control.Arrow       ((>>>))
import           Data.Foldable       (asum)
import           Data.Function       ((&))
import           Data.Functor        (void)
import           Data.Semigroup      ((<>))
import           System.Exit         (die)
import qualified System.IO           as IO

-- optparse-applicative
import qualified Options.Applicative as Opt

-- parsec
import qualified Text.Parsec        as P
import           Text.Parsec.String as P (Parser)

optsParserInfo :: Opt.ParserInfo Opts
optsParserInfo = Opt.info (Opt.helper <*> optsParser) $
  Opt.header "Perform simple replacements in a text file, using a list \
             \of search/replace pairs" <>
  Opt.footer "All streams are assumed to be UTF-8 encoded."

enc :: IO.TextEncoding
enc = IO.utf8

main :: IO ()
main = do
  opts <- Opt.execParser optsParserInfo
  IO.hSetEncoding IO.stdout enc
  IO.hSetEncoding IO.stderr enc

  let d = delimiterOpt opts

  argMapping <-
    let
      f arg = parseReplacementList' d "--mapping" arg
    in
      foldMap f (opt_mapping opts)

  fileMapping <-
    let
      f path = IO.withFile path IO.ReadMode $ \h -> do
        IO.hSetEncoding h enc
        x <- IO.hGetContents h
        parseReplacementList' d path x
     in
      foldMap f (opt_mapFile opts)

  withInputH (opt_inFile opts) $ \inH ->
    withOutputH (opt_outFile opts) $ \outH -> do
      input <- IO.hGetContents inH
      let output = replaceWithList (argMapping ++ fileMapping) input
      IO.hPutStr outH output

withInputH :: Maybe FilePath -> (IO.Handle -> IO a) -> IO a
withInputH Nothing f = f IO.stdin
withInputH (Just path) f = IO.withFile path IO.ReadMode f

withOutputH :: Maybe FilePath -> (IO.Handle -> IO a) -> IO a
withOutputH Nothing f = f IO.stdout
withOutputH (Just path) f = IO.withFile path IO.WriteMode f

data Opts =
  Opts
    { opt_inFile :: Maybe FilePath
    , opt_outFile :: Maybe FilePath
    , opt_mapping :: [String]
    , opt_mapFile :: [FilePath]
    , opt_delimiter :: [Delimiter]
    , opt_newlineDelimiter :: Bool
    }

optsParser :: Opt.Parser Opts
optsParser =
  Opts
    <$> inFileParser
    <*> outFileParser
    <*> mappingParser
    <*> mapFileParser
    <*> delimiterParser
    <*> newlineDelimiterParser

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

mappingParser :: Opt.Parser [String]
mappingParser = Opt.many $ Opt.strOption $
  Opt.metavar "MAPPING" <>
  Opt.long "mapping" <>
  Opt.short 'm' <>
  Opt.help "A list of search/replace pairs, separated by DELIMITER"

mapFileParser :: Opt.Parser [FilePath]
mapFileParser = Opt.many $ Opt.strOption $
  Opt.metavar "FILEPATH" <>
  Opt.long "map-file" <>
  Opt.short 'f' <>
  Opt.help "A file containing a list of search/replace pairs, \
           \separated by DELIMITER"

type Delimiter = String

delimiterParser :: Opt.Parser [Delimiter]
delimiterParser = Opt.many $ Opt.strOption $
  Opt.metavar "DELIMITER" <>
  Opt.long "delimiter" <>
  Opt.short 'd' <>
  Opt.help "Add a delimiter that separates search/replace strings in \
           \--mapping and in the contents of --map-file"

newlineDelimiterParser :: Opt.Parser Bool
newlineDelimiterParser = Opt.switch $
  Opt.long "newline-delimiter" <>
  Opt.short 'n' <>
  Opt.help "Add newline as a delimiter"

delimiterOpt :: Opts -> [Delimiter]
delimiterOpt opts =
  opt_delimiter opts
  & (if opt_newlineDelimiter opts then ("\n" :) else id)

parseReplacementList :: [Delimiter] -> P.SourceName -> String
                     -> Either P.ParseError [Replace]
parseReplacementList delims sourceName input =
  let
    delimP :: P.Parser ()
    delimP = delims & fmap (void . P.try . P.string) & asum

    strP :: P.Parser String
    strP = P.manyTill P.anyChar (delimP <|> P.eof)

    strP' :: P.Parser String'
    strP' = do
      x <- P.anyChar
      xs <- P.manyTill P.anyChar (delimP)
      pure $ String' (x :| xs)

    replaceP :: P.Parser Replace
    replaceP = Replace <$> strP' <*> strP

  in
    P.parse (P.many replaceP <* P.eof) sourceName input

parseReplacementList' :: [Delimiter] -> P.SourceName -> String -> IO [Replace]
parseReplacementList' delims sourceName input =
  parseReplacementList delims sourceName input & either (show >>> die) pure
