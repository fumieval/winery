{-# LANGUAGE LambdaCase, OverloadedStrings, CPP #-}
module Main where

import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Word (Word64)
import qualified Codec.Winery.Query as Q
import Codec.Winery.Query.Parser
import Codec.Winery
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Megaparsec

data Options = Options
  { streamInput :: Bool
  , separateSchema :: Maybe (Maybe FilePath)
  , printSchema :: Bool
  , outputJSON :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { streamInput = False
  , printSchema = False
  , separateSchema = Nothing
  , outputJSON = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "s" ["stream"] (NoArg $ \o -> o { streamInput = True }) "streaming input: each payload is prefixed by little-endian Word64"
  , Option "S" ["separate-schema"] (OptArg (\s o -> o { separateSchema = Just s }) "PATH") "the schema is stored separately on PATH"
  , Option "" ["print-schema"] (NoArg $ \o -> o { printSchema = True }) "print the schema"
  , Option "J" ["JSON"] (NoArg $ \o -> o { outputJSON = True }) "print as JSON"
  ]

getRight :: Either WineryException a -> IO a
getRight (Left err) = do
  hPutDoc stderr $ prettyWineryException err <> hardline
  exitFailure
getRight (Right a) = return a

putTerm :: Doc AnsiStyle -> IO ()
putTerm t = putDoc $ t <> hardline

getLengthPrefixed :: Handle -> IO B.ByteString
getLengthPrefixed h = do
  str <- B.hGet h 8
  B.hGet h (fromIntegral (evalDecoder decodeCurrent str :: Word64))

app :: Options -> Q.Query (Doc AnsiStyle) (Doc AnsiStyle) -> Handle -> IO ()
app o q h = do
  let p
        | outputJSON o = pretty . T.decodeUtf8 . BL.toStrict . JSON.encode
        | otherwise = pretty
  let getDec = getRight . getDecoderBy (Q.runQuery q (Extractor (pure (pure . p))))

  printer <- case separateSchema o of
    Just mpath -> do
      bs <- maybe (getLengthPrefixed h) B.readFile mpath
      sch <- getRight $ deserialiseSchema bs
      when (printSchema o) $ putDoc $ pretty sch <> hardline
      dec <- getDec sch
      return $ mapM_ putTerm . evalDecoder dec
    Nothing -> return $ \bs_ -> do
      (s, bs) <- getRight $ splitSchema bs_
      dec <- getDec s
      when (printSchema o) $ putDoc $ pretty s <> hardline
      mapM_ putTerm $ evalDecoder dec bs

  case streamInput o of
    False -> B.hGetContents h >>= printer
    True -> forever $ getLengthPrefixed h >>= printer

main :: IO ()
main = getOpt Permute options <$> getArgs >>= \case
  (fs, qs : paths, []) -> do
    let o = foldl' (flip id) defaultOptions fs
    q <- case parse (parseQuery <* eof) "argument" $ T.pack qs of
      Left e -> do
#if MIN_VERSION_megaparsec(7,0,0)
        hPutStrLn stderr $ errorBundlePretty e
#else
        hPutStrLn stderr $ parseErrorPretty e
#endif
        exitWith (ExitFailure 2)
      Right a -> pure a
    case paths of
      [] -> app o q stdin
      ["-"] -> app o q stdin
      paths -> forM_ paths $ \path -> withFile path ReadMode (app o q)

  (_, _, es) -> do
    name <- getProgName
    die $ unlines es ++ usageInfo name options
