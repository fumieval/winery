{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import qualified Data.ByteString as B
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Winery.Term
import Data.Winery
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Options = Options
  { streamInput :: Bool
  , separateSchema :: Bool
  , printSchema :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { streamInput = False
  , printSchema = True
  , separateSchema = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "s" ["stream"] (NoArg $ \o -> o { streamInput = True }) "stream input"
  , Option "S" ["separate-schema"] (NoArg $ \o -> o { separateSchema = True }) "the schema is separated"
  , Option "" ["print-schema"] (NoArg $ \o -> o { printSchema = True }) "print the schema"
  ]

main = getOpt Permute options <$> getArgs >>= \case
  (fs, _, []) -> do
    let o = foldl (flip id) defaultOptions fs
    case separateSchema o of
      False -> forever $ do
        n <- readLn
        bs <- B.hGet stdin n
        case deserialiseTerm bs of
          Left err -> hPutStrLn stderr err
          Right (s, t) -> do
            when (printSchema o) $ putDoc $ pretty s <> hardline
            putDoc $ pretty t <> hardline
      True -> do
        n <- readLn
        bs <- B.hGet stdin n
        case deserialise bs of
          Left err -> hPutStrLn stderr err
          Right sch -> do
            when (printSchema o) $ putDoc $ pretty (sch :: Schema) <> hardline
            forever $ do
              n <- readLn
              bs <- B.hGet stdin n
              case deserialiseWithSchemaBy decodeTerm sch bs of
                Left err -> hPutStrLn stderr err
                Right t -> putDoc $ pretty t <> hardline
  (_, _, es) -> do
    name <- getProgName
    die $ unlines es ++ usageInfo name options
