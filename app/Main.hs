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
  , separateSchema :: Maybe (Maybe FilePath)
  , printSchema :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { streamInput = False
  , printSchema = True
  , separateSchema = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "s" ["stream"] (NoArg $ \o -> o { streamInput = True }) "stream input"
  , Option "S" ["separate-schema"] (OptArg (\s o -> o { separateSchema = Just s }) "PATH") "the schema is separated"
  , Option "" ["print-schema"] (NoArg $ \o -> o { printSchema = True }) "print the schema"
  ]

getRight :: Either StrategyError a -> IO a
getRight (Left err) = do
  hPutDoc stderr err
  exitFailure
getRight (Right a) = return a

main = getOpt Permute options <$> getArgs >>= \case
  (fs, _, []) -> do
    let o = foldl (flip id) defaultOptions fs

    printer <- case separateSchema o of
      Just mpath -> do
        bs <- maybe (readLn >>= B.hGet stdin) B.readFile mpath
        sch <- getRight $ deserialise bs
        when (printSchema o) $ putDoc $ pretty sch <> hardline
        dec <- getRight $ getDecoderBy decodeTerm sch
        return $ \bs -> putDoc $ pretty (dec bs) <> hardline
      Nothing -> return $ \bs -> do
        (s, t) <- getRight $ deserialiseTerm bs
        when (printSchema o) $ putDoc $ pretty s <> hardline
        putDoc $ pretty t <> hardline

    case streamInput o of
      False -> B.hGetContents stdin >>= printer
      True -> forever $ do
        n <- readLn
        B.hGet stdin n >>= printer
  (_, _, es) -> do
    name <- getProgName
    die $ unlines es ++ usageInfo name options
