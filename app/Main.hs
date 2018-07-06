{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString as B
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Void
import qualified Data.Winery.Query as Q
import Data.Winery.Query.Parser
import Data.Winery.Term
import Data.Winery
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

data Options = Options
  { streamInput :: Bool
  , separateSchema :: Maybe (Maybe FilePath)
  , printSchema :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { streamInput = False
  , printSchema = False
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
  hPutDoc stderr (err <> hardline)
  exitFailure
getRight (Right a) = return a

putTerm :: Doc AnsiStyle -> IO ()
putTerm t = putDoc $ t <> hardline

app :: Options -> Q.Query (Doc AnsiStyle) (Doc AnsiStyle) -> Handle -> IO ()
app o q h = do
  let getDec = getRight . getDecoderBy (Q.runQuery q (pure . pretty <$> decodeTerm))
  printer <- case separateSchema o of
    Just mpath -> do
      bs <- maybe (readLn >>= B.hGet h) B.readFile mpath
      sch <- getRight $ deserialise bs
      when (printSchema o) $ putDoc $ pretty sch <> hardline
      dec <- getDec sch
      return $ mapM_ putTerm . dec
    Nothing -> return $ \bs_ -> do
      (s, bs) <- getRight $ splitSchema bs_
      dec <- getDec s
      when (printSchema o) $ putDoc $ pretty s <> hardline
      mapM_ putTerm $ dec bs

  case streamInput o of
    False -> B.hGetContents h >>= printer
    True -> forever $ do
      n <- readLn
      B.hGet h n >>= printer

main :: IO ()
main = getOpt Permute options <$> getArgs >>= \case
  (fs, qs : paths, []) -> do
    let o = foldl (flip id) defaultOptions fs
    q <- case parse (parseQuery <* eof) "argument" $ T.pack qs of
      Left e -> do
        hPutStrLn stderr $ parseErrorPretty e
        exitWith (ExitFailure 2)
      Right a -> pure a
    forM_ paths $ \case
      "-" -> app o q stdin
      path -> withFile path ReadMode (app o q)

  (_, _, es) -> do
    name <- getProgName
    die $ unlines es ++ usageInfo name options
