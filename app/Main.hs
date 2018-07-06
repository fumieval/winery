{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString as B
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Void
import qualified Data.Winery.Query as Q
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
  hPutDoc stderr err
  exitFailure
getRight (Right a) = return a

parseQuery :: Parsec Void String Q.Query
parseQuery = mconcat <$> sepBy1 parseTerm (string "|")

parseTerm :: Parsec Void String Q.Query
parseTerm = choice
  [ try $ Q.list <$ string ".[]"
  , try $ string "." >> Q.field <$> T.pack <$> some (noneOf ("| " :: String))
  , mempty <$ string "." ]

putTerm :: Term -> IO ()
putTerm t = putDoc $ pretty t <> hardline

main :: IO ()
main = getOpt Permute options <$> getArgs >>= \case
  (fs, args, []) -> do
    let o = foldl (flip id) defaultOptions fs
    q <- case parse parseQuery "arg" (unwords args) of
      Left e -> do
        hPutStrLn stderr $ parseErrorPretty e
        exitWith (ExitFailure 2)
      Right a -> pure a
    let getDec = getRight . getDecoderBy (Q.fromQuery decodeTerm q)
    printer <- case separateSchema o of
      Just mpath -> do
        bs <- maybe (readLn >>= B.hGet stdin) B.readFile mpath
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
      False -> B.hGetContents stdin >>= printer
      True -> forever $ do
        n <- readLn
        B.hGet stdin n >>= printer
  (_, _, es) -> do
    name <- getProgName
    die $ unlines es ++ usageInfo name options
