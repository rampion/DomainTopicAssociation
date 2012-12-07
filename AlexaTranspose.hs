{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (forM_, (>>), return)
import Control.Monad.Trans (lift)
import Data.ByteString.Lazy.Char8 (readFile, count)
import Data.HashMap (empty, insertWith, assocs, Map)
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Text (unpack, pack, Text)
import Prelude (show, Show, ($), (.), (++), div, (*), (-), Char, Int, replicate, Eq, Ord, concat, read, length, map, const, flip, Either(..), fromIntegral)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (putStrLn, hPutStr, hPutStrLn, stderr, IO)
import Text.Parsec (char, eof, many, many1, noneOf, digit, string, ParsecT, Stream, runParserT)
import Text.Parsec.Prim (modifyState, getState, getPosition)
import Text.Parsec.Pos (sourceLine)

newtype Url = Url { fromUrl :: Text } deriving (Eq, Ord, Hashable)
newtype Category = Category { fromCategory :: Text } deriving (Eq, Ord)
data RelativeRank = RelativeRank { getIndex :: !Int, getTotal :: !Int }

instance Show Url where
  show = unpack . fromUrl

instance Show Category where
  show = unpack . fromCategory

instance Show RelativeRank where
  show (RelativeRank index total) = "(" ++ show index ++ "/" ++ show total ++ ")"

-- a category is a `/` delimited path on a line alone
category :: Stream s m Char => ParsecT s u m Category
category = do
  let section = (:) <$> char '/' <*> many1 (noneOf "/\n") 
  cat <- many1 section
  char '\n'
  return . Category . pack . concat $ cat

-- the urls for each category are given as "RANK. URL\n"
rankedUrl :: Stream s m Char => ParsecT s u m (Int -> (Url, RelativeRank))
rankedUrl = do
  index <- many1 digit
  string ". "
  url <- many1 $ noneOf "\n"
  char '\n'
  return $ (Url $ pack url,) . RelativeRank (read index)

-- a full listing including the category and its ranked list of urls 
categoryListing :: Stream s m Char => ParsecT s u m (Category, [(Url, RelativeRank)])
categoryListing = do
  cat <- category
  pairs <- many rankedUrl
  let tot = length pairs
  return $ (cat,  map ($ tot) pairs)

transposeCategoryListing :: Stream s m Char => ParsecT s (Map Url [(Category,RelativeRank)]) m ()
transposeCategoryListing = do
  (cat, pairs) <- categoryListing
  let include !m (url,rank) = let pair = (cat,rank) in insertWith (const (pair:)) url [pair] m
  modifyState $ foldl' include `flip` pairs

printProgress :: Stream s IO Char => Int -> ParsecT s u IO ()
printProgress numLines = do
  lineNo <- sourceLine <$> getPosition
  let perc = (100 * lineNo) `div` numLines
  let spac = 100 - perc
  let bar = "[" ++ replicate perc '*' ++ replicate (100-perc) ' ' ++ "]"
  let stat = show lineNo ++ "/" ++ show numLines
  lift . hPutStr stderr $ "\r" ++ bar ++ " " ++ stat
  
loadTranspose :: (Stream s IO Char) => Int -> ParsecT s (Map Url [(Category,RelativeRank)]) IO [(Url,[(Category,RelativeRank)])]
loadTranspose numLines = do
  printProgress numLines
  many1 $ transposeCategoryListing >> printProgress numLines
  eof
  assocs <$> getState
  
main :: IO ()
main = do
  [inputFile] <- getArgs
  contents <- readFile inputFile
  let numLines = fromIntegral $  count '\n' contents
  result <- runParserT (loadTranspose numLines) empty inputFile contents
  hPutStr stderr "\n"
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure
    Right xs -> do
      forM_ xs $ \(url, ys) -> do
        putStrLn $ show url
        forM_ ys $ \(cat, rank) -> 
          putStrLn $ show rank ++ " " ++ show cat
