{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.Identity (runIdentity, forM_)
import Data.HashMap (empty, insertWith, assocs, Map)
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Text (unpack, pack, Text)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (putStrLn, readFile)
import Text.Parsec (char, eof, many, many1, noneOf, digit, string, runParsecT, getParserState, parse, Consumed(..), Reply(..), SourceName)
import Text.Parsec.String (Parser)

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
category :: Parser Category
category = do
  let section = (:) <$> char '/' <*> many1 (noneOf "/\n") 
  cat <- many1 section
  char '\n'
  return . Category . pack . concat $ cat

-- the urls for each category are given as "RANK. URL\n"
rankedUrl :: Parser (Int -> (Url, RelativeRank))
rankedUrl = do
  index <- many1 digit
  string ". "
  url <- many1 $ noneOf "\n"
  char '\n'
  return $ (Url $ pack url,) . RelativeRank (read index)

-- a full listing including the category and its ranked list of urls 
categoryListing :: Parser (Category, [(Url, RelativeRank)])
categoryListing = do
  cat <- category
  pairs <- many rankedUrl
  let tot = length pairs
  return $ (cat,  map ($ tot) pairs)

-- parses an entire string (lazily) as multiple copies of the given parser
-- warning: uses `error` on parse error
parseAsMany :: Parser a -> SourceName -> String -> [a]
parseAsMany parser inputFile contents = loop initialState
  where Right initialState = parse getParserState inputFile contents
        loop state = case unconsume $ runParsecT parser' state of
                        Error err             -> error $ show err
                        Ok Nothing _ _        -> []
                        Ok (Just a) state' _  -> a : loop state'
        unconsume v = runIdentity $ case runIdentity v of 
                                      Consumed ma -> ma
                                      Empty ma -> ma
        parser' = (Just <$> parser) <|> (const Nothing <$> eof)

-- transpose a 2-dimensional associative list-of-lists
transpose :: (Ord b, Hashable b) => [(a, [(b, c)])] -> [(b, [(a, c)])]
transpose = assocs . foldl' f empty 
  where f :: (Ord b, Hashable b) => Map b [(a,c)] -> (a,[(b,c)]) -> Map b [(a,c)]
        f !m (a, bcs) = foldl' (g a) m bcs
        g :: (Ord b, Hashable b) => a -> Map b [(a,c)] -> (b,c) -> Map b [(a,c)]
        g a !m (b,c)  = let ac = (a,c) in insertWith (const (ac:)) b [ac] m

display :: (Show a, Show b, Show c) => [(a, [(b,c)])] -> IO ()
display xs = forM_ xs $ \(url, ys) -> do
  putStrLn $ show url
  forM_ ys $ \(cat, rank) -> 
    putStrLn $ show rank ++ " " ++ show cat

main :: IO ()
main = do
  [inputFile] <- getArgs
  contents <- readFile inputFile
  display . transpose $ parseAsMany categoryListing inputFile contents 
