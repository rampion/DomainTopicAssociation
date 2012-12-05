{-# LANGUAGE FlexibleContexts #-}
module AlexaInvert where

import Text.Parsec
import Control.Applicative hiding (many)

newtype Url = Url String deriving (Show, Eq)
newtype Category = Category String deriving (Show, Eq)

data UrlPair = UP { getRank :: !Int, getUrl :: !Url } deriving (Show, Eq)
data CategoryListing = CL { getCategory :: !Category, getUrlPairs :: ![UrlPair] } deriving (Show, Eq)

category :: Stream s m Char => ParsecT s u m Category
category = Category . concat <$> many1 ((:) <$> char '/' <*> many1 (noneOf "/\n"))

categoryLine :: Stream s m Char => ParsecT s u m Category
categoryLine = category <* char '\n'

urlPair :: Stream s m Char => ParsecT s u m UrlPair
urlPair = do
  rank <- many1 digit
  string ". "
  url <- many1 $ noneOf "\n"
  return $ UP (read rank) (Url url)

urlLine :: Stream s m Char => ParsecT s u m UrlPair
urlLine = urlPair <* char '\n'

categoryListing :: Stream s m Char => ParsecT s u m CategoryListing
categoryListing = CL <$> categoryLine <*> many urlLine
