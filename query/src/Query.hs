{-# LANGUAGE OverloadedStrings #-}
module Query
  ( Query(..)
  , parseQuery
  )
where

import           Data.Attoparsec.Text
import           Data.Text                                ( Text )
import           Data.Functor                             ( ($>) )
import           Data.Foldable                            ( foldl' )
import           Control.Applicative


data Query
  = Value Text
  | And Query Query
  | Or Query Query
  deriving (Show, Eq)

value :: Parser Query
value = do
  val <- char '"' *> takeWhile1 (/= '"') <* char '"'
  pure $ Value val

inSpaces :: Parser b -> Parser b
inSpaces p = skipSpace *> p <* skipSpace

expression :: Parser Query
expression = choice [char '(' *> query <* char ')', value]

query :: Parser Query
query = inSpaces $ do
  firstExpr  <- expression
  components <- many component
  pure $ foldl' rollUp firstExpr components
 where
  rollUp query (op, exp) = query `op` exp

  component = inSpaces $ do
    op <- choice ["AND" $> And, "OR" $> Or]
    skipSpace
    exp <- expression
    pure $ (op, exp)

parseQuery :: Text -> Either String Query
parseQuery = parseOnly (query <* endOfInput)
