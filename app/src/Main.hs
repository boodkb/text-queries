{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  )
where

import           System.Environment                       ( getArgs )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Text                                ( Text
                                                          , isInfixOf
                                                          )
import           Control.Monad                            ( when )
import qualified System.IO.Streams             as Streams

import           Query

makePredicate :: Query -> (Text -> Bool)
makePredicate = \case
  (Value text) -> isInfixOf text
  (And q1 q2 ) -> \t -> makePredicate q1 t && makePredicate q2 t
  (Or  q1 q2 ) -> \t -> makePredicate q1 t || makePredicate q2 t

pipeline :: (Text -> Bool) -> IO ()
pipeline predicate = do
  out <- Streams.encodeUtf8 =<< (Streams.unlines Streams.stdout)
  Streams.lines Streams.stdin
    >>= Streams.decodeUtf8
    >>= Streams.filter predicate
    >>= Streams.connectTo out

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ errorWithoutStackTrace "No query set"

  case parseQuery $ T.pack (head args) of
    Left  _     -> errorWithoutStackTrace "Invalid query"
    Right query -> pipeline (makePredicate query)

