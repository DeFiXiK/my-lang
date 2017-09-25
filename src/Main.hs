module Main where

import qualified Data.Text.IO       as TIO
import           System.Environment (getArgs)
import           Tokens

parseAndPrint :: String -> IO ()
parseAndPrint path = do
    text <- TIO.readFile path
    case parse text of
      Left (ParserError line pos msg) ->
        putStrLn ("Line:"++ show line ++ " Position:" ++ show pos ++ " Message:" ++ msg)
      Right tokens ->
        mapM_ print tokens

main :: IO ()
main = do
  args <- getArgs
  parseAndPrint $ head args
