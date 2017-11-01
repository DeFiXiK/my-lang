module Main where

import           Control.Monad
import qualified Data.Text.IO       as TIO
import qualified Syntax             as Syn
import           System.Environment (getArgs)
import           Table
import qualified Tokens             as Tok
import           Traverse           (ASTTraversible (..))

-- parseAndPrint :: String -> IO ()
-- parseAndPrint path = do
--     text <- TIO.readFile path
--     case parse text of
--         Left (ParserError line pos msg) ->
--             putStrLn ("Строка:"++ show line ++ " Позиция:" ++ show pos ++ " Сообщение:" ++ msg)
--         Right tokens -> do
--             let parserEntr = toParserEntries tokens
--             let keywrdsList = zip [0..] (toList keywordsTbl)
--             let opersList = zip [0..] (toList operatorsTbl)

--             putStrLn (tableName 0)
--             forM_ keywrdsList printTableEntry
--             putStrLn ""

--             putStrLn (tableName 1)
--             forM_ opersList printTableEntry
--             putStrLn ""

--             putStrLn (tableName 2)
--             forM_ (zip [0..] $ toList $ peIdentsTable parserEntr) printTableEntry
--             putStrLn ""

--             putStrLn (tableName 3)
--             forM_ (zip [0..] $ toList $ peNumbersTable parserEntr) printTableEntry
--             putStrLn ""

--             putStrLn (tableName 4)
--             forM_ (peEntries parserEntr) printEntry
--             putStrLn ""

-- printTableEntry :: (Show a) => (Int, a) -> IO ()
-- printTableEntry (eNum, eVal) =
--     putStrLn $ show eNum ++ " - " ++ show eVal

-- printEntry :: Entry -> IO()
-- printEntry en =
--     putStrLn $ "(" ++ show (entryTable en) ++ "," ++ show (entryIndex en) ++ ")" ++ " - " ++ "Строка: " ++ show (entryLine en) ++ ";" ++ " Позиция:" ++ show (entryIndex en)

-- tableName :: Int -> String
-- tableName 0 = "Ключевые слова"
-- tableName 1 = "Разделители"
-- tableName 2 = "Идентификаторы"
-- tableName 3 = "Значения"
-- tableName 4 = "Токены"
-- tableName _ = error "Ошибка номера таблицы"

synPrint :: String -> Int -> IO Int
synPrint text indent = do
    let indentStr = concat (replicate indent "|   ")
    putStrLn $ indentStr ++ text
    return $ indent + 1


synParseAndPrint :: String -> IO ()
synParseAndPrint path = do
    text <- TIO.readFile path
    case Tok.parse text of
        Left (Tok.ParserError line pos msg) ->
            putStrLn (show line ++ ":" ++ show pos ++ ": " ++ msg)
        Right tokens ->
            case Syn.parse tokens of
                Left (Syn.ParserError msg line pos) ->
                    putStrLn (show line ++ ":" ++ show pos ++ ": " ++ msg)
                Right prg ->
                    asttraverse synPrint prg 0

main :: IO ()
main = do
    args <- getArgs
    synParseAndPrint $ head args
