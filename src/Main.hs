module Main where

import qualified Compile            as Comp
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
--     case Tok.parse text of
--         Left (Tok.ParserError line pos msg) ->
--             putStrLn ("Строка:"++ show line ++ " Позиция:" ++ show pos ++ " Сообщение:" ++ msg)
--         Right tokens -> do
--             let parserEntr = Tok.toParserEntries tokens
--             let keywrdsList = zip [0..] (toList Tok.keywordsTbl)
--             let opersList = zip [0..] (toList Tok.operatorsTbl)

--             putStrLn (tableName 0)
--             forM_ keywrdsList printTableEntry
--             putStrLn ""

--             putStrLn (tableName 1)
--             forM_ opersList printTableEntry
--             putStrLn ""

--             putStrLn (tableName 2)
--             forM_ (zip [0..] $ toList $ Tok.peIdentsTable parserEntr) printTableEntry
--             putStrLn ""

--             putStrLn (tableName 3)
--             forM_ (zip [0..] $ toList $ Tok.peNumbersTable parserEntr) printTableEntry
--             putStrLn ""

--             putStrLn (tableName 4)
--             forM_ (Tok.peEntries parserEntr) printEntry
--             putStrLn ""

-- printTableEntry :: (Show a) => (Int, a) -> IO ()
-- printTableEntry (eNum, eVal) =
--     putStrLn $ show eNum ++ " - " ++ show eVal

-- printEntry :: Tok.Entry -> IO()
-- printEntry en =
--     putStrLn $ "(" ++ show (Tok.entryTable en) ++ "," ++ show (Tok.entryIndex en) ++ ")" ++ " - " ++ "Строка: " ++ show (Tok.entryLine en) ++ ";" ++ " Позиция:" ++ show (Tok.entryIndex en)

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

asmParseAndPrint :: String -> IO ()
asmParseAndPrint path = do
    text <- TIO.readFile path
    case Tok.parse text of
        Left (Tok.ParserError line pos msg) ->
            putStrLn ("L:" ++ show line ++ ":" ++ show pos ++ ": " ++ msg)
        Right tokens ->
            case Syn.parse tokens of
                Left (Syn.ParserError msg line pos) ->
                    putStrLn ("S:" ++ show line ++ ":" ++ show pos ++ ": " ++ show msg)
                Right prg ->
                    case Comp.compile prg of
                        Left (Comp.Error msg) ->
                            putStrLn $ "C:" ++ ":" ++ ": " ++ msg
                        Right instr ->
                            forM_ (toEnumList instr) (\(i, ins) -> putStrLn $ show i ++ " @ " ++ " : " ++ show ins)

main :: IO ()
main = do
    args <- getArgs
    asmParseAndPrint $ head args
    -- parseAndPrint $ head args
