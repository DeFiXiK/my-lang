module Tokens (ParserError(..), Token(..), parse, TokenType(..)) where

import           Control.Monad (foldM)
import           Data.Char     (isDigit, isLetter)
import           Data.List     (elemIndex)
import           Data.Maybe    (fromJust, fromMaybe, isJust, isNothing)
import           Data.Text     (Text)
import qualified Data.Text     as T

data TokenType
    = TokInteger Int Int -- Целое число и система счисления
    | TokReal Double -- Дробное число
    -- Логические операторы
    | TokNotEqual -- !=
    | TokEqual -- ==
    | TokLess -- <
    | TokLessOrEqual -- <=
    | TokGreater -- >
    | TokGreaterOrEqual -- >=
    -- Операции группы сложения
    | TokPlus -- +
    | TokMinus -- -
    | TokOr -- ||
    -- Операции группы умножения
    | TokMult -- *
    | TokDiv -- /
    | TokAnd -- &&
    -- Унарная операция
    -- | TokNot -- !
    -- Скобки(Select)
    | TokSelOpen -- (
    | TokSelClose -- )
    -- Логические константы
    | TokTrue -- true
    | TokFalse -- false
    -- Идентификатор
    | TokIdent String
    -- Запятая
    | TokComma -- ,
    | TokSemicolon -- ;
    | TokColon -- :
    -- Квадратные скобки - Программа
    | TokStartProgramm -- {
    | TokEndProgramm -- }
    -- Типы
    | TokPercent -- % - Целый тип
    | TokExcl -- ! - Дробный
    | TokDollar -- $ - Логический
    -- Составной оператор
    | TokCompoundStatementBegin -- begin
    | TokCompoundStatementEnd -- end
    -- Присвоение
    | TokAssignment -- :=
    -- Условный оператор
    | TokIf -- if
    | TokElse -- else
    -- Параметризированный цикл
    | TokParametrizedCycleFor -- for
    | TokParametrizedCycleTo -- to
    | TokParametrizedCycleStep -- step
    | TokParametrizedCycleNext -- next
    -- Условный цикл
    | TokCondLoop -- while
    -- Операторы ввода-вывода
    | TokInput -- readln
    | TokOutput -- writeln
    deriving (Show)

fromWords :: String -> Maybe TokenType
fromWords wds = case wds of
    "begin"   -> Just TokStartProgramm
    "end"     -> Just TokEndProgramm
    "if"      -> Just TokIf
    "else"    -> Just TokElse
    "for"     -> Just TokParametrizedCycleFor
    "to"      -> Just TokParametrizedCycleTo
    "step"    -> Just TokParametrizedCycleStep
    "next"    -> Just TokParametrizedCycleNext
    "while"   -> Just TokCondLoop
    "readln"  -> Just TokInput
    "writeln" -> Just TokOutput
    _         -> Nothing


fromOperator :: String -> Maybe TokenType
fromOperator oper = case oper of
    "!=" -> Just TokNotEqual
    "==" -> Just TokEqual
    "<"  -> Just TokLess
    "<=" -> Just TokLessOrEqual
    ">"  -> Just TokGreater
    ">=" -> Just TokGreaterOrEqual
    "+"  -> Just TokPlus
    "-"  -> Just TokMinus
    "||" -> Just TokOr
    "*"  -> Just TokMult
    "/"  -> Just TokDiv
    "&&" -> Just TokAnd
    -- "!"  -> Just TokNot
    "("  -> Just TokSelOpen
    ")"  -> Just TokSelClose
    ","  -> Just TokComma
    ";"  -> Just TokSemicolon
    ":"  -> Just TokColon
    "{"  -> Just TokStartProgramm
    "}"  -> Just TokEndProgramm
    "%"  -> Just TokPercent
    "!"  -> Just TokExcl
    "$"  -> Just TokDollar
    ":=" -> Just TokAssignment
    _    -> Nothing

data Token = Token
    { tokenType :: TokenType
    , tokenLine :: Int
    , tokenPos  :: Int
    }
    deriving(Show)

data CharType
    = CharWhitespace -- usual space, \t, \r
    | CharNewline    -- \n
    | CharDigit      -- 0-9
    | CharLetter     -- a-z, A-Z
    | CharSpecial    -- special symbols: comma, period and so on
    | CharSemicolon
    | CharOther
    deriving (Eq)

charType :: Char -> CharType
charType ch
    | ch == ' ' = CharWhitespace
    | ch == '\t' = CharWhitespace
    | ch == '\r' = CharWhitespace
    | ch == '\n' = CharNewline
    | isDigit ch = CharDigit
    | isLetter ch = CharLetter
    | ch `elem` ":<>=+-*/()%!$[].,{}" = CharSpecial
    | ch == ';' = CharSemicolon
    | otherwise = CharOther

data ParserState
    -- Свободное состояние
    = StateFree
    -- Состояние чтения идентификаторов, ключевых слов.
    | StateAlpha String Int
    -- Состояние чтения операторов.
    | StateOper String Int
    -- Состояние комментария.
    | StateComment Char
    -- Состояние чтение целых чисел.
    | StateInteger String Int


data Parser = Parser
    { parserState      :: ParserState
    , parserLine       :: Int
    , parserPos        :: Int
    , parserTokenStack :: [Token]
    }

data ParserError = ParserError
    { errorLine :: Int
    , errorPos  :: Int
    , errorMsg  :: String
    }

readBase :: Int -> String -> Maybe (Int, Int)
readBase = readBase' 0

-- |Reads a number from string with given base.
readBase' :: Int -> Int -> String -> Maybe (Int, Int)
readBase' acc base "" = Just (acc, base)
readBase' acc base (c:cs)
    | isNothing charDigit = Nothing
    | fromJust charDigit >= base = Nothing
    | otherwise = readBase' (acc * base + fromJust charDigit) base cs
    where
        digit char
            | isDigit char = elemIndex char "0123456789"
            | char `elem` "abcdef" = (+ 10) <$> elemIndex char "abcdef"
            | char `elem` "ABCDEF" = (+ 10) <$> elemIndex char "ABCDEF"
            | otherwise = Nothing
        charDigit = digit c



parseInt :: String -> Maybe (Int, Int)
parseInt str
    | head str == 'b' || head str == 'B' =
        readBase 2 (tail str)
    | head str == 'o' || head str == 'O' =
        readBase 8 (tail str)
    | head str == 'd' || head str == 'D' =
        readBase 10 (tail str)
    | head str == 'h' || head str == 'H' =
        readBase 16 (tail str)
    | otherwise =
        readBase 10 str

newParser :: Parser
newParser = Parser StateFree 1 1 []


advance :: Parser -> Maybe Char -> Either ParserError Parser
advance (Parser state line pos tokens) char =
    case advResult of
        AdvError err ->
            Left err
        AdvNoToken newState ->
            Right $ Parser newState newLine newPos tokens
        AdvToken newState newTok ->
            Right $ Parser newState newLine newPos (newTok:tokens)
        AdvNotConsumed newState ->
            advance (Parser newState line pos tokens) char
        AdvNotConsumedToken newState newTok ->
            advance (Parser newState line pos (newTok:tokens)) char
    where
        advResult = advance' state line pos char
        ct = fmap charType char
        newLine = if ct == Just CharNewline then line+1 else line
        newPos = if ct == Just CharNewline then 1 else pos+1

data AdvanceResult
    -- Ошибка парсинга
    = AdvError ParserError
    -- Символ использован, но нового токена еще нету, либо символ проигнорирован
    | AdvNoToken ParserState
    -- Символ использован и произведен новый токен
    | AdvToken ParserState Token
    -- Символ не использован
    | AdvNotConsumed ParserState
    -- Символ не использован, но произведен новый токен
    | AdvNotConsumedToken ParserState Token

advance' :: ParserState -> Int -> Int -> Maybe Char -> AdvanceResult
advance' StateFree line pos char
    | ct == Just CharLetter =
        AdvNotConsumed (StateAlpha [] pos)
    | ct == Just CharSpecial =
        AdvNotConsumed (StateOper [] pos)
    | ct == Just CharDigit =
        AdvNotConsumed (StateInteger [] pos)
    | ct == Just CharSemicolon =
        AdvToken StateFree (Token TokSemicolon line pos)
    | char == Nothing =
        AdvNoToken StateFree
    | ct == Just CharWhitespace =
        AdvNoToken StateFree
    | ct == Just CharNewline =
        AdvNoToken StateFree
    | otherwise =
        AdvError (ParserError line pos ("Неожиданный символ, " ++ show (fromJust char)))
    where
        ct = fmap charType char

advance' (StateAlpha buf tokpos) line pos char
    | ct == Just CharLetter =
        AdvNoToken (StateAlpha newBuf tokpos)
    | otherwise =
        AdvNotConsumedToken StateFree (Token identOrKeyword line tokpos)
    where
        identOrKeyword = fromMaybe (TokIdent buf) $ fromWords buf
        ct = fmap charType char
        newBuf = buf ++ [fromJust char]
advance' (StateOper buf tokpos) line pos char
    | ct == Just CharSpecial =
        case newBuf of
            "/*" ->
                AdvNoToken (StateComment 'Ƀ')
            _ -> AdvNoToken (StateOper newBuf tokpos)
    | otherwise =
        case maybeOper of
            Just oper ->
                AdvNotConsumedToken StateFree (Token oper line tokpos)
            Nothing ->
                AdvError (ParserError line pos ("Оператор " ++ buf ++ " не существует"))
    where
        maybeOper = fromOperator buf
        ct = fmap charType char
        newBuf = buf ++ [fromJust char]
advance' (StateComment lastChar) line pos char
    | lastChar == '*' && char == Just '/' =
        AdvNoToken StateFree
    | isJust char =
        AdvNoToken (StateComment $ fromJust char)
    | char == Nothing =
        AdvError (ParserError line pos "Неожиданный конец файла в середине комментария")
advance' (StateInteger buf tokpos) line pos char
    | ct == Just CharDigit =
        AdvNoToken (StateInteger newBuf tokpos)
    | otherwise =
        case parseInt buf of
            Just (num, base) -> AdvNotConsumedToken StateFree (Token (TokInteger num base) line tokpos)
            Nothing -> AdvError (ParserError line pos "Невалидное число")
    where
        ct = fmap charType char
        newBuf = buf ++ [fromJust char]

parse :: Text -> Either ParserError [Token]
parse text =
    case finResult of
        Left err     -> Left err
        Right parser -> Right $ reverse $ parserTokenStack parser
    where
        charQueue = fmap Just (T.unpack text) ++ [Nothing]
        finResult = foldM advance newParser charQueue
