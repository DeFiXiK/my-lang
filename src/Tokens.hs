module Tokens (ParserError(..), Token(..), parse, TokenType(..), ParserEntries(..), toParserEntries, keywordsTbl, operatorsTbl, Entry(..)) where

import           Control.Monad   (foldM)
import           Data.Char       (isDigit, isLetter)
import           Data.List       (elemIndex)
import           Data.List.Split (splitOneOf)
import           Data.Maybe      (fromJust, fromMaybe, isJust, isNothing)
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Table           as Tbl
import           Text.Read       (readMaybe)

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
    | TokNot -- not
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
    | TokInt -- Int
    | TokFloat -- Float
    | TokBool -- Bool
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
    deriving (Show, Eq)

fromWords :: String -> Maybe TokenType
fromWords wds = case wds of
    "begin"   -> Just TokCompoundStatementBegin
    "end"     -> Just TokCompoundStatementEnd
    "if"      -> Just TokIf
    "not"     -> Just TokNot
    "else"    -> Just TokElse
    "true"    -> Just TokTrue
    "false"   -> Just TokFalse
    "int"     -> Just TokInt
    "float"   -> Just TokFloat
    "bool"    -> Just TokBool
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
    "("  -> Just TokSelOpen
    ")"  -> Just TokSelClose
    ","  -> Just TokComma
    ";"  -> Just TokSemicolon
    ":"  -> Just TokColon
    "{"  -> Just TokStartProgramm
    "}"  -> Just TokEndProgramm
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
    | CharTypes      -- Символы типов
    | CharBracket    -- Скобки
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
    | ch `elem` "()[]{}" = CharBracket
    | ch `elem` ":<>=+-*/.,&|!" = CharSpecial
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
    -- Состояние чтения дробных чисел.
    | StateFractional Int String Int
    -- Состояние чтения значений экспоненты
    | StateExpSign Double Int
    -- Чтение оператора экспоненты
    | StateExpVal Double Sign String Int
    -- Состояние внаружи программы
    | StateOutOfProgram

data Sign = SignPos | SignNeg

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

-- Читает строчку и определяет основание системы счисления
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
        readBase 2 (reverse $ tail str)
    | head str == 'o' || head str == 'O' =
        readBase 8 (reverse $ tail str)
    | head str == 'd' || head str == 'D' =
        readBase 10 (reverse $ tail str)
    | head str == 'h' || head str == 'H' =
        readBase 16 (reverse $ tail str)
    | otherwise =
        readBase 10 (reverse str)

newParser :: Parser
newParser = Parser StateOutOfProgram 1 1 []

parseWeirdDouble :: String -> Maybe Double
parseWeirdDouble str =
    case splitOneOf "eE" str of
        [wholeS, expS] -> do
            whole <- readMaybe wholeS :: Maybe Double
            expVal <- readMaybe expS :: Maybe Int
            return $ whole * 10^^expVal
        _ -> Nothing



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
    | char == Just '.' =
        AdvNoToken (StateFractional 0 "" pos)
    | ct == Just CharSpecial =
        AdvNotConsumed (StateOper [] pos)
    | ct == Just CharDigit =
        AdvNotConsumed (StateInteger [] pos)
    | ct == Just CharSemicolon =
        AdvToken StateFree (Token TokSemicolon line pos)
    | ct == Just CharTypes =
        AdvNoToken (StateOper [(fromJust char)] pos)
    | char == Just '}' =
        AdvToken StateOutOfProgram (Token TokEndProgramm line pos)
    | ct == Just CharBracket =
        AdvToken StateFree (Token oper line pos)
    |  ct == Just CharWhitespace || ct == Just CharNewline =
        AdvNoToken StateFree
    | char == Nothing =
        AdvError (ParserError line pos "Неожиданный конец файла")
    | otherwise =
        AdvError (ParserError line pos ("Неожиданный символ, " ++ show (fromJust char)))
    where
        ct = fmap charType char
        oper = fromJust $ fromOperator [(fromJust char)]

advance' (StateAlpha buf tokpos) line _ char
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
    | ct == Just CharDigit || ct == Just CharLetter =
        AdvNoToken (StateInteger newBuf tokpos)
    | char == Just '.' =
        case readBase 10 buf of
            Just (num, base) -> AdvNoToken (StateFractional num "" tokpos)
            Nothing -> AdvError (ParserError line pos "Дробное число имеет невалидный формат")
    | otherwise =
        case parseInt (reverse buf) of
            Just (num, base) -> AdvNotConsumedToken StateFree (Token (TokInteger num base) line tokpos)
            Nothing  -> case parseWeirdDouble buf of
                Just dbl -> AdvNotConsumedToken StateFree (dblToken dbl)
                Nothing -> AdvError $ ParserError line tokpos "Целое число имеет невалидный формат"
    where
        ct = fmap charType char
        newBuf = buf ++ [fromJust char]
        dblToken dbl = Token (TokReal dbl) line tokpos
advance' (StateFractional whole buf tokpos) line _ char
    | ct == Just CharDigit =
        AdvNoToken (StateFractional whole newBuf tokpos)
    | char == Just 'e' || char == Just 'E' =
        case bufDouble of
            Just double ->
                -- AdvError (ParserError line tokpos "Дробное число имеет невалидный формат 11")
                AdvNoToken (StateExpSign double tokpos)
            Nothing ->
                AdvError (ParserError line tokpos "Дробное число имеет невалидный формат 22")
    | maybe False (`elem` [CharWhitespace, CharNewline, CharSpecial, CharSemicolon, CharBracket]) ct || isNothing char =
        case bufDouble of
            Just double ->
                AdvNotConsumedToken StateFree (Token (TokReal double) line tokpos)
            Nothing ->
                AdvError (ParserError line tokpos "Дробное число имеет невалидный формат 33")
    | otherwise =
        AdvError (ParserError line tokpos "Дробное число имеет невалидный формат 44")
    where
        ct = fmap charType char
        newBuf = buf++[fromJust char]
        bufDouble = do
            bufNum <- readMaybe buf
            let bufDiv = 10 ^ length buf
            return $ fromIntegral whole + (bufNum / bufDiv)
advance' (StateExpSign mant tokpos) line _ char
    | char == Just '+' =
        AdvNoToken (StateExpVal mant SignPos "" tokpos)
    | char == Just '-' =
        AdvNoToken (StateExpVal mant SignNeg "" tokpos)
    | ct == Just CharDigit =
        AdvNotConsumed (StateExpVal mant SignPos "" tokpos)
    | otherwise =
        AdvError (ParserError line tokpos "Дробное число имеет невалидный формат 55")
    where
        ct = fmap charType char
advance' (StateExpVal mant sign buf tokpos) line _ char
    | ct == Just CharDigit =
        AdvNoToken (StateExpVal mant sign newBuf tokpos)
    | maybe False (`elem` [CharWhitespace, CharNewline, CharSpecial, CharSemicolon]) ct || isNothing char =
        case bufDouble of
            Just dbl ->
                AdvToken StateFree (Token (TokReal dbl) line tokpos )
            Nothing ->
                AdvError (ParserError line tokpos "Дробное число имеет невалидный формат 66")
    | otherwise =
        AdvError (ParserError line tokpos "Дробное число имеет невалидный формат 77")
    where
        ct = fmap charType char
        newBuf = fromJust char : buf
        buildDouble :: Double -> Sign -> Int -> Double
        buildDouble m s p =
            case s of
                SignPos -> m * 10 ^ p
                SignNeg -> m * 0.1 ^ p
        bufDouble = do
            pow <- readMaybe $ reverse buf
            let dbl = buildDouble mant sign pow
            return dbl

advance' StateOutOfProgram line pos char
    | char == Just '{' =
        AdvNotConsumed StateFree
    | ct == Just CharWhitespace|| ct == Just CharNewline || char == Nothing =
        AdvNoToken StateOutOfProgram
    | otherwise =
        AdvError (ParserError line pos $ "Неожиданный символ " ++ show char ++ " снаружи программы")
    where
        ct = fmap charType char

parse :: Text -> Either ParserError [Token]
parse text =
    case finResult of
        Left err     -> Left err
        Right parser -> Right $ reverse $ parserTokenStack parser
    where
        charQueue = fmap Just (T.unpack text) ++ [Nothing]
        finResult = foldM advance newParser charQueue

keywordsTbl :: Tbl.Table  TokenType
keywordsTbl = Tbl.tableList
    [ TokCompoundStatementBegin -- begin
    , TokCompoundStatementEnd -- end
    , TokIf -- if
    , TokElse -- else
    , TokParametrizedCycleFor -- for
    , TokParametrizedCycleTo -- to
    , TokNot --not
    , TokParametrizedCycleStep -- step
    , TokParametrizedCycleNext -- next
    , TokCondLoop -- while
    , TokInput -- readln
    , TokOutput -- writeln
    , TokInt -- int
    , TokFloat -- float
    , TokBool -- bool
    ]

operatorsTbl :: Tbl.Table TokenType
operatorsTbl = Tbl.tableList
    [ TokNotEqual -- !=
    , TokEqual -- ==
    , TokLess -- <
    , TokLessOrEqual -- <=
    , TokGreater -- >
    , TokGreaterOrEqual -- >=
    , TokPlus -- +
    , TokMinus -- -
    , TokOr -- ||
    , TokMult -- *
    , TokDiv -- /
    , TokAnd -- &&
    , TokSelOpen -- (
    , TokSelClose -- )
    , TokComma -- ,
    , TokSemicolon -- ;
    , TokColon -- :
    , TokStartProgramm -- {
    , TokEndProgramm -- }
    , TokAssignment -- :=
    ]

data EntryNumber = NumInteger Int Int| NumReal Double
    deriving (Eq)

instance Show EntryNumber where
    show (NumInteger int base) = show int
    show (NumReal dbl)         = show dbl

entryNumber :: TokenType -> EntryNumber
entryNumber (TokInteger int base) = NumInteger int base
entryNumber (TokReal real)        = NumReal real
entryNumber tt                    = error $ "Invalid token type " ++ show tt

data Entry = Entry
    { entryLine  :: Int
    , entryPos   :: Int
    , entryTable :: Int
    , entryIndex :: Int
    }
    deriving (Show)

data ParserEntries = ParserEntries
    { peIdentsTable  :: Tbl.Table String
    , peNumbersTable :: Tbl.Table EntryNumber
    , peEntries      :: [Entry]
    }
    deriving (Show)

isTokIdent :: TokenType -> Bool
isTokIdent (TokIdent _) = True
isTokIdent _            = False

identString :: TokenType -> String
identString (TokIdent s) = s
identString tt           = error $ show tt ++ " is not an identifier"

isTokInteger :: TokenType -> Bool
isTokInteger (TokInteger _ _) = True
isTokInteger _                = False

intValue :: TokenType -> Int
intValue (TokInteger i _) = i
intValue tt               = error $ show tt ++ " is not an integer"

isTokReal :: TokenType -> Bool
isTokReal (TokReal _) = True
isTokReal _           = False

isTokNumber :: TokenType -> Bool
isTokNumber tt = isTokInteger tt || isTokReal tt

emptyParserEntries :: ParserEntries
emptyParserEntries = ParserEntries Tbl.empty Tbl.empty []

pushEntry :: ParserEntries -> Token -> ParserEntries
pushEntry (ParserEntries identTable numTable entries) (Token tt line pos)
    | isJust kwIndex =
        ParserEntries identTable numTable (entries ++ [entr 0 (fromJust kwIndex)])
    | isJust operIndex =
        ParserEntries identTable numTable (entries ++ [entr 1 (fromJust operIndex)])
    | isTokIdent tt && isJust identIndex =
        ParserEntries identTable numTable (entries ++ [entr 2 (fromJust identIndex)])
    | isTokIdent tt && isNothing identIndex =
        ParserEntries newIdentTable numTable (entries ++ [entr 2 newIdentIndex])
    | isTokNumber tt && isJust numIndex =
        ParserEntries identTable numTable (entries ++ [entr 3 (fromJust numIndex)])
    | isTokNumber tt && isNothing numIndex =
        ParserEntries identTable newNumTable (entries ++ [entr 3 newNumIndex])
    | otherwise =
        error $ "Unexpected TokenType " ++ show tt
    where
        kwIndex = Tbl.find keywordsTbl tt
        operIndex = Tbl.find operatorsTbl tt
        identIndex = Tbl.find identTable $ identString tt
        (newIdentIndex, newIdentTable) = Tbl.append identTable $ identString tt
        numIndex = Tbl.find numTable $ entryNumber tt
        (newNumIndex, newNumTable) = Tbl.append numTable $ entryNumber tt
        entr tbl ind = Entry line pos tbl ind

toParserEntries :: [Token] -> ParserEntries
toParserEntries = foldl pushEntry emptyParserEntries
