import System.IO
import System.Exit
import System.Environment
import Data.Maybe
import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Map as Map

data OpCode = Out
            | In
            | Emit
            | Plus
            | Minus
            | Multiply
            | Divide
            | Modulo
            | If
            | IfEnd
            | Dup
            | Rot
            | Swap
            | Drop
            | Over
            | Alloc
            | Free
            | Write
            | Read
            | Send
            | Recieve
            | Exit
            | Push
            deriving(Enum, Show)

data Token = Token OpCode
        | FNum Int
        | TName String
        | Word String
        | Define
        | WEnd
        | StartComment
        | EndComment
        | StartThread
        | EndThread
        | BackParsingError String
        deriving(Show)

data ByteCode = ByteCode Int | Header String deriving(Show)
type WordBody = [Token]
type WordTable = Map.Map String WordBody

tokenize :: String -> Token
tokenize "." = Token Out
tokenize "," = Token In
tokenize "emit" = Token Emit
tokenize "+" = Token Plus
tokenize "-" = Token Minus
tokenize "*" = Token Multiply
tokenize "/" = Token Divide
tokenize "%" = Token Modulo
tokenize ":" = Define
tokenize ";" = WEnd
tokenize "if" = Token If
tokenize "then" = Token IfEnd
tokenize "dup" = Token Dup
tokenize "rot" = Token Rot
tokenize "swap" = Token Swap
tokenize "drop" = Token Drop
tokenize "over" = Token Over
tokenize "alloc" = Token Alloc
tokenize "free" = Token Free
tokenize "write" = Token Write
tokenize "read" = Token Read
tokenize "send" = Token Send
tokenize "recv" = Token Recieve
tokenize "exit" = Token Exit
tokenize "(" = StartComment
tokenize ")" = EndComment
tokenize "[" = StartThread
tokenize "]" = EndThread
tokenize str
    | all isDigit $ str = FNum (read str :: Int)
    | otherwise = Word str

backlex :: String -> [Token] 
backlex sourcecode = map tokenize $ words sourcecode

untilcmt :: [Token]-> [Token] -> WordTable -> ([Token] -> [Token] -> WordTable -> [Token]) -> [Token]
untilcmt [] ac tbl f = [BackParsingError "Unclosed paranthesis."]
untilcmt (EndComment:tokens) ac tbl f = f tokens ac tbl
untilcmt (tok:tokens) ac tbl f = untilcmt tokens ac tbl f

collect :: [Token] -> [Token] -> [Token] -> WordTable -> ([Token] -> [Token] -> WordTable -> [Token]) -> [Token]
collect [] bd _ _ _ = bd
collect (WEnd:tokens) [] _ _ _ = [BackParsingError "Empty Word Defintion."]
collect (tok:tokens) [] ac tbl f = collect tokens [tok] ac tbl f
collect (WEnd:tokens) (Word nm:body) ac tbl f = f tokens ac $ Map.insert nm body tbl
collect (WEnd:tokens) (hd:body) _ _ _ = [BackParsingError "Word Definition doesn't start with a valid Word."]
collect (tok:tokens) body ac tbl f = collect tokens (body++[tok]) ac tbl f

backparse_code :: [Token] -> [Token] -> WordTable -> [Token]
backparse_code [] acc _ = acc
backparse_code (StartComment:tks) acc table = untilcmt tks acc table backparse_code
backparse_code (EndComment:tks) _ _ = [BackParsingError "Encountred Closing paranthesis without a preceeding open one."]
backparse_code (Word name:tks) acc table = case Map.lookup name table of
                                        Nothing ->  [BackParsingError ("Tried to use word not in scope. '" ++ name ++ "'")]
                                        Just bd -> backparse_code tks ((++) acc $ backparse_code bd [] table) $ table 
backparse_code (Define:tks) acc table = collect tks [] acc table backparse_code
backparse_code (FNum n:tks) acc table = backparse_code tks (acc++[Token Push, FNum n]) table
backparse_code (tk:tks) acc table = backparse_code tks (acc++[tk]) table

backparse_glob :: [Token] -> [Token] -> WordTable -> [Token]
backparse_glob [] acc _ = acc
backparse_glob (StartComment:tks) acc table = untilcmt tks acc table backparse_glob
backparse_glob (EndComment:tks) _ _ = [BackParsingError "Encountred Closing paranthesis without a preceeding open one."]
backparse_glob (Define:tks) acc table = collect tks [] acc table backparse_glob
backparse_glob (Word name:StartThread:tks) acc table = collectT tks [] table name where
    collectT :: [Token]-> [Token] -> WordTable -> String -> [Token]
    collectT [] _ _ _ = [BackParsingError "Unclosed Square Bracket."]
    collectT (EndThread:tokens) [] _ _  = acc
    collectT (tok:tokens) [] tbl n = collectT tokens [tok] tbl n
    collectT (EndThread:tokens) a tbl n = backparse_glob tokens ((++) acc $ (TName n):(backparse_code a [] tbl)) tbl
    collectT (tok:tokens) a tbl n = collectT tokens (a++[tok]) tbl n
backparse_glob (Word name:tks) _ _ = [BackParsingError "Encountred Thread Definition, but couldn't find '['."]
backparse_glob (EndThread:tks) _ _ = [BackParsingError "Encountred Closing Square Bracket without a preceeding open one."]

bkparse :: [Token] -> [Token]
bkparse toks = backparse_glob toks [] Map.empty

backemit :: [Token] -> [ByteCode] -> Either String [ByteCode]
backemit [] acc = Right acc
backemit (FNum n:toks) acc = case backemit toks (acc++[ByteCode n]) of
    Left er -> Left er
    Right result -> Right result
backemit (TName nm:toks) acc = case backemit toks (acc++[Header nm]) of
    Left er -> Left er
    Right result -> Right result
backemit (Token op:toks) acc = case backemit toks (acc++[ByteCode $ fromEnum op]) of
    Left er -> Left er
    Right result -> Right result
backemit (BackParsingError err:toks) _ = Left err


bkemit :: [Token] -> Either String [ByteCode]
bkemit toks = backemit toks []

backformat :: [ByteCode] -> [String] -> [String]
backformat [] acc = acc
backformat (ByteCode cd:bc) acc = backformat bc (acc++[show cd])
backformat (Header hd:bc) acc = backformat bc (acc++[hd])

-- impure

backerror :: String -> IO ()
backerror msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)

finish :: Either String [ByteCode] -> IO ()
finish out = do
        case out of
            Left err -> backerror err
            Right res -> writeFile "out.bin" $ intercalate " " (backformat res [])
        return ()

main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Nothing -> do backerror "Not Enough Arguments."
        Just fname -> finish . bkemit . bkparse . backlex =<< readFile fname
