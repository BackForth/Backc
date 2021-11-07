import System.IO
import System.Exit
import System.Environment
import Data.Maybe
import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Map as Map

data OpCode = Nop
            | Out
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
            | RecieveN
            | Exit
            | Do
            | Loop
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
        | NoVal
        | BackParsingError String
        deriving(Show)

instance Eq Token where
    (==) (Word n) (Word m) = n == m
    _ == _ = False -- note we return False because we really only need to check whether two words are equal or not

data ByteCode = ByteCode Int | Header String deriving(Show)
type WordBody = [Token]
type WordTable = Map.Map String WordBody

hexChar :: Char -> Int
hexChar '0' = 0
hexChar '1' = 1
hexChar '2' = 2
hexChar '3' = 3
hexChar '4' = 4
hexChar '5' = 5
hexChar '6' = 6
hexChar '7' = 7
hexChar '8' = 8
hexChar '9' = 9
hexChar 'A' = 10
hexChar 'B' = 11
hexChar 'C' = 12
hexChar 'D' = 13
hexChar 'E' = 14
hexChar 'F' = 15
hexChar ch = 0

hexToDec :: String -> Int
hexToDec [] = 0
hexToDec hxStr = hexChar (last hxStr) + 16 * hexToDec (init hxStr)

recognize :: String -> Token
recognize "" = NoVal
recognize "emit" = Token Emit
recognize "if" = Token If
recognize "then" = Token IfEnd
recognize "dup" = Token Dup
recognize "rot" = Token Rot
recognize "swap" = Token Swap
recognize "drop" = Token Drop
recognize "over" = Token Over
recognize "alloc" = Token Alloc
recognize "free" = Token Free
recognize "write" = Token Write
recognize "read" = Token Read
recognize "send" = Token Send
recognize "recv" = Token Recieve
recognize "recv#" = Token RecieveN
recognize "do" = Token Do
recognize "loop" = Token Loop
recognize "exit" = Token Exit
recognize str
    | all isDigit str = FNum (read str :: Int)
    | otherwise = Word str

backlex :: String -> String -> [Token] -> [Token]
backlex [] _ acc = acc
backlex ('.':input) wacc acc = backlex input [] (acc++[recognize wacc, Token Out])
backlex (',':input) wacc acc = backlex input [] (acc++[recognize wacc, Token In])
backlex ('+':input) wacc acc = backlex input [] (acc++[recognize wacc, Token Plus])
backlex ('-':input) wacc acc = backlex input [] (acc++[recognize wacc, Token Minus])
backlex ('*':input) wacc acc = backlex input [] (acc++[recognize wacc, Token Multiply])
backlex ('/':input) wacc acc = backlex input [] (acc++[recognize wacc, Token Divide])
backlex ('%':input) wacc acc = backlex input [] (acc++[recognize wacc, Token Modulo])
backlex (':':input) wacc acc = backlex input [] (acc++[recognize wacc, Define])
backlex (';':input) wacc acc = backlex input [] (acc++[recognize wacc, WEnd])
backlex ('(':input) wacc acc = backlex input [] (acc++[recognize wacc, StartComment])
backlex (')':input) wacc acc = backlex input [] (acc++[recognize wacc, EndComment])
backlex ('[':input) wacc acc = backlex input [] (acc++[recognize wacc, StartThread])
backlex (']':input) wacc acc = backlex input [] (acc++[recognize wacc, EndThread])
backlex (' ':input) wacc acc = backlex input [] (acc++[recognize wacc])
backlex ('\n':input) wacc acc = backlex input [] (acc++[recognize wacc])
backlex ('\t':input) wacc acc = backlex input [] (acc++[recognize wacc])
backlex (c:input) wacc acc = backlex input (wacc++(c:[])) acc

backstrip :: [Token] -> [Token] -> [Token]
backstrip [] acc = acc
backstrip (NoVal:tokens) acc = backstrip tokens acc
backstrip (tok:tokens) acc = backstrip tokens (acc++[tok])

bklex :: String -> [Token]
bklex sourcode = backstrip (backlex sourcode [] []) []

backelem :: (Eq a) => [a] -> a -> Maybe a
backelem [] s = Nothing
backelem (x:xs) s
    | x == s = Just s
    | otherwise = backelem xs s

untilcmt :: [Token]-> [Token] -> WordTable -> ([Token] -> [Token] -> WordTable -> [Token]) -> [Token]
untilcmt [] ac tbl f = [BackParsingError "Unclosed paranthesis."]
untilcmt (EndComment:tokens) ac tbl f = f tokens ac tbl
untilcmt (tok:tokens) ac tbl f = untilcmt tokens ac tbl f

collect :: [Token] -> [Token] -> [Token] -> WordTable -> ([Token] -> [Token] -> WordTable -> [Token]) -> [Token]
collect [] bd _ _ _ = bd
collect (WEnd:tokens) [] _ _ _ = [BackParsingError "Empty Word Defintion."]
collect (tok:tokens) [] ac tbl f = collect tokens [tok] ac tbl f
collect (WEnd:tokens) (Word nm:body) ac tbl f = case backelem body (Word nm) of
    Nothing -> f tokens ac $ Map.insert nm body tbl
    _ -> [BackParsingError "Encountred Recursive Word Definition."]
collect (WEnd:tokens) (hd:body) _ _ _ = [BackParsingError "Word Definition doesn't start with a valid Word."]
collect (tok:tokens) body ac tbl f = collect tokens (body++[tok]) ac tbl f

backparse_code :: [Token] -> [Token] -> WordTable -> [Token]
backparse_code [] acc _ = acc
backparse_code (StartComment:tks) acc table = untilcmt tks acc table backparse_code
backparse_code (EndComment:tks) _ _ = [BackParsingError "Encountred Closing paranthesis without a preceeding open one."]
backparse_code (Word ('$':name):tks) acc table = backparse_code tks (acc++[Word ('$':name)]) table
backparse_code (Word ('~':name):tks) acc table = backparse_code tks (acc++[Word ('~':name)]) table
backparse_code (Word ('@':name):tks) acc table = backparse_code tks (acc++[Word ('@':name)]) table
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
backparse_glob (Word name:StartThread:tks) acc table = let localt = table in collectT tks [] localt name where
    collectT :: [Token]-> [Token] -> WordTable -> String -> [Token]
    collectT [] _ _ _ = [BackParsingError "Unclosed Square Bracket."]
    collectT (EndThread:tokens) [] _ _  = acc
    collectT (tok:tokens) [] tbl n = collectT tokens [tok] tbl n
    collectT (EndThread:tokens) a tbl n = backparse_glob tokens ((++) acc $ (TName n):(backparse_code a [] tbl)) table
    collectT (tok:tokens) a tbl n = collectT tokens (a++[tok]) tbl n
backparse_glob (Word name:tks) _ _ = [BackParsingError "Encountred Thread Definition, but couldn't find '['."]
backparse_glob (EndThread:tks) _ _ = [BackParsingError "Encountred Closing Square Bracket without a preceeding open one."]
backparse_glob (tk:tks) _ _  = [BackParsingError "Encountred Something which is neither a thread or name defintion."]

bkparse :: [Token] -> [Token]
bkparse toks = backparse_glob toks [] Map.empty

encode :: String -> String -> ByteCode
encode [] acc = ByteCode (read acc :: Int)
encode (x:xs) acc = encode xs (acc++(pad . show $ fromEnum x)) where
    pad :: String -> String
    pad (x:[]) = ['0', '0', x]
    pad (x:n:[]) = ['0', x, n]
    pad str = str

backemit :: [Token] -> [ByteCode] -> [String] ->  Either String [ByteCode]
backemit [] acc defv = Right acc
backemit (FNum n:toks) acc defv = backemit toks (acc++[ByteCode n]) defv
backemit (TName nm:toks) acc defv = backemit toks (acc++[Header nm]) defv
backemit (Word ('$':addr):toks) acc defv = let adr = hexToDec addr in backemit toks (acc++[ByteCode 22, ByteCode adr]) defv
backemit (Word ('~':name):toks) acc defv = backemit toks (acc++[ByteCode 27, encode name []]) (defv++[name])
backemit (Word ('@':name):toks) acc defv = case backelem defv name of
    Nothing -> Left "Tried to put word not defined before."
    _ -> backemit toks (acc++[ByteCode 28, encode name []]) defv
backemit (Token op:toks) acc defv = backemit toks (acc++[ByteCode $ fromEnum op]) defv
backemit (BackParsingError err:toks) _ _ = Left err

bkemit :: [Token] -> Either String [ByteCode]
bkemit toks = backemit toks [] []

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
        Just fname -> finish . bkemit . bkparse . bklex =<< readFile fname
