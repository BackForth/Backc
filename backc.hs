import System.IO
import System.Exit
import System.Environment
import Data.Maybe
import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Map as Map

data Token = FNum Int
        | Word String
        | Define
        | WEnd
        | StartComment
        | EndComment
        | StartThread
        | EndThread
        | StartMacro
        | EndMacro
        | No
        deriving(Show)

instance Eq Token where -- note that its necessary we define this, so that the elem function works, and we can generate correct errors @ backparse_code
    (Word n) == (Word m) = n == m
    _ == _ = False -- note we return false, because we don't really care.

data ByteCode = ByteCode Int | Header String | BackParsingError String deriving(Show)
type WordBody = [ByteCode]
type WordTable = Map.Map String WordBody
type MacroTable = Map.Map Int WordBody

hexChar :: Char -> Int
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

recognize :: String -> Token -> [Token]
recognize "" t = [t]
recognize str No
    | all isDigit str = [FNum (read str :: Int)]
    | otherwise = [Word str]
recognize str t
    | all isDigit str = [FNum (read str :: Int), t]
    | otherwise = [Word str, t]

backlex :: String -> String -> [Token] -> [Token]
backlex [] [] acc = acc
backlex [] wacc acc = acc++(recognize wacc No)
backlex (':':input) wacc acc = backlex input [] ((++) acc $ recognize wacc Define)
backlex (';':input) wacc acc = backlex input [] ((++) acc $ recognize wacc WEnd)
backlex ('(':input) wacc acc = backlex input [] ((++) acc $ recognize wacc StartComment)
backlex (')':input) wacc acc = backlex input [] ((++) acc $ recognize wacc EndComment)
backlex ('[':input) wacc acc = backlex input [] ((++) acc $ recognize wacc StartThread)
backlex (']':input) wacc acc = backlex input [] ((++) acc $ recognize wacc EndThread)
backlex ('{':input) wacc acc = backlex input [] ((++) acc $ recognize wacc StartMacro)
backlex ('}':input) wacc acc = backlex input [] ((++) acc $ recognize wacc EndMacro)
backlex (c:input) wacc acc = backlex input (wacc++[c]) acc

backlexm :: [String] -> [Token] -> [Token]
backlexm [] a = a
backlexm (x:xs) a = backlexm xs (a++(backlex x [] []))

bklex :: String -> [Token]
bklex sourcode = backlexm (words sourcode) []

untilcmt :: [Token]-> [ByteCode] -> WordTable -> MacroTable -> ([Token] -> [ByteCode] -> WordTable -> MacroTable -> [ByteCode]) -> [ByteCode]
untilcmt [] ac tbl mt f = [BackParsingError "Unclosed paranthesis."]
untilcmt (EndComment:tokens) ac tbl mt f = f tokens ac tbl mt
untilcmt (tok:tokens) ac tbl mt f = untilcmt tokens ac tbl mt f

collect :: [Token] -> [Token] -> [ByteCode] -> WordTable -> MacroTable -> ([Token] -> [ByteCode] -> WordTable -> MacroTable -> [ByteCode]) -> [ByteCode]
collect [] _ _ _ _ _ = [BackParsingError "Word Defintion doesn't end with ';'."]
collect (WEnd:tokens) [] _ _ _ _ = [BackParsingError "Empty Word Defintion."]
collect (tok:tokens) [] ac tbl m f = collect tokens [tok] ac tbl m f
collect (WEnd:tokens) (Word nm:body) ac tbl m f = let localt = tbl in case elem (Word nm) body of
    False -> f tokens ac (Map.insert nm (backparse_code body [] localt m) tbl) m
    True -> [BackParsingError "Encountred Recursive Word Definition."]
collect (WEnd:tokens) (hd:body) _ _ _ _ = [BackParsingError "Word Definition doesn't start with a valid Word."]
collect (tok:tokens) body ac tbl m f = collect tokens (body++[tok]) ac tbl m f

encode :: String -> String -> ByteCode
encode [] acc = ByteCode (read acc :: Int)
encode (x:xs) acc = encode xs (acc++(pad . show $ fromEnum x)) where
    pad :: String -> String
    pad (x:[]) = ['0', '0', x]
    pad (x:n:[]) = ['0', x, n]
    pad str = str

backparse_code :: [Token] -> [ByteCode] -> WordTable -> MacroTable -> [ByteCode]
backparse_code [] acc _ _ = acc
backparse_code (StartComment:tks) acc table mtable = untilcmt tks acc table mtable backparse_code
backparse_code (EndComment:tks) _ _ _ = [BackParsingError "Encountred Closing paranthesis without a preceeding open one."]
backparse_code (Word ('$':name):tks) acc table mtable = backparse_code tks (acc++[ByteCode 1, ByteCode (hexToDec name)]) table mtable
backparse_code (Word ('~':name):tks) acc table mtable = backparse_code tks (acc++[ByteCode 27, encode name []]) table mtable
backparse_code (Word ('@':name):tks) acc table mtable = backparse_code tks (acc++[ByteCode 28, encode name []]) table mtable
backparse_code (Word ('\\':name):tks) acc table mtable = if all isDigit name then
        case Map.lookup (read name :: Int) mtable of
            Nothing -> [BackParsingError ("Tried To expand word not in scope. '" ++ name ++ "'")]
            Just bd -> backparse_code tks (acc++bd) table mtable
    else [BackParsingError ("Tried to expand invalid word. '"++name++"'")]
backparse_code (Word name:tks) acc table mtable = case Map.lookup name table of
                                        Nothing ->  [BackParsingError ("Tried to use word not in scope. '" ++ name ++ "'")]
                                        Just bd -> backparse_code tks (acc++bd) table mtable
backparse_code (Define:tks) acc table mtable = collect tks [] acc table mtable backparse_code
backparse_code (FNum n:StartMacro:tks) acc table mtable = let localm = mtable in collectM tks [] localm where
    collectM :: [Token] -> [Token] -> MacroTable -> [ByteCode]
    collectM (EndMacro:ts) [] mt = [BackParsingError "Empty Word Defintion."]
    collectM (EndMacro:ts) ac mt = let res = backparse_code ac [] table mt in
        backparse_code ts (((++) acc (ByteCode 29:ByteCode n:res))++[ByteCode 30]) table $ Map.insert n res mt
    collectM (t:ts) ac mt = collectM ts (ac++[t]) mt
backparse_code (FNum n:tks) acc table mtable = backparse_code tks (acc++[ByteCode 1, ByteCode n]) table mtable
backparse_code (StartThread:_) _ _ _ = [BackParsingError "Encountred illegal word while parsing. '['"]
backparse_code (EndThread:_) _ _ _ = [BackParsingError "Encountred illegal word while parsing. ']'"]
backparse_code (StartMacro:_) _ _ _ = [BackParsingError "Encountred illegal word while parsing. '{'"]
backparse_code (EndMacro:_) _ _ _ = [BackParsingError "Encountred illegal word while parsing. '}'"]

backparse_glob :: [Token] -> [ByteCode] -> WordTable -> MacroTable -> [ByteCode]
backparse_glob [] acc _ _ = acc
backparse_glob (StartComment:tks) acc table mtable = untilcmt tks acc table mtable backparse_glob
backparse_glob (EndComment:tks) _ _ _ = [BackParsingError "Encountred Closing paranthesis without a preceeding open one."]
backparse_glob (Define:tks) acc table mtable = collect tks [] acc table mtable backparse_glob
backparse_glob (Word name:StartThread:tks) acc table mtable = let localt = table in check $ collectT tks [] localt name where
    check :: [ByteCode] -> [ByteCode]
    check [Header n, BackParsingError er] = [BackParsingError er]
    check ac = ac
    collectT :: [Token]-> [Token] -> WordTable -> String -> [ByteCode]
    collectT [] _ _ _ = [BackParsingError "Unclosed Square Bracket."]
    collectT (EndThread:tokens) [] _ _  = acc
    collectT (tok:tokens) [] tbl n = collectT tokens [tok] tbl n
    collectT (EndThread:tokens) a tbl n = backparse_glob tokens ((++) acc $ (Header n):(backparse_code a [] tbl mtable)) table mtable
    collectT (tok:tokens) a tbl n = collectT tokens (a++[tok]) tbl n
backparse_glob (Word name:_) _ _ _ = [BackParsingError "Encountred Thread Definition, but couldn't find '['."]
backparse_glob (EndThread:tks) _ _ _ = [BackParsingError "Encountred Closing Square Bracket without a preceeding open one."]
backparse_glob _ _ _ _ = [BackParsingError "Encountred Something which is neither a thread or name defintion."]

backmap :: WordTable
backmap = Map.fromList [(".",[ByteCode 2])
    ,(",",[ByteCode 3])
    ,("emit",[ByteCode 4])
    ,("+",[ByteCode 5])
    ,("-",[ByteCode 6])
    ,("*",[ByteCode 7])
    ,("/",[ByteCode 8])
    ,("%",[ByteCode 9])
    ,("if",[ByteCode 10])
    ,("then",[ByteCode 11])
    ,("dup",[ByteCode 12])
    ,("rot",[ByteCode 13])
    ,("swap",[ByteCode 14])
    ,("drop",[ByteCode 15])
    ,("over",[ByteCode 16])
    ,("alloc",[ByteCode 17])
    ,("free",[ByteCode 18])
    ,("write",[ByteCode 19])
    ,("read",[ByteCode 20])
    ,("send",[ByteCode 21])
    ,("recv",[ByteCode 22])
    ,("recv#",[ByteCode 23])
    ,("exit",[ByteCode 24])
    ,("do",[ByteCode 25])
    ,("loop",[ByteCode 26])
    ]

backmac :: MacroTable
backmac = Map.empty

bkparse :: [Token] -> Either String [ByteCode]
bkparse toks = case backparse_glob toks [] backmap backmac of
    [BackParsingError er] -> Left er
    res -> Right res

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
        Just fname -> finish . bkparse . bklex =<< readFile fname
