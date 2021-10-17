import System.IO
import System.Exit
import System.Environment
import Data.Maybe
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Map as Map

backerror :: String -> IO ()
backerror msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)

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
            | Push
            deriving(Enum, Show)

data Token = Token OpCode
        | FNum Int
        | Word String
        | Define
        | WEnd
        | BackParsingError String
        deriving(Show)

instance Eq Token where
    WEnd == WEnd = True
    WEnd == _ = False
    _ == WEnd = False

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
tokenize str
    | all isDigit $ str = FNum (read str :: Int)
    | otherwise = Word str

backlex :: String -> [Token] 
backlex sourcecode = map tokenize $ words sourcecode

backparse :: [Token] -> [Token] -> WordTable -> [Token]
backparse [] acc _ = acc
backparse (Word name:tks) acc table = case Map.lookup name table of
                                        Nothing ->  [BackParsingError "Tried to use word not in scope."]
                                        Just bd -> backparse tks ((++) acc $ backparse bd [] table) $ table 
backparse (Define:tks) acc table = collect tks [] where
    collect :: [Token] -> [Token] -> [Token]
    collect [] bd = bd
    collect (WEnd:tokens) [] = [BackParsingError "Empty Word Defintion."]
    collect (tok:tokens) [] = collect tokens [tok]
    collect (WEnd:tokens) (Word nm:body) = backparse tokens acc $ Map.insert nm body table
    collect (WEnd:tokens) (hd:body) = [BackParsingError "Word Definition doesn't start with a valid Word."]
    collect (tok:tokens) body = collect tokens (body++[tok])
backparse (FNum n:tks) acc table = backparse tks (acc++[Token Push, FNum n]) table
backparse (tk:tks) acc table = backparse tks (acc++[tk]) table

bkparse :: [Token] -> [Token]
bkparse toks = backparse toks [] Map.empty

backemit :: [Token] -> [Int] -> Either String [Int]
backemit [] acc = Right acc
backemit (FNum n:toks) acc = case backemit toks (acc++[n]) of
    Left er -> Left er
    Right result -> Right result 
backemit (Token op:toks) acc = case backemit toks (acc++[fromEnum op]) of
    Left er -> Left er
    Right result -> Right result
backemit (BackParsingError err:toks) _ = Left err


bkemit :: [Token] -> Either String [Int]
bkemit toks = backemit toks []

-- impure

finish :: Either String [Int] -> IO ()
finish out = do
        case out of
            Left err -> backerror err
            Right res -> writeFile "out.bin" $ intercalate " " (map show res)
        return ()

main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Nothing -> do backerror "Not Enough Arguments."
        Just fname -> finish . bkemit . bkparse . backlex =<< readFile fname
