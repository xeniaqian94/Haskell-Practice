module Assignment3 where

import Parsing
import Control.Monad.State

-- Exercise 1
option :: a -> Parser a -> Parser a
option x p = 

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = undefined

-- Exercise 2
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 = undefined

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy = undefined

-- You should not modify this definition. But you can add a deriving clause if you want.
type Label = String
type Program = [(Maybe Label, Instruction)]

data Instruction
    = ADD Label
    | SUB Label
    | STA Label
    | LDA Label
    | BRA Label
    | BRZ Label
    | BRP Label
    | DAT Int
    | INP
    | OUT
    | HLT

-- Exercise 3
instruction :: Parser Instruction
instruction = undefined

-- Exercise 4
line :: Parser (Maybe Label, Instruction)
line = undefined

parseLMC :: String -> Program
parseLMC s = case parse (sepBy line (char '\n')) s of
               [(p, "")] -> p
               _ -> error "Parse error"

-- Exercise 5
showProgram :: Program -> String
showProgram = undefined

type Addr = Int
type Accumulator = Maybe Int
type PC = Int
type Mailbox = (String, Int)

data Env
    = Env
    { mailboxes :: [(String, Int)]
    , accumulator :: Accumulator
    , pc :: Addr -- program counter
    , instructions :: [Instruction]
    , labelAddr :: [(String, Int)]
    }

-- Exercise 6
initMailboxes :: Program -> [Mailbox]
initMailboxes = undefined

initLabelAddr :: [Maybe Label] -> [(Label, Addr)]
initLabelAddr = undefined

mkInitEnv :: Program -> Env
mkInitEnv = undefined

type IOEnv = StateT Env IO

-- Exercise 7
decode :: Instruction  -> IOEnv ()
decode INP =
    do val <- liftIO (readLn :: IO Int)
       setAccumulator val
       i <- nextInstruction
       decode i
decode OUT =
    do acc <- getAccumulator
       liftIO $ print acc
       i <- nextInstruction
       decode i
decode _ = undefined

setAccumulator :: Int -> IOEnv ()
setAccumulator acc =
    do env <- get
       put $ env { accumulator = Just acc }

getAccumulator :: IOEnv Int
getAccumulator =
    do env <- get
       case accumulator env of
         Just i -> return i
         Nothing -> error "Nothing in the accumulator"

nextInstruction :: IOEnv Instruction
nextInstruction =
    do env <- get
       let i = pc env
       when (i == length (instructions env)) $ error "No more instructions"
       put $ env { pc = i + 1 }
       return $ instructions env !! i

evalProgram :: Program -> IO ()
evalProgram [] = return ()
evalProgram p@((_,i):_) = liftM fst $ runStateT (decode i) (mkInitEnv p)
