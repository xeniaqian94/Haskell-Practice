module Assignment3 where

import Parsing
import Control.Monad.State --The State Monad

-- Exercise 1, sample: parse (option 'T' item) "HelloWorld"
option :: a -> Parser a -> Parser a
option x p = do y <- p    
                return y +++ return x -- succeeds return the application result, otherwise if p fails

optionMaybe :: Parser a -> Parser (Maybe a) 
-- liftM :: Monad m => (a -> b) -> (m a -> m b) 
-- lets you use an ordinary function in a monad. 
optionMaybe p = option Nothing (liftM Just p) -- turns a into Maybe a 

-- Exercise 2
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do v <- p    -- at least one occurrence of p is ensured, parse & return 
                  s <- sep  -- ignore the sep & continue
                  vs <- sepBy p sep
                  return (v:vs)


sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep +++ return [] -- if zero occurrence, failure in the left of +++, simply return []

-- You should not modify this definition. But you can add a deriving clause if you want.
type Label = String
type Program = [(Maybe Label, Instruction)]

data Instruction
    = ADD Label -- add accumulator with the value in label
    | SUB Label -- subtract the value in mailbox from the value in the accumulator
    | STA Label -- store from accumulator to mailbox
    | LDA Label -- load from mailbox to accumulator
    | BRA Label -- PC = label
    | BRZ Label -- if accumulator == 0, PC = label
    | BRP Label -- if accumulator positive, PC = label
    | DAT Int -- declare a variable with an initial value
    | INP     -- INBOX get the user input
    | OUT  -- OUTBOX output accumulator
    | HLT -- terminate the program

instance Show Instruction where
  show (ADD x) = "ADD " ++ x
  show (SUB x) = "SUB " ++ x
  show (STA x) = "STA " ++ x
  show (LDA x) = "LDA " ++ x
  show (BRA x) = "BRA " ++ x
  show (BRZ x) = "BRZ " ++ x
  show (BRP x) = "BRP " ++ x
  show (DAT x) = "DAT " ++ show x
  show (INP) = "INP"
  show (OUT) = "OUT"
  show (HLT) = "HLT"

-- Exercise 3
instruction :: Parser Instruction -- define a parser through combining several smaller ones
instruction = parseADD +++ parseSUB +++ parseSTA +++ parseLDA +++ parseBRA +++ parseBRZ +++ parseBRP +++ parseINP +++ parseOUT +++ parseHLT+++ parseDAT  +++ failure

parseADD :: Parser Instruction  -- define auxiliary ADD parsers when defining "instruction"
parseADD = do string "ADD"
              space
              xs <- many1 alphanum
              return (ADD xs)

parseSUB :: Parser Instruction -- define auxiliary SUB parsers when defining "instruction"
parseSUB = do string "SUB"
              space
              xs <- many1 alphanum
              return (SUB xs)

parseSTA :: Parser Instruction  -- define auxiliary STA parsers when defining "instruction"
parseSTA = do string "STA"
              space
              xs <- many1 alphanum
              return (STA xs)

parseLDA :: Parser Instruction  -- define auxiliary LDA parsers when defining "instruction"
parseLDA = do string "LDA"
              space
              xs <- many1 alphanum
              return (LDA xs)

parseBRA :: Parser Instruction -- define auxiliary BRA parsers when defining "instruction"
parseBRA = do string "BRA"
              space
              xs <- many1 alphanum
              return (BRA xs)

parseBRZ :: Parser Instruction  -- define auxiliary BRZ parsers when defining "instruction"
parseBRZ = do string "BRZ"
              space
              xs <- many1 alphanum
              return (BRZ xs)

parseBRP :: Parser Instruction  -- define auxiliary BRP parsers when defining "instruction"
parseBRP = do string "BRP"
              space
              xs <- many1 alphanum
              return (BRP xs)

parseDAT :: Parser Instruction  -- define auxiliary DAT parsers when defining "instruction"
parseDAT = do string "DAT"
              space
              y <- int
              return (DAT y)
           +++ return (DAT 0)

parseINP :: Parser Instruction  -- define auxiliary INP parsers when defining "instruction"
parseINP = do string "INP"
              return INP

parseOUT :: Parser Instruction -- define auxiliary OUT parsers when defining "instruction"
parseOUT = do string "OUT"
              return OUT

parseHLT :: Parser Instruction -- define auxiliary HLT parsers when defining "instruction"
parseHLT = do string "HLT"
              return HLT

parselabel :: Parser (Maybe Label)
parselabel = optionMaybe (many alphanum)


-- Exercise 4

-- parse line "   LOOP ADD 1 "
-- [((Just "LOOP",ADD "1")," ")]

line :: Parser (Maybe Label, Instruction)
line = do label <- optionMaybe (many alphanum) -- if start with a label
          space
          instruct <- instruction
          space   -- ignore any comment
          many (char '/')
          many (sat (==' ') +++ alphanum)
          if label == Just "" then return (Nothing,instruct)
          else return (label, instruct) 

-- parseLMC "LABEL ADD 1 //comments\n SUB 2 //comments\n"
-- [(Just "LABEL",ADD "1"),(Nothing,SUB "2")]
parseLMC :: String -> Program
parseLMC s = case parse (sepBy line (char '\n')) s of --if the String will end with '\n'
               [(p, "")] -> p
               _ -> error "Parse error"

-- Exercise 5
printLine :: (Maybe Label, Instruction) -> String
printLine (Just a, b) = a ++ " " ++ show b 
printLine (Nothing, b) = " " ++ show b 


-- showProgram $ parseLMC " INP \nTHIS STA FIRST //some comments\n HLT \nFIRST DAT 1 //comments\nSECOND DAT 2\n"
showProgram :: Program -> String
showProgram xs = init $ concat [printLine x ++ "\n"|x<-xs]

type Addr = Int
type Accumulator = Maybe Int
type PC = Int
type Mailbox = (String, Int)

data Env
    = Env -- with automatic accessor function e.g. mailboxes :: Env -> [(String, Int)]
    { mailboxes :: [(String, Int)]
    , accumulator :: Accumulator
    , pc :: Addr -- program counter
    , instructions :: [Instruction]
    , labelAddr :: [(String, Int)]
    } deriving (Show)

-- Exercise 6
-- RULE1: DAT instructions without Label will be ignored when initializing the environment
-- RULE2: DAT instructions not followed by any integer are initialised with 0.
isDAT :: (Maybe Label, Instruction) -> Bool
isDAT (Just _, DAT _) = True
isDAT (_,_) = False

extractDAT :: (Maybe Label, Instruction) -> Mailbox
extractDAT (Just x,DAT y) = (x,y)

-- usage: initMailboxes $ parseLMC " INP \nTHIS STA FIRST //some comments\n HLT \nFIRST DAT 1 //comments\nSECOND DAT\n"
-- return: [("FIRST",1),("SECOND",0)]
initMailboxes :: Program -> [Mailbox]
initMailboxes ps = [extractDAT (l,i)| (l,i) <- ps, isDAT (l,i)]

fromJust :: Maybe Label -> Label  -- remove the Just part, with only Label remains
fromJust Nothing = error "filter failure"
fromJust (Just x) = x

isJust :: Maybe Label -> Bool
isJust (Just _) = True
isJust Nothing = False

-- initLabelAddr $ fst $ unzip $ parseLMC " INP \nTHIS STA FIRST //some comments\n HLT \nFIRST DAT 1 //comments\nSECOND DAT 3\n"
-- [("THIS",1),("FIRST",3),("SECOND",4)]
initLabelAddr :: [Maybe Label] -> [(Label, Addr)]
initLabelAddr ps = [(fromJust $ ps !! i , i)|i<-[0..(length ps - 1)],isJust (ps !! i)] 

mkInitEnv :: Program -> Env --initialize the state Env, next instruction pc starts from 1
mkInitEnv program = Env {mailboxes = initMailboxes program, accumulator=Nothing, pc=1, instructions=snd $ unzip program , labelAddr=initLabelAddr $ fst $ unzip program }

type IOEnv = StateT Env IO  -- also, type IOEnv a = StateT Env IO e 
-- Env is the type of the state to carry and IO is the return value

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

decode HLT = return ()

decode (ADD label) = 
    do acc <- getAccumulator
       label_value <- getMailboxes label
       setAccumulator (acc + label_value)
       i <- nextInstruction
       decode i

decode (SUB label) = 
    do acc <- getAccumulator
       label_value <- getMailboxes label
       setAccumulator (acc - label_value)
       i <- nextInstruction
       decode i

decode (BRA label) = updatePCJump label 

decode (BRZ label) =
    do acc <- getAccumulator
       case acc of 
         0 -> updatePCJump label -- if the value in the accumulator is 0
         otherwise -> do i <- nextInstruction
                         decode i

decode (BRP label) =
    do acc <- getAccumulator
       if acc>0 then updatePCJump label -- if the value in the accumulator is positive
       else do i <- nextInstruction
               decode i

decode (STA label) =
    do acc <- getAccumulator
       setMailboxes label acc
       i <- nextInstruction
       decode i

decode (LDA label) =
    do label_value <- getMailboxes label
       setAccumulator label_value
       i <- nextInstruction
       decode i 


updatePCJump :: Label -> IOEnv ()
updatePCJump label = 
    do env <- get -- if the value in the accumulator is 0
       addr <- findAddress label
       put $ env { pc = addr }
       i <- nextInstruction
       decode i

getMailboxes :: Label -> IOEnv Int
getMailboxes label =
    do env <- get
       case [i|(l,i)<-mailboxes env, l==label] of 
         [] -> error "Label undefined"
         [x] -> return x

setMailboxes :: Label -> Int -> IOEnv ()
setMailboxes label acc = 
    do env <- get  
       put $ env {mailboxes = map (\(l,i) -> if l==label then (l,acc) else (l,i)) (mailboxes env) }

findAddress :: Label -> IOEnv Int
findAddress label = 
    do env <- get
       case [i|(l,i)<-labelAddr env, l==label] of
        [] -> error "Label undefined"
        [x] -> return x

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
       put $ env { pc = i + 1 } --increment pc by 1 to the next instruction
       return $ instructions env !! i

evalProgram :: Program -> IO ()
evalProgram [] = return ()
evalProgram p@((_,i):_) = liftM fst $ runStateT (decode i) (mkInitEnv p)
