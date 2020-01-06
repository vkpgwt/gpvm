module VM.State (State, withVM, step, run) where
import           Data.Int (Int8)
import           VM
import qualified VM.Instruction as I

data State = State { code :: Code
                   , stack :: [VM.Word]
                   , sp :: Int
                   , pc :: Int
                   } deriving (Show)

data StepResult = StepOk | StepEndInstruction
                  deriving (Show)

data RunResult = RunEnded | RunMaxInstructionsReached
                 deriving (Show)

stackSize = 16

withVM :: VM -> Maybe State
withVM (VM [])   = Nothing
withVM (VM code) = Just State { code = code
                              , stack = map (const 0) [1..stackSize]
                              , sp = 0
                              , pc = 0
                              }

run :: Int -> State -> (State, RunResult)
run maxSteps state
    | maxSteps <= 0 = (state, RunMaxInstructionsReached)
    | otherwise     = case stepResult of
                          StepOk             -> run (maxSteps - 1) newState
                          StepEndInstruction -> (newState, RunEnded)
    where (newState, stepResult) = (step state)

step :: State -> (State, StepResult)
step state = (transform fs state, result)
    where (fs, result) = case opcode of
                             I.LoadConst -> ( [ putToStack 0 (fromIntegral arg)
                                              , incSP spDelta
                                              , incPC 1]
                                            , StepOk)
                             I.End       -> ([incSP spDelta], StepEndInstruction)

          instruction = code state !! pc state
          opcode      = I.opcode instruction
          arg         = fromIntegral (I.arg instruction) :: Int
          spDelta     = fromIntegral (I.spDelta instruction) :: Int

transform :: [State -> State] -> State -> State
transform fs = foldl (.) id (reverse fs)

putToStack :: Int -> VM.Word -> State -> State
putToStack relIdx value state = state { stack = newStack }
    where newStack = listReplaceAt (stack state) (stackAbsIndex relIdx state) value

stackAbsIndex :: Int -> State -> Int
stackAbsIndex relIdx state = (sp state + relIdx) `mod` stackSize

listReplaceAt :: [a] -> Int -> a -> [a]
listReplaceAt list idx value = prefix ++ (value:suffix)
    where (prefix, (_:suffix)) = splitAt idx list

incSP :: Int -> State -> State
incSP increment state = state { sp = stackAbsIndex increment state }

incPC :: Int -> State -> State
incPC increment state = state { pc = (pc state + increment) `mod` codeLength state }

codeLength state = length $ code state
