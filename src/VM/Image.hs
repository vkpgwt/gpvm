module VM.Image (Image, withVM, step, run) where
import           Data.Array
import           Data.Int (Int8)
import           VM
import qualified VM.Instruction as I

data Image = Image { code :: Code
                   , stack :: Array Int VM.Word
                   , sp :: Int
                   , pc :: Int
                   } deriving (Show)

data StepResult = StepOk | StepEndInstruction
                  deriving (Show)

data RunResult = RunEnded | RunMaxInstructionsReached
                 deriving (Show)

stackSize = 16

withVM :: VM -> Maybe Image
withVM (VM code)
    | null (assocs code) = Nothing
    | otherwise = Just Image { code = code
                             , stack = listArray (0, stackSize-1) (repeat 0)
                             , sp = 0
                             , pc = 0
                             }

run :: Int -> Image -> (Image, RunResult)
run maxSteps image
    | maxSteps <= 0 = (image, RunMaxInstructionsReached)
    | otherwise     = case stepResult of
                          StepOk             -> run (maxSteps - 1) newImage
                          StepEndInstruction -> (newImage, RunEnded)
    where (newImage, stepResult) = (step image)

step :: Image -> (Image, StepResult)
step image = (transform fs image, result)
    where (fs, result) = case opcode of
                             I.LoadConst -> ( [ putToStack 0 (fromIntegral arg)
                                              , incSP spDelta
                                              , incPC 1]
                                            , StepOk)
                             I.End       -> ([incSP spDelta], StepEndInstruction)

          instruction = code image ! pc image
          opcode      = I.opcode instruction
          arg         = fromIntegral (I.arg instruction) :: Int
          spDelta     = fromIntegral (I.spDelta instruction) :: Int

transform :: [Image -> Image] -> Image -> Image
transform fs = foldl (.) id (reverse fs)

putToStack :: Int -> VM.Word -> Image -> Image
putToStack relIdx value image = image { stack = newStack }
    where newStack = (stack image) // [(stackAbsIndex relIdx image, value)]

stackAbsIndex :: Int -> Image -> Int
stackAbsIndex relIdx image = (sp image + relIdx) `mod` stackSize

incSP :: Int -> Image -> Image
incSP increment image = image { sp = stackAbsIndex increment image }

incPC :: Int -> Image -> Image
incPC increment image = image { pc = (pc image + increment) `mod` codeLength image }

codeLength image = length $ code image
