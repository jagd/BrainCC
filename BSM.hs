{- |

Brainf**k Stack Machine Language

At the End of each operations, the memory pointer will be aligned at the first
element of a 'Memory Unit'

-}

module BSM where

import Control.Monad.Writer
import Control.Monad (forM_)

type CodeGen = Writer String ()

loop :: (Num t, Monad m) => t -> m a -> m ()
loop 0 _ = return ()
loop n m = m >> loop (n-1) m

-- | Number of Cells in each Memory Unit (as a constant)
unitElements = 3

{------------------------------------------------------------------------------}
-- * operations


-- | init the framework
begin :: CodeGen
      -- ^ Init procedure for the user code (e.g. global variable allocation)
      -> CodeGen
begin m = do
        varInit 1 -- the jump register
        m -- user init code
        raw "[" -- start the global loop
    where
      -- | the first global variable
      varInit :: Int -> CodeGen
      varInit n = do
                  raw "[-]" -- clean the value
                  loop n $ raw "+" -- set the value to n
                  raw ">[-]" -- set the stack bottom flag
                  raw "<" -- return to the align cell

-- | see the function `begin`
end :: CodeGen
end = stackFirst
   >> raw "]"

-- | jump the first (leftmost) element of the Stack
stackFirst :: CodeGen
stackFirst = do
             raw ">[" -- move to the flag cell
             -- move left with the length of a Memory Unit
             loop unitElements $ raw "<"
             raw "]<" -- align

-- | jump the last (rightmost) element of the Stack
stackLast :: CodeGen
stackLast = do
             raw ">[" -- move to the flag cell
             -- move right with the length of a Memory Unit
             loop unitElements $ raw ">"
             raw "]<" -- align


-- | generate the raw code
raw :: String
    -- ^ any Brainf**k code
    -> CodeGen
raw = tell

-- | allocate a new variable at the top of the stack
var :: Int
    -- ^ the value of this variable
    -> CodeGen
var n = do
        stackLast
        raw ">+" -- move to the stack flag
        loop (unitElements - 1) $ raw "+" -- clean the stack top flag
        raw "[-]" -- reset the value
        loop n $ raw "+" -- set the value
        raw ">[-]" -- set the stack top flag
        raw "<<" -- return to the value cell

{------------------------------------------------------------------------------}
-- * translation

-- * translation

-- | generate the Brainf**k Code
genCode :: CodeGen -> String
genCode = prettyCode . execWriter

-- | format the code as a block
prettyCode :: String -> String
prettyCode code =
        case rest of
             [] -> line ++ eol
             _  -> line ++ eol ++ prettyCode rest
    where
      (line, rest) = splitAt 80 code
      eol = "\n"
