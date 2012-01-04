{- |

Brainf**k Stack Machine Language

The whole memory is a chain of the \'Memory Unit\'

The structure of a \'Memory Unit\':

@
+--------------------------------------------------+
| Offset        Field                              |
+--------------------------------------------------+
| 0             value of the variable              |
|                                                  |
| 1             stack top/bottom flag (see later)  |
|               top (rightmost) will be set to 0   |
|               bottom (leftmost) is -1            |
|               otherwise, a small positive number |
|                                                  |
| 2             temporary variable                 |
+--------------------------------------------------+
@


At the End of each operations, the memory pointer will be aligned at the first
element of a 'Memory Unit'


-}

module BSM where

import Control.Monad.Writer

type CodeGen = Writer String ()

loop :: (Num t, Monad m) => t -> m a -> m ()
loop 0 _ = return ()
loop n m = m >> loop (n-1) m

-- | Number of Cells in each Memory Unit (as a constant)
unitElements = 3

{------------------------------------------------------------------------------}
-- * operations

-- ** the framework

-- | init the framework
begin :: CodeGen
      -- ^ Init procedure for the user code (e.g. global variable allocation)
      -> CodeGen
begin m = do
          varBottom
          varFirst 1 -- the jump register
          m -- user init code
          raw "[" -- start the global loop
    where
      -- the variable, reserved for the stack bottom
      varBottom :: CodeGen
      varBottom = do
                  raw ">[-]-" -- set the stack bottom flag
                  -- return to the next cell and align
                  loop (unitElements - 1) $ raw ">"
      -- the variable, reserved for the stack bottom
      varFirst n = do
                   raw "[-]" -- clear the value
                   loop n $ raw "+" -- set the value
                   raw ">[-]<" -- set the stack top flag and align

-- | see the function `begin`
end :: CodeGen
end = stackFirst
   >> raw "]"

-- | jump the first (leftmost) element of the Stack ( the jump register )
stackFirst :: CodeGen
stackFirst = do
             raw ">+[-" -- move to the flag cell, and look whether it was -1
             -- move left with the length of a Memory Unit
             loop unitElements $ raw "<"
             raw "+]-" -- untill the -1 mark, reset it to -1
             -- skip the reserved Unit with align
             loop (unitElements - 1) $ raw ">"

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


-- ** variable operations: define , assign , modify , delete , move to

-- | decreasing the current variable
dec :: CodeGen
dec = raw "-"

-- | increasing the current variable
inc :: CodeGen
inc = raw "+"

-- | allocate a new variable at the top of the stack
newVar :: Int
       -- ^ the value of this variable
       -> CodeGen
newVar n = do
           stackLast
           raw ">+" -- move to the stack flag & clean it
           loop (unitElements - 1) $ raw ">" -- move to the next Unit
           raw "[-]" -- reset the value
           loop n $ raw "+" -- set the value
           raw ">[-]<" -- set the stack top flag & align

-- | push a char into the stack
pushChar :: Char
    -- ^ the value of this variable
    -> CodeGen
pushChar = newVar . fromEnum


stackDrop :: Int
          -- ^ How many Units at the stack top will be droped
          -- this number will not be checked!
          -> CodeGen
stackDrop n = do
              stackLast
              raw ">" -- move to the stack flag cell
              loop (n * unitElements) $ raw "<"
              raw "[-]<" -- set the top flag & align


-- | goto the n\'st variable, backwards, \'n\' starts from 0
lastVar :: Int
        -- ^ the \'n\'
        -> CodeGen
lastVar n = do
            stackLast
            loop (n * unitElements) $ raw "<"

-- | goto the n\'st variable, forwards, \'n\' = 0 means the jump register
globalVar :: Int
          -- ^ the \'n\'
          -> CodeGen
globalVar n = do
              stackFirst
              loop (n * unitElements) $ raw ">"

{------------------------------------------------------------------------------}
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
