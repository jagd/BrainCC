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
import Data.List (foldl')

type CodeGen = Writer String ()

loop :: (Num t, Monad m) => t -> m a -> m ()
loop 0 _ = return ()
loop n m = m >> loop (n-1) m

-- | Number of Cells in each Memory Unit (as a constant)
unitElements = 3


data Variable = LocalVar { localOffset :: Int }
              | GlobalVar { globalOffset :: Int }


{------------------------------------------------------------------------------}
-- * Operations

-- ** The framework

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
                   incConstant n -- set the value
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


-- ** Variable operations: define , assign , modify , delete , move to

-- | allocate a new variable at the top of the stack
newVar :: Int
       -- ^ the value of this variable
       -> CodeGen
newVar n = do
           stackLast
           raw ">+" -- move to the stack flag & clean it
           loop (unitElements - 1) $ raw ">" -- move to the next Unit
           raw "[-]" -- reset the value
           incConstant n -- set the value
           raw ">[-]<" -- set the stack top flag & align

-- | push a char into the stack
pushChar :: Char
    -- ^ the value of this variable
    -> CodeGen
pushChar = newVar . fromEnum

stackEnlarge :: Int
             -- ^ Number of addition Units
             -> CodeGen
stackEnlarge n = do
        stackLast
        raw ">" -- move to the stack flag
        loop n f
        raw "<" -- align
    where f = do
              raw "+" -- clean stack top flag
              loop (unitElements) $ raw ">" -- move to the next Unit
              raw "[-]" -- set the stack top flag


stackDrop :: Int
          -- ^ How many Units at the stack top will be droped
          -- this number will not be checked!
          -> CodeGen
stackDrop n = do
              stackLast
              raw ">" -- move to the stack flag cell
              loop (n * unitElements) $ raw "<"
              raw "[-]<" -- set the top flag & align


-- | goto the n\'st variable, forwards or backwards,
--   global variable \'n\' = 0 means the jump register
--   local variable \'n\' = 0 means the stack top unit
gotoVar :: Variable -> CodeGen
gotoVar (LocalVar n) = do
        stackLast
        loop (n * unitElements) $ raw "<"
gotoVar (GlobalVar n) = do
        stackFirst
        loop (n * unitElements) $ raw ">"

setJump :: Int
        -- ^ Jump to
        -> CodeGen
setJump n = do
          stackFirst
          raw "[-]" -- clear
          incConstant n -- set

-- | decreasing the current variable
dec :: CodeGen
dec = raw "-"

-- | increasing the current variable
inc :: CodeGen
inc = raw "+"

incConstant :: Int
            -- ^ the constant Integer to be added
            -> CodeGen
incConstant n = loop n $ raw "+" -- for debug, otherwise with wrapped forms

-- | equivalent as in C: @ a += b @
assignAdd :: Variable
          -- ^ a
          -> Variable
          -- ^ b
          -> CodeGen
assignAdd a b = do
               gotoVar b
               -- clear the temporary cell of var b & align
               raw ">>[-]<<"
               raw "[" -- copy loop
               raw "->>+<<" -- copy to var b's temp
               gotoVar a
               raw "+" -- copy to var a
               gotoVar b
               raw "]"
               gotoVar b -- to recover b's original value
               raw ">>[-<<+>>]<<"


{------------------------------------------------------------------------------}
-- * Translation

-- | generate the Brainf**k Code
genCode :: CodeGen -> String
genCode = pretty . optimationSimple . execWriter

-- | format the code as a block
pretty :: String -> String
pretty code =
        case rest of
             [] -> line ++ eol
             _  -> line ++ eol ++ pretty rest
    where
      (line, rest) = splitAt 80 code
      eol = "\n"

-- | a simple object code optimation,
--   it eliminates the sequences of \"@\<\>@\" and \"@+-@\"
optimationSimple :: String -> String
optimationSimple s = mergeRest
                   $ foldl' f (('#', 0), "")
                   $ filter (`elem` "<>+-.,[]") s
        where
          f :: ((Char, Int), String) -> Char -> ((Char, Int), String)
          f (('<', i), s) '<' = (('<', i + 1), s)
          f (('<', i), s) '>' = (('<', i - 1), s)
          f (('<', i), s) c | i > 0 = ((c, 1), s ++ replicate i '<')
                            | i < 0 = ((c, 1), s ++ replicate (-i) '>')
                            | otherwise = ((c, 1), s)

          f (('>', i), s) '>' = (('>', i + 1), s)
          f (('>', i), s) '<' = (('>', i - 1), s)
          f (('>', i), s) c | i > 0 = ((c, 1), s ++ replicate i '>')
                            | i < 0 = ((c, 1), s ++ replicate (-i) '<')
                            | otherwise = ((c, 1), s)

          f (('+', i), s) '+' = (('+', i + 1), s)
          f (('+', i), s) '-' = (('+', i - 1), s)
          f (('+', i), s) c | i > 0 = ((c, 1), s ++ replicate i '+')
                            | i < 0 = ((c, 1), s ++ replicate (-i) '-')
                            | otherwise = ((c, 1), s)

          f (('-', i), s) '-' = (('-', i + 1), s)
          f (('-', i), s) '+' = (('-', i - 1), s)
          f (('-', i), s) c | i > 0 = ((c, 1), s ++ replicate i '-')
                            | i < 0 = ((c, 1), s ++ replicate (-i) '+')
                            | otherwise = ((c, 1), s)

          f (('#', _), s) c = ((c, 1), s)
          f ((c1, 1), s) c2 = ((c2, 1), s ++ [c1])
          f state _ = error $ "Error occurs by optimation: unmatched "
                           ++ show state

          mergeRest state = snd $ f state '#'
