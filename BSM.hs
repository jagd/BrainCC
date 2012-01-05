{- |

Brainf**k Stack Machine Language

In this text, /Cell/ means a Brainf**k memory element and /Unit/
means the following basic memory structure.

The whole memory is a chain of the /Memory Unit/

The structure of a /Memory Unit/:

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

The first /Unit/ is reserved, the second is a variable, that indicate,
which subprogram to jump into.

After the second /Unit/ follows the user stack.

At the end of each operations, the memory pointer will be aligned at the first
element of a /Memory Unit/.

-}

module BSM where

import Control.Monad.Writer
import Data.List (foldl')

type CodeGen = Writer String ()

loop :: (Num t, Monad m) => t -> m a -> m ()
loop 0 _ = return ()
loop n m = m >> loop (n-1) m

-- | Number of Cells in each /Memory Unit/ (as a constant)
unitElements = 3

data Variable = LocalVar { localOffset :: Int }
              | GlobalVar { globalOffset :: Int }
              deriving (Eq, Ord)


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

-- | see the function `begin`, it only requires aligned at any arbitrary /Unit/
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

-- | allocate a new variable at the top of the stack and move the current Unit to it
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

-- | push a char into the stack and goto it
pushChar :: Char
    -- ^ the value of this variable
    -> CodeGen
pushChar = newVar . fromEnum

stackEnlarge :: Int
             -- ^ Number of addition /Unit/s
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
          -- ^ How many /Unit/s at the stack top will be droped (deleted)
          --   and goto the stack top
          --   this number will not be checked!
          -> CodeGen
stackDrop n = do
              stackLast
              raw ">" -- move to the stack flag cell
              loop (n * unitElements) $ raw "<"
              raw "[-]<" -- set the top flag & align


-- | goto the n\'st variable, forwards or backwards,
--   global variable \'n\' = 0 means the jump register
--   local variable \'n\' = 0 means the stack top /Unit/
gotoVar :: Variable -> CodeGen
gotoVar (LocalVar n) = do
        stackLast
        loop (n * unitElements) $ raw "<"
gotoVar (GlobalVar n) = do
        stackFirst
        loop (n * unitElements) $ raw ">"



-- | fix a old variable due to the extra offset from the new allocated variables
amendVar :: Int
         -- ^ how many new variables was allocated after the variable @a@
         -> Variable
         -- ^ the variable @a@, which will be amended
         -> Variable
         -- ^ the result variable
amendVar _ v@(GlobalVar _) = v
amendVar n (LocalVar x) = LocalVar (x+n)

-- | finally goto the /jump/ variable
setJump :: Int
        -- ^ Jump to
        -> CodeGen
setJump n = do
          stackFirst
          raw "[-]" -- clear
          incConstant n -- set

-- | Do not invoke any functions or goto some where.
--   The program will exit at the end.
--   (Finally goto the /jump/ variable)
clearJump :: CodeGen
clearJump = setJump 0

-- | decreasing the current variable
dec :: CodeGen
dec = raw "-"

-- | increasing the current variable
inc :: CodeGen
inc = raw "+"

setCurVar :: Int -> CodeGen
setCurVar n = raw "[-]" >> incConstant n

setVar :: Variable -> Int -> CodeGen
setVar v n = gotoVar v >> setCurVar n

-- | add a constant to che current /Cell/
incConstant :: Int
            -- ^ the constant Integer to be added
            -> CodeGen
incConstant n | n > 0 = loop n $ raw "+"
              | n < 0 = incConstant (-n)
              | otherwise = return ()


-- | equivalent as in C: @ a += b @.
--   finally goto the variable @b@
assignAdd :: Variable
          -- ^ a
          -> Variable
          -- ^ b
          -> CodeGen
assignAdd = _assign (raw "+")

-- | equivalent as in C: @ a -= b @.
--   finally goto the variable @b@
assignMinus :: Variable
          -- ^ a
          -> Variable
          -- ^ b
          -> CodeGen
assignMinus = _assign (raw "-")

-- | the low-level assign: @ a = f(b) @
--   finally goto the variable @b@
_assign :: CodeGen
        -- ^ the mapping f
        -> Variable
        -- ^ a
        -> Variable
        -- ^ b
        -> CodeGen
_assign op a b
        | (a == b) = do
                gotoVar b
                -- clear the temporary cell of var b & align
                raw ">>[-]<<"
                raw "[->>+<<]" -- copy from b to b's temp
                raw ">>" -- goto b's temp
                raw "[-<<" -- dec(b's temp) then return to b
                raw "+" -- recover b's original value
                op
                raw ">>]" -- goto b's temp
                raw "<<" -- align
        | otherwise = do
                gotoVar b
                -- clear the temporary cell of var b & align
                raw ">>[-]<<"
                raw "[" -- copy loop
                raw "->>+<<" -- copy to var b's temp
                gotoVar a
                op -- modify the var a with op
                gotoVar b
                raw "]"
                gotoVar b -- to recover b's original value
                raw ">>[-<<+>>]<<"

-- | perform logical AND on two variables,
--   the result will be a new variable at the stack top.
--   Finally located at the stack top /Unit/ (the result)
doLogAND :: Variable -> Variable -> CodeGen
doLogAND a b =
        do
          newVar 0
          raw ">>[-]<<" -- res's temp = 0
          let res = LocalVar 0 -- result
              a' = amendVar 1 a
              b' = amendVar 1 b
              far = max a' b' -- optimization
              near = min a' b'
          assignAdd res far
          gotoVar res
          raw "[" -- if (res) , equivalent to if (far)
          raw "[-]" -- res := 0
          assignAdd res near
          gotoVar res
          raw "[->>+<<][-]" -- copy res to res'tmp, then res := 0
          raw "]" -- endif (res) , equivalent to endif (far)
          raw ">>[-<<+>>]<<" -- copy res's tmp to res

-- | perform logical OR on two variables,
--   the result will be a new variable at the stack top.
--   Finally located at the stack top /Unit/ (the result)
doLogOR :: Variable -> Variable -> CodeGen
doLogOR a b =
        do
          newVar 0
          raw ">>[-]<<" -- res's temp := 0
          let res = LocalVar 0 -- result
              a' = amendVar 1 a
              b' = amendVar 1 b
              far = max a' b' -- optimization
              near = min a' b'
          -- @ res
          gotoVar near
          raw "[" -- if (near)
          gotoVar res
          raw ">>+<<"
          raw "]"
          gotoVar far
          raw "[" -- if (far)
          gotoVar res
          raw ">>+<<"
          raw "]"
          gotoVar res -- until now, res == 0
          raw ">>[[-]<<+>>]<<"

{------------------------------------------------------------------------------}
-- * Translation

-- | generate the Brainf**k Code
genCode :: CodeGen -> String
-- genCode = pretty . optimizationSimple . execWriter
genCode = pretty . execWriter

-- | format the code as a block
pretty :: String -> String
pretty code =
        case rest of
             [] -> line ++ eol
             _  -> line ++ eol ++ pretty rest
    where
      (line, rest) = splitAt 80 code
      eol = "\n"

-- | a simple object code optimaztion,
--   it eliminates the sequences of \"@\<\>@\" and \"@+-@\"
optimizationSimple :: String -> String
optimizationSimple s = mergeRest
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
          f state _ = error
                    $ "Error occurs by simple optimization: unmatched "
                    ++ show state

          mergeRest state = snd $ f state '#'
