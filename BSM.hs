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
| 0             value of this variable             |
|                                                  |
| 1             stack top/bottom flag (see later)  |
|               top (rightmost) will be set to 0   |
|               bottom (leftmost) is -1,           |
|               otherwise                          |
|               a small positive number:           |
|               in this case, it would be used as  |
|               a flag for relative addressing.    |
|                                                  |
| 2             temporary variable                 |
+--------------------------------------------------+
@

The first /Unit/ is reserved, the second is a variable, that indicate,
which subprogram to jump into.

After the second /Unit/ follows the user stack.

At the end of each operations, the memory pointer will be aligned at the first
element of a /Memory Unit/.

The flag /unsafe/  means this operation can not be used at the same variables.
Those /unsafe/ functions have prefix \"unsafe\" or a underline \"_\"

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

-- | GlobalVar will be only used for global variables,
--   do not mix it with LocalVar, or the assign will not work rightly.
--   To the Order : ArrayVar is the most expensive and so is PointerVar
data Variable = LocalVar { localOffset :: Int }
              -- ^ counted backwards from the stack top
              | GlobalVar { globalOffset :: Int }
              -- ^ forwards from stack bottom
              | PointerVar Variable
              -- ^ based on the GlobalVar 0
              | ArrayVar {
                        arrayBase   :: Variable,
                        arrayOffset :: Variable
                }
              -- ^ Based on a arbitrary Variable
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

-- | finally goto the /jump/ variable
--   /temp/ of the jump register would be dirty
setJump :: Int
        -- ^ which subprogram to jump into
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


-- ** Variable operations: define , assign , modify , delete , move to

-- | allocate a new variable at the top of the stack
--   and move the current Unit to it.
--   /temp/ would be dirty
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
--   local variable \'n\' = 0 means the stack top /Unit/.
--   For the ArrayVar: inorder to handle a variable like "x[x[i]]",
--   the inner x[i] will be aissigned to a temporary /Unit/ (not a temperory
--   /Cell/) on the stack top. So it could be very expensive
-- FIXME: ArrayVar and when ArrayVar == LocalVar
gotoVar :: Variable -> CodeGen
gotoVar (LocalVar n) = do
        stackLast
        loop (n * unitElements) $ raw "<"
gotoVar (GlobalVar n) = do
        stackFirst
        loop (n * unitElements) $ raw ">"

gotoVar _ = undefined



-- | fix a old variable due to extra offset from the new allocated variables
amendVar :: Int
         -- ^ how many new variables was allocated after the variable @a@
         -> Variable
         -- ^ the variable @a@, which will be amended
         -> Variable
         -- ^ the result variable
amendVar _ v@(GlobalVar _) = v
amendVar n (LocalVar x) = LocalVar (x+n)
amendVar n (PointerVar p) = PointerVar (amendVar n p)
amendVar n (ArrayVar b o) = ArrayVar (amendVar n b) (amendVar n o)

-- | decreasing the current variable
dec :: CodeGen
dec = raw "-"

-- | increasing the current variable
inc :: CodeGen
inc = raw "+"

-- | output a char in the current variable
output :: CodeGen
output = raw "."

-- | input a char in the current variable
input :: CodeGen
input = raw ","

-- | @v = 0@
clearVar :: Variable
         -- ^ v
         -> CodeGen
clearVar v = do
             gotoVar v
             raw "[-]"

-- | /temp/ would be dirty
setCurVar :: Int -> CodeGen
setCurVar n = raw "[-]" >> incConstant n

-- | @v = c@ @c@ is a constant
-- /temp/ would be dirty
setVar :: Variable
       -- ^ the variable @v@
       -> Int
       -- ^ the constant @c@
       -> CodeGen
setVar v n = gotoVar v >> setCurVar n

-- | add a constant to che current /Unit/,
--   which would make the temp of this Unit dirty
incConstant :: Int
            -- ^ the constant Integer to be added
            -> CodeGen
incConstant n | n > 0 = loop n $ raw "+"
              | n < 0 = incConstant (-n)
              | otherwise = return ()


-- | @a = b@, please ensure, that a != b
unsafeAssign :: Variable
       -- ^ a
       -> Variable
       -- ^ b
       -> CodeGen
unsafeAssign a b = clearVar a >> _assignAdd a b

-- | perform logical NOT to the current /Unit/.
-- This operation does not allocate new variables on the stack,
-- but uses temp variables
curLogNOT :: CodeGen
curLogNOT = do
            raw ">>[-]-<<" -- temp := -1
            raw "[>>+<<[-]]" -- if (curr) then temp := 0 else temp keep -1
            -- if (temp) ~~~~ if (!curr.old) then curr = 1 else keep 0
            raw ">>[<<+>>+]<<"


-- | @a = b@
safeAssign :: Variable
       -- ^ a
       -> Variable
       -- ^ b
       -> CodeGen
safeAssign a b | a == b = return ()
safeAssign a@(LocalVar _) b@(LocalVar _) =  unsafeAssign a b
safeAssign a@(GlobalVar _) b@(GlobalVar _) =  unsafeAssign a b
safeAssign a b = do
                 newVar 0
                 let a' = amendVar 1 a
                     b' = amendVar 1 b
                 _assignAdd (LocalVar 0) b'
                 clearVar a
                 _assignAdd b (LocalVar 0)
                 stackDrop 1
-- a@(GlobalVar _) b@(LocalVar _) this situation is not suit for global
-- pointers. So it belongs the last case.


-- | perform logical AND on two variables,
--   @result = a && b@
doLogAND :: Variable
         -- ^ @result@
         -> Variable
         -- ^ @a@
         -> Variable
         -- ^ @b@
         -> CodeGen
doLogAND r a b = do
                 _doLogAND a b
                 unsafeAssign (amendVar 1 r) (LocalVar 0)
                 stackDrop 1

-- | perform logical OR on two variables,
--   @result = a || b@
doLogOR :: Variable
        -- ^ @result@
        -> Variable
        -- ^ @a@
        -> Variable
        -- ^ @b@
        -> CodeGen
doLogOR r a b = do
                _doLogOR a b
                unsafeAssign (amendVar 1 r) (LocalVar 0)
                stackDrop 1

--   @result = a + b@, it is /safe/
doPlus :: Variable
       -- ^ @result@
       -> Variable
       -- ^ @a@
       -> Variable
       -- ^ @b@
       -> CodeGen
doPlus r a b = do
               _doPlus a b
               unsafeAssign (amendVar 1 r) (LocalVar 0)
               stackDrop 1

--   @result = a + b@, it is /safe/
doMinus :: Variable
        -- ^ @result@
        -> Variable
        -- ^ @a@
        -> Variable
        -- ^ @b@
        -> CodeGen
doMinus r a b = do
                _doMinus a b
                unsafeAssign (amendVar 1 r) (LocalVar 0)
                stackDrop 1

-- | the low-level assign: @ a = f(b) @
--   finally goto the variable @b@
--   /unsafe/
_assign :: CodeGen
        -- ^ the mapping f
        -> Variable
        -- ^ a
        -> Variable
        -- ^ b
        -> CodeGen
_assign op a b
        | (a == b) = do  -- this case does not inclusive Array and Pointer !
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

-- | equivalent as in C: @ a += b @.
--   finally goto the variable @b@.
--   /unsafe/
_assignAdd :: Variable
          -- ^ a
          -> Variable
          -- ^ b
          -> CodeGen
_assignAdd = _assign (raw "+")

-- | equivalent as in C: @ a -= b @.
--   finally goto the variable @b@.
--   /unsafe/
_assignMinus :: Variable
          -- ^ a
          -> Variable
          -- ^ b
          -> CodeGen
_assignMinus = _assign (raw "-")


-- | calculate the sum of two variables,
--   @a + b@
--   the result will be a new variable at the stack top.
--   /unsafe/
_doPlus :: Variable
        -- ^ @a@
        -> Variable
        -- ^ @b@
        -> CodeGen
_doPlus a b = do
              newVar 0
              let a' = amendVar 1 a
                  b' = amendVar 1 b
                  res = LocalVar 0
              _assignAdd res a
              _assignAdd res b

-- | calculate the difference of two variables,
--   @a - b@
--   the result will be a new variable at the stack top.
--   /unsafe/
_doMinus :: Variable
         -- ^ @a@
         -> Variable
         -- ^ @b@
         -> CodeGen
_doMinus a b = do
               newVar 0
               let a' = amendVar 1 a
                   b' = amendVar 1 b
                   res = LocalVar 0
               _assignAdd res a
               _assignMinus res b

-- | perform logical AND on two variables,
--   the result will be a new variable at the stack top.
--   Finally located at the stack top /Unit/ (the result).
--   /unsafe/
_doLogAND :: Variable -> Variable -> CodeGen
_doLogAND a b =
        do
          newVar 0
          raw ">>[-]<<" -- res's temp = 0
          let res = LocalVar 0 -- result
              a' = amendVar 1 a
              b' = amendVar 1 b
              far = max a' b' -- optimization
              near = min a' b'
          gotoVar near
          raw "[" -- if (near)
          gotoVar res
          raw ">>+<<" -- res's temp++
          raw "]"
          gotoVar near
          raw "[" -- if (near)
          gotoVar far
          raw "[" -- if (far)
          gotoVar res
          raw ">>+<<" -- res's temp++
          raw "]"
          raw "]"
          gotoVar res
          raw "+>>--" -- res := 1 , res's temp -= 2
          raw "[[+]<<->>]" -- if (res's temp) then temp := 0 , res := 0
          raw "<<" -- align


-- | perform logical OR on two variables,
--   the result will be a new variable at the stack top.
--   Finally located at the stack top /Unit/ (the result).
--   /unsafe/
_doLogOR :: Variable -> Variable -> CodeGen
_doLogOR a b =
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

-- | perform logical OR on variable,
--   the result will be a new variable at the stack top.
--   Finally located at the stack top /Unit/ (the result)
_doLogNOT :: Variable -> CodeGen
_doLogNOT v = do
             newVar 0
             raw ">>[-]+<<" -- new var's temp := 1
             let v' = amendVar 1 v
             gotoVar v'
             raw "[" -- if (v')
             stackLast -- goto the new Variable
             raw ">>-<<" -- new var's temp := 0
             raw "]" -- now: new var == 0
             stackLast -- goto the new Variable
             raw ">>[-<<+>>]<<" -- if (temp) then newVar := 1


-- | @a = a && b@.
--  /unsafe !!/ @a@ and @b@ must be different variables
--  finally locates at @a@
--  /unsafe/ , /obsolete!/
_assignLogAND :: Variable
             -- ^ Variable @a@
             -> Variable
             -- ^ Variable @b@
             -> CodeGen
_assignLogAND a b | a == b = gotoVar a
_assignLogAND a b =
        do
          gotoVar a
          raw ">>[-]<<" -- clear a's temp
          raw "[[-]" -- if (a) then a := 0
          gotoVar b
          raw "[" -- if (b)
          gotoVar a
          raw ">>+<<" -- a's temp ++
          raw "]" -- endif(b)
          gotoVar a
          raw "]" -- endif(a)
          -- located at a
          raw ">>[<<+>>-]<<" -- if (a's temp) then a++, temp-- ; else a = 0


{------------------------------------------------------------------------------}
-- * Translation

-- | generate the Brainf**k Code
genCode :: CodeGen -> String
genCode = pretty . optimizationSimple . execWriter

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
