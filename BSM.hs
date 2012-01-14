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
|               otherwise a small positive number. |
|                                                  |
| 2             temporary variable                 |
|                                                  |
| 3             counter for relative addressing    |
+--------------------------------------------------+
@

The first /Unit/ is reserved, the second is a variable, that indicate,
which subprogram to jump into.

After the second /Unit/ follows the user stack.

At the end of each operations, the memory pointer will be aligned at the first
element of a /Memory Unit/.

The flag /unsafe/  means this operation can not be used at the same variables.

-}

module BSM where

import Control.Monad.Writer
import Data.List (foldl')

type CodeGen = Writer String ()

loop :: (Ord t, Num t, Monad m) => t -> m a -> m ()
loop 0 _ = return ()
loop n _ | n < 0 = error "negative (infinity) loop"
loop n m = m >> loop (n-1) m

-- | Number of Cells in each /Memory Unit/ (as a constant)
unitElements = 4

-- | GlobalVar will be only used for global variables,
--
--   Do not mix it with LocalVar, or the (unsafe) assigns may not work rightly.
--
--   The Order:
--
--   * ArrayVar is the most expensive and so is PointerVar.
--
--   * GlobalVar is expensiver als LocalVar
--
--   * (LocalVar @n@) is expensiver als (LocalVar @n+1@)
--
--   * (GlobalVar @n@) is expensiver als (GlobalVar @n+1@)
data Variable = LocalVar { localOffset :: Int }
              -- ^ counted backwards from the stack top
              | GlobalVar { globalOffset :: Int }
              -- ^ forwards from stack bottom
              | ArrayVar {
                        -- | This variable is the base variable.
                        --   The addressing process do not care what value it has.
                        arrayBase   :: Variable,
                        -- | the /runtime value/ of this variable
                        --   will be treated as offset
                        arrayOffset :: Variable
                }
              -- ^ Based on a arbitrary Variable,
              --   grows from left to right (forwards).
              --
              --   Array will be indexed with a variable,
              --   or it is a  LocalVar \/ GlobalVar with constant (negative)
              --   offset.
              --
              --   Pointers are also Arrays, usually they will be some
              --   offset address, starting from a GlobalVar, the base Variable
              --   can be e.g. (GlobalVar 0).
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

-- | Jump to the first (leftmost) element of the Stack ( the global jump mark )
--
--   Time: @O(s)@
stackFirst :: CodeGen
stackFirst = do
             raw ">+[-" -- move to the flag cell, and look whether it was -1
             -- move left with the length of a Memory Unit
             unitLeft
             raw "+]-" -- untill the -1 mark, reset it to -1
             -- skip the reserved Unit with align
             loop (unitElements - 1) $ raw ">"

-- | Jump to the last (rightmost) element of the Stack
--
--   Time: @O(s)@
stackLast :: CodeGen
stackLast = do
             raw ">[" -- move to the flag cell
             -- move right with the length of a Memory Unit
             unitRight
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
--
--   The program will exit at the end.
clearJump :: CodeGen
clearJump = setJump 0


-- ** Variable operations: define , assign , modify , delete , move to

-- | Push a constant @n@ at the top of the stack.
--   This process would use several new temp variables
--   and make the temp of this Unit dirty.
--
--   The bigger @n@ is, the longer time will this process cost.
--   Time costs do not grow linear during the increasing of @n@.
--   For the bigger @n@, this process will be more and more expansive.
--   In turns, the code length will not grow rapidly.
newVar :: Int
       -- ^ n
       -> CodeGen
newVar n = do
           stackLast
           raw ">+" -- move to the stack flag & clean it
           loop (unitElements - 1) $ raw ">" -- move to the next Unit
           raw "[-]" -- the target := 0
           f n -- dirty assign
           raw ">[-]<" -- set the stack top flag & align
        where
           base = 10
           neg "+" = "-"
           neg "-" = "+"
           f n | n > 0 = g n "+"
               | n < 0 = g (-n) "-"
               | otherwise = return ()
           g n op | n == 0 = return () -- this case will not be executed
                  | n <= base = loop n $ raw op
                    -- (n == base) inclusived,
                    -- the next cell will not be 0
                  | otherwise = do
                                loop (n `rem` base) $ raw op
                                raw ">[-]"
                                g (n `div` base) op
                                raw "["
                                raw $ neg op
                                raw "<"
                                loop base $ raw op
                                raw ">]"
                                raw "<"

-- | Push a char into the stack and goto it.
--   It is only a wrap for 'newVar'
pushChar :: Char
    -- ^ the value of this variable
    -> CodeGen
pushChar = newVar . fromEnum


-- | Push a copy of the Variable @x@ onto the stack top.
--   As expensive as a normal addition on @x@
dupVar :: Variable
       -- ^ @x@
       -> CodeGen
dupVar x = do
           newVar 0
           _assignAdd (LocalVar 0) (amendVar 1 x)

stackEnlarge :: Int
             -- ^ Number of addition /Unit/s on the stack top
             -> CodeGen
stackEnlarge n = do
        stackLast
        raw ">" -- move to the stack flag
        loop n f
        raw "<" -- align
    where f = do
              raw "+" -- clean stack top flag
              unitRight -- move to the next Unit
              raw "[-]" -- set the stack top flag

stackDrop :: Int
          -- ^ How many /Unit/s at the stack top will be droped (deleted).
          --   Then goto the stack top.
          --
          --   /Warning/: This number will not be checked!
          -> CodeGen
stackDrop n = do
              stackLast
              raw ">" -- move to the stack flag cell
              loop (n * unitElements) $ raw "<"
              raw "[-]<" -- set the top flag & align

-- | move one Unit length towards right.
--
--   Time: @O(1)@
unitRight :: CodeGen
unitRight = loop unitElements $ raw ">"


-- | move one Unit length towards left
--
--   time: @O(1)@
unitLeft :: CodeGen
unitLeft = loop unitElements $ raw "<"


-- | Goto the n\'st variable, forwards or backwards:
--
--   Global variable \'n\' = 0 means the jump register
--
--   * Time ('GlobalVar') : @O(s)@
--
--   Local variable \'n\' = 0 means the stack top /Unit/
--
--   * Time ('LocalVar') : @O(s)@
--
--   It could be very expensive to adress a ArrayVar
--
--   * Time on 'ArrayVar' : expensiver than @O(s*s)@,
--   @s@ is the stack length (or the base \/ offset locations)
gotoVar :: Variable -> CodeGen

gotoVar (LocalVar n) = do
        stackLast
        loop (n * unitElements) $ raw "<"

gotoVar (GlobalVar n) = do
        stackFirst
        loop (n * unitElements) $ raw ">"

gotoVar (ArrayVar (ArrayVar bbase boffset) offset) = do
        dupVar boffset
        -- init the sum with boffset
        -- the later offset will be add on it
        _gotoArrayVar bbase offset

-- The pattern:
--     gotoVar (ArrayVar base (ArrayVar _ _))
-- is in the commen pattern inclusived

gotoVar (ArrayVar base offset) = do -- otherwise
        newVar 0
        _gotoArrayVar base offset

-- the low-level implementation of gotoArrayVar or
-- gotoVar with a ArrayVar argument.
-- only makes the Indexing Cell dirty
_gotoArrayVar base offset = do
        -- newVar 0 -- clone of the offset, should be done at higher level
        let offset' = amendVar 1 offset
            base' = amendVar 1 base
            cloneOffset = (LocalVar 0)
        _assignAdd cloneOffset offset'
        gotoVar base'
        raw ">>>[-]<<<" -- clean base's counter
        gotoVar cloneOffset
        raw "[" -- add the cloneOffset to base's counter
        raw "-" -- cloneOffset--
        gotoVar base'
        raw ">>>+<<<" -- base's counter++
        gotoVar cloneOffset
        raw "]"
        stackDrop 1
        -- cloneOffset deleted, this step must be done right now
        gotoVar base -- the unfixed base
        raw ">>>" -- move to its counter
        -- whether it is the top of the stack will not be guaranteed
        -- because Array (LocalVar 0) (0) is also legal
        raw "["  -- if its counter > 0
        -- the next's flag := 0
        unitRight -- move to next Unit's counter
        raw "[-]" -- next's counter := 0
        unitLeft -- move back
        raw "[" -- carry the rest of counter to the next Unit
        raw "-"
        unitRight -- move to next Unit
        raw "+"
        unitLeft -- move back
        raw "]"
        -- now located at the lefter Unit
        unitRight -- move to next Unit
        raw "-" -- next's counter--
        raw "]" -- the final position arrived
        raw "<<<" -- align


-- | To fix a old variable due to extra offset from the new allocated stack
--   elements
--
--   /This function does not directly influence the runtime brainf**k code./
amendVar :: Int
         -- ^ how many new variables was allocated after the variable @a@
         -> Variable
         -- ^ the variable @a@, which will be amended
         -> Variable
         -- ^ the result variable
amendVar _ v@(GlobalVar _) = v
amendVar n (LocalVar x) = LocalVar (x+n)
amendVar n (ArrayVar b o) = ArrayVar (amendVar n b) (amendVar n o)

-- | @v := 0@
clearVar :: Variable
         -- ^ v
         -> CodeGen
clearVar v = do
             gotoVar v
             raw "[-]"

-- | @v := c@
setVar :: Variable
       -- ^ the variable @v@
       -> Int
       -- ^ the constant @c@
       -> CodeGen
setVar v n = newVar n
          >> unsafeAssign v (LocalVar 0)
          >> stackDrop 1

-- | Simply do constant numbers of \"+\" to the current /Cell/,
incConstant :: Int
            -- ^ the constant Integer to be added
            -> CodeGen
incConstant n | n > 0 = loop n $ raw "+"
              | n < 0 = loop (-n) $ raw "-"
              | otherwise = return ()


-- | Assign @a := b@
--
-- Please ensure @a@ and @b@ are not the same /Unit/!
unsafeAssign :: Variable
       -- ^ @a@
       -> Variable
       -- ^ @b@
       -> CodeGen
unsafeAssign a b = clearVar a >> _assignAdd a b


-- | @a := b@
safeAssign :: Variable
       -- ^ @a@
       -> Variable
       -- ^ @b@
       -> CodeGen
safeAssign a b | a == b = return ()
safeAssign a@(LocalVar _) b@(LocalVar _) =  unsafeAssign a b
safeAssign a@(GlobalVar _) b@(GlobalVar _) =  unsafeAssign a b
-- a@(GlobalVar _) b@(LocalVar _) this situation is not suit for global
-- pointers. So it belongs the last case.
safeAssign a b = do
                 dupVar b
                 let a' = amendVar 1 a
                 clearVar a'
                 _assignAdd a' (LocalVar 0)
                 stackDrop 1


-- | do logical NOT on the current /Unit/.
--
-- This operation does not allocate new variables on the stack,
-- but uses temp variables
curLogNOT :: CodeGen
curLogNOT = do
            raw ">>[-]-<<" -- temp := -1
            raw "[>>+<<[-]]" -- if (curr) then temp := 0 else temp keep -1
            -- if (temp) ~~~~ if (!curr.old) then curr = 1 else keep 0
            raw ">>[<<+>>+]<<"

-- | @result := !x@
--
-- it is /safe/, but expansive
doLogNOT :: Variable
         -- ^ @result@
         -> Variable
         -- ^ @x@
         -> CodeGen
doLogNOT r x = do
               _doLogNOT x
               unsafeAssign (amendVar 1 r) (LocalVar 0)
               stackDrop 1

-- | logical NOT on any variable,
--
--   the result will be a new variable at the stack top.
--
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

-- | @result := a && b@
--
-- it is /safe/, but expansive
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

-- | logical AND on two variables,
--   the result will be a new variable at the stack top.
--
--   Finally located at the stack top /Unit/ (the result).
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


-- | @result := a || b@
--
-- it is /safe/, but expansive
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

-- | logical OR on two variables,
--
--   the result will be a new variable at the stack top.
--
--   Finally located at the stack top /Unit/ (the result).
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

-- | evaluate expression @(a > b)@ (unsigned) ,
--   the result will be stored in @r@
doGT :: Variable
      -- ^ @r@
      -> Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
doGT r a b = do
             _doGT a b
             unsafeAssign (amendVar 1 r) (LocalVar 0)
             stackDrop 1

-- | evaluate expression @(a >= b)@ (unsigned) ,
--   the result will be stored in @r@
doGE :: Variable
      -- ^ @r@
      -> Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
doGE r a b = do
             _doGE a b
             unsafeAssign (amendVar 1 r) (LocalVar 0)
             stackDrop 1

-- | evaluate expression @(a < b)@ (unsigned) ,
--   the result will be stored in @r@
doLT :: Variable
      -- ^ @r@
      -> Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
doLT r a b = do
             _doLT a b
             unsafeAssign (amendVar 1 r) (LocalVar 0)
             stackDrop 1

-- | evaluate expression @(a <= b)@ (unsigned) ,
--   the result will be stored in @r@
doLE :: Variable
      -- ^ @r@
      -> Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
doLE r a b = do
             _doLE a b
             unsafeAssign (amendVar 1 r) (LocalVar 0)
             stackDrop 1


-- | evaluate @a > b@ (unsigned) ,
--   the result will pushed in the stack as a new local variable
_doGT :: Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
_doGT a b = do
            newVar 0
            newVar 0
            newVar 0
            let a' = amendVar 3 a
                b' = amendVar 3 b
                res  = (LocalVar 2)
                tmp1 = (LocalVar 1)
                tmp2 = (LocalVar 0) -- always = 0, as quit var
            _assignAdd res a'
            _assignAdd tmp1 b'

            gotoVar res
            raw ">>[-]<<" -- res's temp = 0 :: loop flag

            raw "[" -- if (res)
            raw ">>+<<" -- then res's temp := 1
            gotoVar tmp2 -- quit
            raw "]"

            gotoVar res
            raw ">>[-<<" -- if (res's temp == 1) then res's temp := 0 and align

            raw "[" -- if (res)
            unitRight -- goto tmp1
            raw "[" -- if (tmp1)
            raw "-" -- tmp1--
            unitLeft
            raw "-" -- res--
            raw ">>+<<" -- res.temp++ (== 1)
            gotoVar tmp2
            raw "]"
            gotoVar tmp2
            raw "]"

            gotoVar res
            raw ">>]<<"

            -- if (res.now != 0) then a > b else a <= b
            stackDrop 2

-- | evaluate @a < b@ (unsigned) ,
--   the result will pushed in the stack as a new local variable
_doLT :: Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
_doLT = flip _doGT

-- | evaluate @a >= b@ (unsigned) ,
--   the result will pushed in the stack as a new local variable
_doGE :: Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
_doGE a b = _doLT a b >> curLogNOT

-- | evaluate @a <= b@ (unsigned) ,
--   the result will pushed in the stack as a new local variable
_doLE :: Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
_doLE a b = _doGT a b >> curLogNOT



-- @result := a + b@
--
-- it is /safe/, but expansive
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

-- | calculate the sum of two variables,
--
--   @a + b@
--
--   the result will be a new variable at the stack top.
_doPlus :: Variable
        -- ^ @a@
        -> Variable
        -- ^ @b@
        -> CodeGen
_doPlus a b = do
              dupVar a
              let b' = amendVar 1 b
                  res = LocalVar 0
              _assignAdd res b'


-- @result := a + b@
--
-- it is /safe/, but expansive
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

-- | calculate the difference of two variables,
--
--   @a - b@
--
--   the result will be a new variable at the stack top.
_doMinus :: Variable
         -- ^ @a@
         -> Variable
         -- ^ @b@
         -> CodeGen
_doMinus a b = do
               dupVar a
               let b' = amendVar 1 b
                   res = LocalVar 0
               _assignMinus res b'


-- @result := a * b@
--
-- Time : expensiver than @O(a*b)@
doMul :: Variable
      -- ^ @result@
      -> Variable
      -- ^ @a@
      -> Variable
      -- ^ @b@
      -> CodeGen
doMul r a b = do
                _doMul a b
                unsafeAssign (amendVar 1 r) (LocalVar 0)
                stackDrop 1

-- | calculate @a * b@ (unsigned),
--   the result will be stored in a new variable at the top of the stack
--
-- Time : expensiver than @O(a*b)@
_doMul a b = do
             newVar 0 -- the result
             dupVar (amendVar 1 a) -- if not clone `a` and `b`,
             dupVar (amendVar 2 b) --    it will be very expensive for Arrays

             -- because of the speed and the overhead
             -- the following code will not use
             -- those high-level functions such as gotoVar

             stackLast --b'
             unitLeft -- a'
             raw ">>[-]<<" -- a'.temp := 0
             unitRight

             raw "[" -- while (b' > 0)
             raw "-" --   then b'--
             unitLeft -- goto a'

             raw "[" -- while (a' > 0)
             raw "-" -- a'--
             raw ">>+<<" -- a'.temp++
             unitLeft -- result
             raw "+" -- result++
             unitRight -- to a'
             raw "]" -- loop (a')

             -- now, a' == 0
             raw ">>[-<<+>>]<<" -- a' <= a'.temp
             unitRight -- to b'
             raw "]" -- loop (b')

             stackDrop 2 -- drop a' and b'

-- | evaluate @a / b@ (unsigned), assumed @b != 0@.
--   The quotient will first be pushed.
--   At last, the remainder will be pushed.
_doDivRem a b = do
                -- to avoid array indexing
                -- only manipulate local variables
                newVar 0 -- quotient
                dupVar (amendVar 1 a) -- a'
                dupVar (amendVar 2 b) -- b'
                newVar 0 -- tmp1
                newVar 0 -- tmp2, tmp2.temp = dummy (tmp2)

                let quot = LocalVar 4
                    a' = LocalVar 3
                    b' = LocalVar 2
                    tmp1 = LocalVar 1
                    tmp2 = LocalVar 0

                gotoVar a'
                raw ">>[-]<<" -- a'.temp := 0
                raw "[" -- while (a')
                raw "-" -- a'--
                raw ">>[-]+<<" -- a'.temp := 1

                unitRight -- b'
                unitRight -- tmp1
                unitRight -- tmp2
                raw ">>[-]-<<" -- tmp2.temp := -1

                unitLeft -- goto tmp1
                raw "[" -- if (tmp1)
                raw "-" -- tmp1--
                unitRight -- tmp2
                raw ">>+<<" -- tmp2.temp := 0 then align
                raw "]" -- endif at tmp2

                gotoVar tmp2

                -- if (tmp2.temp == -1) then tmp2.temp := 0 and tmp2 = 1
                raw ">>[+<<+>>]<<"

                raw "[" -- if (tmp2 == 1) equivalent to if (tmp1 == 0)
                        --   then tmp1 := b' - 1
                raw "-" -- tmp2 := 0
                gotoVar quot -- quotient++
                raw "+"
                _assignAdd tmp1 b' -- tmp1 := b', assume that b != 0
                -- now at b'
                unitRight -- goto tmp1
                raw "-" -- tmp1--
                unitRight -- goto tmp2
                raw "]" -- endif at tmp2

                gotoVar a'
                raw "]" -- endwhile (a')
                -- at a'

                -- now: a' == 0
                --      b' == b
                --      tmp1 == b' or b' - remainder
                --      tmp2 == 0
                raw ">>[<<" -- if(a.temp)
                unitLeft -- goto quotient
                raw "-" -- then quotient--
                unitRight -- goto a'
                raw "]" -- a' == 0, and it was already aligned
                -- at a
                -- calculate the remainder
                unitRight -- b'
                unitRight -- tmp1
                raw "[" -- if (tmp1)
                _assignAdd a' b' -- a' := b'
                -- at b'
                unitRight -- goto tmp1
                raw "[" -- if (tmp1)
                raw "-" -- tmp1--
                unitLeft -- b'
                unitLeft -- a'
                raw "-" -- a'--
                unitRight -- b'
                unitRight -- tmp1'
                raw "]" -- endif(tmp1)
                raw "]" -- endif(tmp1)

                stackDrop 3


-- | @r := (a == b)@
doEQ :: Variable
     -- ^ @r@
     -> Variable
     -- ^ @a@
     -> Variable
     -- ^ @b@
     -> CodeGen
doEQ r a b = do
             _doEQ a b
             unsafeAssign (amendVar 1 r) (LocalVar 0)
             stackDrop 1

-- | evaluate the express @a == b@ ,
--   result will be stored on a new Unit at the stack top.
_doEQ :: Variable
      -- @a@
      -> Variable
      -- @b@
      -> CodeGen
_doEQ = __doEQ False


-- | @r := (a != b)@
doNE :: Variable
     -- ^ @r@
     -> Variable
     -- ^ @a@
     -> Variable
     -- ^ @b@
     -> CodeGen
doNE r a b = do
             _doNE a b
             unsafeAssign (amendVar 1 r) (LocalVar 0)
             stackDrop 1

-- | evaluate the express @a != b@ ,
--   result will be stored on a new Unit at the stack top.
_doNE :: Variable
      -- @a@
      -> Variable
      -- @b@
      -> CodeGen
_doNE = __doEQ True

-- low level function
__doEQ isNE a b = do
            newVar 0
            newVar 0
            let res = (LocalVar 1)
                tmp = (LocalVar 0)
                a' = amendVar 2 a
                b' = amendVar 2 b
            _assignAdd res a'
            _assignAdd tmp b'
            gotoVar res
            raw "["
            raw "-"
            unitRight -- next Unit
            raw "-"
            unitLeft -- prev Unit
            raw "]"
            when (not isNE) $ raw "+" -- res := 1
            unitRight -- next Unit
            raw "["
            unitLeft -- prev Unit
            if isNE then (raw "+") else (raw "-")
            unitRight -- next Unit
            raw "[-]]"
            stackDrop 1

-- | the low-level assign: @ a := f(b) @
--
--   finally goto the variable @b@
--
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
                raw "[" -- copy loop : while (b != 0)
                raw "->>+<<" -- copy to b's temp
                gotoVar a
                op -- modify the var a with op
                gotoVar b
                raw "]"
                gotoVar b -- to recover b's original value
                raw ">>[-<<+>>]<<"

-- | @ a += b @
--
--   finally goto the variable @b@.
--
--   /unsafe/
_assignAdd :: Variable
          -- ^ a
          -> Variable
          -- ^ b
          -> CodeGen
_assignAdd = _assign (raw "+")

-- | @ a -= b @
--
--   finally goto the variable @b@.
--
--   /unsafe/
_assignMinus :: Variable
          -- ^ a
          -> Variable
          -- ^ b
          -> CodeGen
_assignMinus = _assign (raw "-")



-- | @a := a && b@.
--
--  finally locates at @a@
--
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
