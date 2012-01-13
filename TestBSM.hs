-- module TestBSM where
module Main where

import Test.QuickCheck

import BSM
import BFI (runBF)

setCurVar n = raw "[-]" >> incConstant n

testFrame1 = genCode $
        do
          begin ( return () )
          setJump 0
          end

testFrame2 = genCode $
        do
          begin ( newVar 0 )
          setJump 0
          end

testNewVar n = genCode $
        do
          begin ( return () )
          newVar n
          raw "."
          setJump 0
          end

testSafeAssign = genCode $
        do
          begin ( pushChar 'Y' )
          stackEnlarge 1
          safeAssign (LocalVar 0) (GlobalVar 1)
          gotoVar (LocalVar 0)
          raw "."
          setJump 0
          end

testLocalVar = genCode $
        do
          begin ( pushChar '\n' >> pushChar 'u' >> pushChar 'w' )
          setJump 0
          gotoVar $ LocalVar 0
          raw "."
          gotoVar $ LocalVar 1
          raw "."
          stackDrop 2
          raw "."
          end

test_assignAdd = genCode $
        do
          begin ( pushChar '\n' >> pushChar 's' >> pushChar 'V' )
          newVar 1
          setJump 0

          _assignAdd (LocalVar 1) (LocalVar 0)
          gotoVar $ LocalVar 1
          raw "."

          gotoVar $ LocalVar 0
          raw "+"

          _assignAdd (LocalVar 2) (LocalVar 0)
          gotoVar $ LocalVar 2
          raw "."

          stackDrop 3
          raw "."
          end

testStackDrop = genCode $
        do
          begin $ do
                    stackEnlarge 10
                    pushChar '\n'
                    pushChar 's'
                    pushChar 'X'

          newVar 1
          setJump 0

          _assignMinus (LocalVar 1) (LocalVar 0)
          gotoVar $ LocalVar 1
          raw "."

          _assignAdd (LocalVar 0) (LocalVar 0)

          _assignAdd (LocalVar 2) (LocalVar 0)
          gotoVar $ LocalVar 2
          raw "."

          stackDrop 3 -- dropped  'V' & 's'
          raw "."
          stackDrop (10 + 1) -- 10 reserved + '\n'
          end

testAmendVar = genCode $
        do
          begin $ do
                    pushChar '\n'
                    pushChar 's'
                    pushChar 'X'
          newVar 1
          setJump 0

          let n = LocalVar 0
              x = LocalVar 1
              s = LocalVar 2
              e = GlobalVar 1

          stackEnlarge 10
          let [n',x',s',e'] = map (amendVar 10) [n,x,s,e]

          _assignMinus x' n'
          gotoVar x'
          raw "."

          _assignAdd n' n'

          _assignAdd s' n'
          gotoVar s'
          raw "."

          gotoVar e'
          raw "."

          end

testAmendVar2 s = genCode $
        do
          begin ( return () )
          clearJump
          newVar 1 -- 3
          newVar 2 -- 2
          newVar 3 -- 1
          pushChar 'X' -- 0
          let base = LocalVar 3
              elem1 = LocalVar 3
              elem2 = LocalVar 2
              elem3 = LocalVar 1
              elemY = LocalVar 0
          stackEnlarge s
          gotoVar $ amendVar s (ArrayVar base elem3)
          raw "." -- X
          raw "+"
          gotoVar $ amendVar s (ArrayVar (ArrayVar base elem2) elem1)
          raw "." -- also Y
          raw "+"
          gotoVar $ amendVar s (ArrayVar base (ArrayVar base elem2))
          raw "." -- also Z
          setCurVar $ fromEnum '\n'
          raw "." -- newline
          end

-- test a && b
test_doLogAND a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar a
          newVar b
          _doLogAND (LocalVar 0) (LocalVar 1)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

test_doLogOR a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar a
          newVar b
          raw "\nOR start\n"
          _doLogOR (LocalVar 0) (LocalVar 1)
          raw "\nOR end\n"
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

-- test (curLogNOT a)
testCurLogNOT a = genCode $
        do
          begin ( return () )
          clearJump
          newVar a
          curLogNOT
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

-- test doLogAND
testDoLogAND a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 10
          newVar a
          newVar b
          doLogAND (LocalVar 2) (LocalVar 0) (LocalVar 1)
          gotoVar (LocalVar 2)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

-- test doLogOR
testDoLogOR a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 10
          newVar a
          newVar b
          doLogOR (LocalVar 2) (LocalVar 0) (LocalVar 1)
          gotoVar (LocalVar 2)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

testArray = genCode $
        do
          begin ( return () )
          clearJump
          newVar 1 -- 3
          newVar 2 -- 2
          newVar 3 -- 1
          pushChar 'X' -- 0
          let base = LocalVar 3
              elem1 = LocalVar 3
              elem2 = LocalVar 2
              elem3 = LocalVar 1
              elemY = LocalVar 0
          gotoVar (ArrayVar base elem3)
          raw "." -- X
          raw "+"
          gotoVar (ArrayVar (ArrayVar base elem2) elem1)
          raw "." -- also Y
          raw "+"
          gotoVar (ArrayVar base (ArrayVar base elem2))
          raw "." -- also Z
          setCurVar $ fromEnum '\n'
          raw "." -- newline
          end

-- test doEQ
testEQ a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 10
          newVar a
          newVar b
          doEQ (LocalVar 2) (LocalVar 0) (LocalVar 1)
          gotoVar (LocalVar 2)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

-- test doNE
testNE a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 10
          newVar a
          newVar b
          doNE (LocalVar 2) (LocalVar 0) (LocalVar 1)
          gotoVar (LocalVar 2)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

testGT a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 0
          newVar a -- LocalVar 1
          newVar b -- LocalVar 0
          doGT (LocalVar 2) (LocalVar 1) (LocalVar 0)
          gotoVar (LocalVar 2)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

testGE a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 0
          newVar a -- LocalVar 1
          newVar b -- LocalVar 0
          doGE (LocalVar 2) (LocalVar 1) (LocalVar 0)
          gotoVar (LocalVar 2)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

testLT a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 0
          newVar a -- LocalVar 1
          newVar b -- LocalVar 0
          doLT (LocalVar 2) (LocalVar 1) (LocalVar 0)
          gotoVar (LocalVar 2)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

testLE a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 0
          newVar a -- LocalVar 1
          newVar b -- LocalVar 0
          doLE (LocalVar 2) (LocalVar 1) (LocalVar 0)
          gotoVar (LocalVar 2)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

testDoLogNOT x = genCode $
        do
          begin ( return () )
          clearJump
          newVar 0
          newVar x
          doLogNOT (LocalVar 1) (LocalVar 0)
          gotoVar (LocalVar 1)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

testDoPlus a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 0
          newVar a
          newVar b
          doPlus (LocalVar 2) (LocalVar 0) (LocalVar 1)
          gotoVar (LocalVar 2)
          raw "."
          end

testDoMinus a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 0
          newVar a
          newVar b
          doMinus (LocalVar 2) (LocalVar 1) (LocalVar 0)
          gotoVar (LocalVar 2)
          raw "."
          end

testDoMul a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar 0
          newVar a
          newVar b
          doMul (LocalVar 2) (LocalVar 1) (LocalVar 0)
          gotoVar (LocalVar 2)
          raw "."
          end


runTest = do
          putStrLn "Test Frame1:"
          quickCheck $ "" == runBF testFrame1 ""

          putStrLn "Test Frame2:"
          quickCheck $ "" == runBF testFrame2 ""

          putStrLn "LocalVar:"
          quickCheck $ "wu\n" == runBF testLocalVar ""

          putStrLn "newVar:"
          quickCheck $ \i ->
                       let x = (abs i) `rem` 256
                       in  runBF (testNewVar x) "" == [(toEnum x) :: Char]

          putStrLn "safeAssign:"
          quickCheck $ "Y" == runBF testSafeAssign ""

          putStrLn "_assignAdd:"
          quickCheck $ "Wu\n" == runBF test_assignAdd ""

          putStrLn "stackDrop:"
          quickCheck $ "Wu\n" == runBF testStackDrop ""

          putStrLn "amendVar:"
          quickCheck $ "Wu\n" == runBF testAmendVar ""

          putStrLn "amendVar on Array:"
          quickCheck $ \i -> let x = (abs i) `rem` 50
                             in runBF (testAmendVar2 x) "" == "XYZ\n"

          putStrLn "Test Array:"
          quickCheck $ "XYZ\n" == runBF testArray ""

          putStrLn "doLogNOT:"
          quickCheck $ \i ->
                       let x = (i `rem` 128) `div` 10
                       in  runBF (testDoLogNOT x) "" ==
                           if x == 0 then "Y\n" else ""

          putStrLn "curLogNOT:"
          quickCheck $ \i ->
                       let x = (i `rem` 128) `div` 10
                       in  runBF (testCurLogNOT x) "" ==
                           if x == 0 then "Y\n" else ""

          putStrLn "_doLogAND && doLogAND:"
          quickCheck $ \(i, j) ->
                       let x = i `rem` 128
                           y = j `rem` 128
                           ra = runBF (test_doLogAND x y) ""
                           rb = runBF (testDoLogAND x y) ""
                       in  ra == rb
                           &&
                           ra == if (x /= 0) && (y /= 0)
                                   then "Y\n"
                                   else ""

          putStrLn "_doLogOR  && doLogOR:"
          quickCheck $ \(i, j) ->
                       let x = i `rem` 128
                           y = j `rem` 128
                           ra = runBF (test_doLogOR x y) ""
                           rb = runBF (testDoLogOR x y) ""
                       in  ra == rb
                           &&
                           ra == if (x /= 0) || (y /= 0)
                                   then "Y\n"
                                   else ""

          putStrLn "doEQ:"
          quickCheck $ \(i, j) ->
                       let x = i `rem` 128
                           y = j `rem` 128
                       in  runBF (testEQ x y) "" ==
                           if x == y
                             then "Y\n"
                             else ""

          putStrLn "doNE:"
          quickCheck $ \(i, j) ->
                       let x = i `rem` 128
                           y = j `rem` 128
                       in  runBF (testNE x y) "" ==
                           if x /= y
                             then "Y\n"
                             else ""

          putStrLn "doGT:"
          quickCheck $ \(i, j) ->
                       let x = (abs i) `rem` 256
                           y = (abs j) `rem` 256
                       in  runBF (testGT x y) "" ==
                           if x > y
                             then "Y\n"
                             else ""

          putStrLn "doLT:"
          quickCheck $ \(i, j) ->
                       let x = (abs i) `rem` 256
                           y = (abs j) `rem` 256
                       in  runBF (testLT x y) "" ==
                           if x < y
                             then "Y\n"
                             else ""

          putStrLn "doGE:"
          quickCheck $ \(i, j) ->
                       let x = (abs i) `rem` 256
                           y = (abs j) `rem` 256
                       in  runBF (testGE x y) "" ==
                           if x >= y
                             then "Y\n"
                             else ""

          putStrLn "doLE:"
          quickCheck $ \(i, j) ->
                       let x = (abs i) `rem` 256
                           y = (abs j) `rem` 256
                       in  runBF (testLE x y) "" ==
                           if x <= y
                             then "Y\n"
                             else ""

          putStrLn "doPlus:"
          quickCheck $ \(i, j) ->
                       let x = i `rem` 127
                           y = j `rem` 127
                       in  runBF (testDoPlus x y) ""
                             == [(toEnum ((x + y + 256) `rem` 256) :: Char)]

          putStrLn "doMinus:"
          quickCheck $ \(i, j) ->
                       let x = i `rem` 127
                           y = j `rem` 127
                       in  runBF (testDoMinus x y) ""
                             == [(toEnum ((x - y + 256) `rem` 256) :: Char)]

          putStrLn "doMul:"
          quickCheck $ \(i, j) ->
                       let x = (abs i) `rem` 127
                           y = (abs j) `rem` 127
                       in  runBF (testDoMul x y) ""
                             == [(toEnum ((x * y) `rem` 256) :: Char)]

main = runTest
