-- module TestBSM where
module Main where

import Test.QuickCheck

import BSM
import BFI (runBF)

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
          inc

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
test8 a = genCode $
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

-- test (doLogNOT a)
test9 a = genCode $
        do
          begin ( return () )
          clearJump
          newVar a
          _doLogNOT (LocalVar 0)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

-- test (assignLogAND a b)
test10 a b = genCode $
        do
          begin ( return () )
          clearJump
          newVar a
          newVar b
          _assignLogAND (LocalVar 0) (LocalVar 1)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

-- test (assignLogAND a a)
test11 a = genCode $
        do
          begin ( return () )
          clearJump
          newVar a
          _assignLogAND (LocalVar 0) (LocalVar 0)
          raw "["
          pushChar 'Y'
          raw "."
          setCurVar $ fromEnum '\n'
          raw "."
          raw "[-]]"
          end

test12 = genCode $
        do
          begin ( pushChar '\n' >> pushChar 's' >> pushChar 'V' )
          newVar 1
          setJump 0

          _assignAdd (LocalVar 1) (LocalVar 0)
          gotoVar $ LocalVar 1
          raw "."

          gotoVar $ LocalVar 0
          _assignAdd (LocalVar 0) (LocalVar 0)

          _assignAdd (LocalVar 2) (LocalVar 0)
          gotoVar $ LocalVar 2
          raw "."

          stackDrop 3
          raw "."
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
          output -- X
          inc
          gotoVar (ArrayVar (ArrayVar base elem2) elem1)
          output -- also Y
          inc
          gotoVar (ArrayVar base (ArrayVar base elem2))
          output -- also Z
          setCurVar $ fromEnum '\n'
          output -- newline
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

runTest = do
          quickCheck $ "" == runBF testFrame1 ""
          quickCheck $ "" == runBF testFrame2 ""
          quickCheck $ "wu\n" == runBF testLocalVar ""
          quickCheck $ "Wu\n" == runBF test_assignAdd ""
          quickCheck $ "Wu\n" == runBF testStackDrop ""
          quickCheck $ "Wu\n" == runBF testAmendVar ""
          quickCheck $ "XYZ\n" == runBF testArray ""

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

          quickCheck $ \(i, j) ->
                       let x = i `rem` 128
                           y = j `rem` 128
                       in  runBF (testEQ x y) "" ==
                           if x == y
                             then "Y\n"
                             else ""

          quickCheck $ \(i, j) ->
                       let x = i `rem` 128
                           y = j `rem` 128
                       in  runBF (testNE x y) "" ==
                           if x /= y
                             then "Y\n"
                             else ""

main = runTest
