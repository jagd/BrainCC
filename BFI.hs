{- |

A plain and dirty Brainf**k interpreter, which was written many years ago.
For QuickCheck and Debug.

-}

module BFI (runBF) where

import Data.Word
import Data.Char (chr, ord)

data BFState = BFState {
     -- the Code State
     opCurr   :: Maybe Char,
     opRight  :: [Char],
     opLeft   :: [Char],
     -- the memory state :
     memCurr  :: Word8,
     memRight :: [Word8],
     memLeft  :: [Word8]
} deriving (Show)

type Brainfuck = (String, BFState)

runBF :: String
      -- ^ The Brainf**k code
      -> String
      -- ^ input
      -> String
      -- ^ output
runBF bfCode input = fst $ brainfuck initState input
         where initState = BFState {
                             opCurr = Nothing,
                             opRight = bfCode,
                             opLeft = "",
                             memCurr = 0,
                             memRight = [],
                             memLeft = []
                            }



brainfuck :: BFState -> String -> Brainfuck
brainfuck state input
          | op == Just '+' =
             let newState  = state { memCurr = memCurr state + 1}
                 nextState = nextOp newState
             in
                 brainfuck nextState input

          | op == Just '-' =
             let newState  = state { memCurr = memCurr state - 1}
                 nextState = nextOp newState
             in
                 brainfuck nextState input

          | op == Just '>' =
                         let newState = nextCell $ nextOp state
                         in brainfuck newState input

          | op == Just '<' =
                         let newState = prevCell $ nextOp state
                         in brainfuck newState input

          | op == Just '[' =
               let nextState = if memCurr state /= 0
                               then nextOp state
                               else skipLoop state
               in brainfuck nextState input

          | op == Just ']' =
               -- = brainfuck (popState state) input
               let nextState = returnLoop state
               in brainfuck nextState input

          | op == Just ',' = case input of
               -- [] -> brainfuck (jumpToEnd state) input
               [] -> let newState = state {memCurr =  -1}
                     in brainfuck (nextOp newState) []
               _ ->
                   let newState = state {memCurr =  (fromIntegral . ord . head) input}
                   in brainfuck (nextOp newState) (tail input)

          | op == Just '.' =
               let (output, newState) = brainfuck (nextOp state) input
               in (( (chr. fromIntegral . memCurr) state) : output , newState)

          | op == Nothing = if opRight state == []
                            then ("", state)
                            else brainfuck (nextOp state) input

          | otherwise -- if it is comment
               = brainfuck (nextOp state) input

          where op = opCurr state

-- -- von ] zurück zu der passenden [
returnLoop :: BFState -> BFState
returnLoop state = returnLoop' (prevOp state) 1

returnLoop' :: BFState -> Int -> BFState
returnLoop' state level
          | level == 0 = error "error matching ["
          | level == 1 && op == Just '[' = state
          | op == Just ']' = returnLoop' (prevOp state) (level + 1)
          | op == Just '[' = returnLoop' (prevOp state) (level - 1)
          | op == Nothing
               = error $ "no matched [ at Byte "
                        ++ (show $ length $ opLeft state)
          | otherwise = returnLoop' (prevOp state) level
          where op = opCurr state

jumpToEnd :: BFState -> BFState
jumpToEnd state = if opRight state /= []
  then nextOp state
  else state

skipLoop :: BFState -> BFState
skipLoop state = skipLoop' (nextOp state) 1

skipLoop' :: BFState -> Int -> BFState
skipLoop' state level
          | level == 0 = state
          | op == Just '[' = skipLoop' (nextOp state) (level + 1)
          | op == Just ']' = skipLoop' (nextOp state) (level - 1)
          | op == Nothing
               = error $ "no matched ] at Byte "
                        ++ (show $ length $ opLeft state)
          | otherwise = skipLoop' (nextOp state) level
          where op = opCurr state

prevOp :: BFState -> BFState
prevOp state | curr == Nothing
                    = state {
                           opCurr = Just newCurr,
                           opLeft = left'
                           }
             | opLeft state == []
                    = state {
                          opRight = right' ,
                          opCurr = Nothing
                          }
             | otherwise =
                     state {
                           opCurr = Just newCurr,
                           opRight = right',
                           opLeft = left'
                           }
             where
                   curr  = opCurr state
                   right  = opRight state
                   left = opLeft state
                   --
                   Just curr' = curr
                   right' = curr' : right
                   newCurr = head left
                   left' = tail left


nextOp :: BFState -> BFState
nextOp state | curr == Nothing
                    = state {
                           opCurr = Just newCurr,
                           opRight = right'
                           }
             | opRight state == []
                    = state {
                          opLeft = left' ,
                          opCurr = Nothing
                          }
             | otherwise =
                     state {
                           opCurr = Just newCurr,
                           opLeft = left',
                           opRight = right'
                           }
             where
                   curr  = opCurr state
                   left  = opLeft state
                   right = opRight state
                   --
                   Just curr' = curr
                   left' = curr' : left
                   newCurr = head right
                   right' = tail right


nextCell :: BFState -> BFState
nextCell state
         | memRight state == []
                     = state {memLeft = newLeft, memCurr = 0}
         | otherwise = state {
                         memLeft  = newLeft,
                         memCurr  = newCurr,
                         memRight = newRight
                         }
         where
                oldCurr  = memCurr state
                newCurr  = head $ memRight state
                newRight = tail $ memRight state
                newLeft  = oldCurr : (memLeft state)

prevCell :: BFState -> BFState
prevCell state
         | memLeft state == []
                     = state {memRight = newRight, memCurr = 0}
         | otherwise = state {
                         memLeft  = newLeft,
                         memCurr  = newCurr,
                         memRight = newRight
                         }
         where
                oldCurr  = memCurr state
                newCurr  = head $ memLeft state
                newLeft  = tail $ memLeft state
                newRight  = oldCurr : (memRight state)
