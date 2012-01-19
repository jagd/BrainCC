{- |
        It is saddly, that Parsec version 2 do not has Transformer !
        Compiling Parsec 3 was not possible.
        So i would write my own tokenizer to avoid unsafePerformIO.

        C Preprocessor
                supported patterns:
                    #include

                    #define M X
                    #define M(ARGS) ARGS
                    #undef M

                    #ifdef M
                    #ifndef M
                    #else
                    #endif

                    THIS VERSION OF PREPROCESSOR DO NOT SUPPORT #if

                supported comment style:
                    \/**\/ and \/\/

        Line number and file name will written
        in a individual line like:
                # LINENUMBER "FILENAME"

        The Arguments of the Macro Pattern
                    #define M(ARGS) ARGS
            will be seperated by Commas,
                except those in Quotes \'\' \"\"
                or those in parenthesis \(\)

        The steps:
            * replace \/* ..  *\/ with white spaces
            * replace // with white spaces
            * process #define #ifdef #ifndef #else #endif and #include
            * comments in the included file will be replaced at first
-}

module PREP where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import qualified Data.Map as M


-- | remove \/* .. *\/ comments
removeCommentBlock :: String -> String
removeCommentBlock [] = []
removeCommentBlock (x:[]) = [x]
removeCommentBlock ('/':'*':xs) = ' ' : ' ' : f xs
        where f [] = error "unterminated comment"
              f (x:[]) = error "unterminated comment"
              f ('*':'/':xs) = ' ' : ' ' : removeCommentBlock xs
              f (x:xs) | isSpace' x = x : f xs
                       | otherwise = ' ' : f xs
removeCommentBlock (x:xs) = x : removeCommentBlock xs


-- | remove \/\/ comments
removeCommentLine :: String -> String
removeCommentLine [] = []
removeCommentLine (x:[]) = [x]
removeCommentLine ('/':'/':xs) = ' ' : ' ' : f xs
        where f (x:xs) | x == '\n' || x == '\r' = x : removeCommentLine xs
                       | otherwise = ' ' : f xs
removeCommentLine (x:xs) = x : removeCommentLine xs

-- | remove C89 \/* .. *\/ and C99 \/\/ Comments
removeComments :: String -> String
removeComments = removeCommentLine . removeCommentBlock

-- | the isSpace function in "Prelude" has some problem with unicode
isSpace' :: Char -> Bool
isSpace' '\n' = True
isSpace' '\r' = True
isSpace' '\t' = True
isSpace' ' ' = True
isSpace' _ = False


data PrepToken = Identifier String
               -- ^ inclusive ID and keyword
               | INCLUDE
               -- ^ #include
               | DEFINE
               -- ^ #define
               | IF
               -- ^ #if
               | IFDEF
               -- ^ #ifdef
               | IFNDEF
               -- ^ #ifndef
               | ELSE
               -- ^ #else
               | ENDIF
               -- ^ #endif
               | LineContinue
               -- ^ \\'NewLine' at the end of a line , only in #define
               | NewLine
               -- ^ regex: [\\r\\n]* will reset the flag for #commands
               | RawString String
               -- ^ c string with \"\" and escape \\
               | RawChar String
               -- ^ c char with \'\' and escape \\
               | COMMA
               -- ^ the , char
               | OtherToken String
               -- ^ seperate by spaces or nonspaces
               | EOF

data PrepState = PrepState {
                         restCode    :: String,
                         lineNumber  :: Int,
                         isLineBegin :: Bool,
                         defineTree  :: M.Map String String
                 }

type PrepParser a = ErrorT String (StateT PrepState (WriterT String IO)) a

_azAZ = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
_azAZ09 = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']

token :: PrepParser PrepToken
token = get >>= return . restCode
            >>= \code -> invokeToken code
        where
          invokeToken [] = return EOF
          invokeToken (x:xs)
                  | x == '#'
                      = get >>= return . isLineBegin >>=
                      \lb -> if lb
                               then tokenMacro
                               else do
                                    l <- getLineNumber
                                    throwError ("illegal # at line " ++ show l)
                  | x `elem` _azAZ
                          = tokenIdentifier
                  | x == '\\' && (not $ null xs) && (head xs) `elem` "\r\n"
                          = tokenLineContinue
                  | x == '"' = tokenRawString
                  | x == '\'' = tokenRawChar
                  | x `elem` "\r\n" = tokenNewLine
                  | x == ',' = do
                               skipCode 1
                               tell " "
                               return COMMA
                  | otherwise = tokenOther


getLineNumber :: PrepParser Int
getLineNumber = get >>= return . lineNumber

skipCode :: Int -> PrepParser ()
skipCode offset = undefined

tokenNewLine = undefined
tokenIdentifier = undefined
tokenLineContinue = undefined
tokenRawChar = undefined
tokenRawString = undefined
tokenMacro = undefined
tokenOther = undefined
