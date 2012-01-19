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

                    #if Express
                    #ifdef M
                    #ifndef M
                    #else
                    #endif

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
               | RawString String
               -- ^ c string with \"\" and escape \\
               | RawChar String
               -- ^ c char with \'\' and escape \\
               | COMMA
               -- ^ the , char
               | OtherToken String
               -- ^ seperate by spaces
               | EOL
               -- ^ regex: [\\r\\n]* will reset the flag for #commands
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
standardEOL = "\n"

token :: PrepParser PrepToken
token = getCode >>= \code -> invokeToken code
        where
          invokeToken [] = return EOF
          invokeToken (x:xs)
                  | x == '#'
                      = get >>= return . isLineBegin >>=
                      \lb -> if lb
                               then clearLineBegin >> tokenMacro
                               else do
                                    l <- getLineNumber
                                    throwError ("illegal # at line " ++ show l)
                  | x `elem` _azAZ
                          = clearLineBegin >>tokenIdentifier
                  -- process line join directly, transparent to prep. parser
                  | x == '\\' && (not $ null xs) && (head xs) `elem` "\r\n"
                          = skipCode 1 >> tokenEOL >> token
                  -- process ##, transparent to prep. parser
                  | x == '#' && (not $ null xs) && (head xs) == '#'
                          = skipCode 2 >> token
                  | x == '"' = clearLineBegin >> tokenRawString
                  | x == '\'' = clearLineBegin >> tokenRawChar
                  | x `elem` "\r\n" = setLineBegin >> tokenEOL
                  -- skip whitspaces, transparent to prep. parser
                  | x `elem` " \t" = tell [x] >> skipCode 1 >> token
                  | x == ',' = clearLineBegin >> skipCode 1 >> return COMMA
                  | otherwise = clearLineBegin >> tokenOther


getLineNumber :: PrepParser Int
getLineNumber = get >>= return . lineNumber

skipCode :: Int -> PrepParser ()
skipCode offset = modify $ \s -> s {restCode = drop offset (restCode s)}

getCode :: PrepParser String
getCode = get >>= return . restCode

setLineBegin :: PrepParser ()
setLineBegin = modify $ \s -> s{ isLineBegin = True }

clearLineBegin :: PrepParser ()
clearLineBegin = modify $ \s -> s{ isLineBegin = False }

tokenEOL = getCode >>= f
        where
          f [] = return EOF
          f (x:[]) | x `elem` "\r\n" = skipCode 1
                                    >> setLineBegin
                                    >> return EOL
                   | otherwise = throwError "not a newline"
          f ('\r':'\n':_) = skipCode 2 >> return EOL
          -- delete this "\n\r" case?
          f ('\n':'\r':_) = skipCode 2 >> return EOL
          f ('\n':_) = skipCode 1 >> return EOL
          f ('\r':_) = skipCode 1 >> return EOL
          f _ = throwError "not a newline"

tokenIdentifier = getCode >>= f
        where
          f [] = return EOF
          f (x:xs) | x `elem` _azAZ = g [x] xs
                   | otherwise = throwError "Not a Identifier"
          g s [] = skipCode (length s) >> return (Identifier s)
          g s (x:xs) | x `elem` _azAZ09 = g (s ++ [x]) xs
                     | otherwise = skipCode (length s) >> return (Identifier s)

tokenLineJoin = undefined -- TODO:
tokenRawChar = undefined
tokenRawString = undefined
tokenMacro = undefined
tokenOther = undefined
