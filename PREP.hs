{- |
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
