import Data.Char


-- Definition : A space character means a ' ', or a '\t', or a '\n', etc..

-- Definition : A special character means a character which is not
--              a space character, and is neither a letter nor a digit.


-- truncate space characters at the beginning and the end of the passage
trunc :: String -> String
trunc = reverse.dropWhile isSpace.reverse.dropWhile isSpace


-- reduce each continuous space character series to one character
-- if there is a '\n' in the series, the series is reduced to '\n'
-- otherwise, the series is reduced to ' '
reduce :: String -> String
reduce "" = ""
reduce tot @ (c : cs)
    | isSpace c = ((:).digest.takeWhile isSpace) <*> (reduce.dropWhile isSpace) $ tot
    | otherwise = c : reduce cs
    where digest ss = case any (== '\n') ss of True -> '\n'
                                               False -> ' '
-- line 21 explanation
-- functions, as applicative functors, have this definition :
-- f <*> g = \x -> f x (g x)
-- thus, we have :
--   ((:).digest.takeWhile isSpace) <*> (reduce.dropWhile isSpace) $ tot
-- = ((:).digest.takeWhile isSpace) tot ((reduce.dropWhile isSpace) tot)
-- = (digest.takeWhile isSpace $ tot) : (reduce.dropWhile isSpace $ tot)


-- if one character is a space character but not a '\n'
-- and the next character is neither a letter nor a digit
-- then the current space character is deleted
push :: String -> String
push "" = ""
push (c : cs)
    | length cs == 0                                         = c : push cs
    | isSpace c && (c /= '\n') && (not.isAlphaNum.head $ cs) = push cs
    | otherwise                                              = c : push cs


-- every space character adjacent to a special character will be deleted,
-- unless it is a '\n'
delete :: String -> String
delete = reverse.push.reverse.push


-- cmprs (compress) : just composition of some above functions
cmprs :: String -> String
cmprs = delete.reduce.trunc


-- after compression, the passage can be expanded to normal format :
-- 1. leave a blank line between paragraphs
-- 2. leave a ' ' after each ',' and '.',
--    unless it is at the end of the paragraph or even the end of the passage
expand :: String -> String
expand "" = ""
expand (c : cs)
    | c == '\n'                  = '\n' : '\n' : expand cs
    | isDot c && length cs == 0  = c : expand cs
    | isDot c && head cs == '\n' = c : expand cs
    | isDot c                    = c : ' ' : expand cs
    | otherwise                  = c : expand cs
    where isDot c = (c == ',') || (c == '.')


main :: IO ()
main = getContents >>= putStr.expand.cmprs
