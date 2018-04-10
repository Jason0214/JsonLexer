import System.Environment
import System.IO
import System.Exit
import qualified Data.Set as Set

main = do
    args <- getArgs
    if length args <= 0 || length args > 1 
        then do 
            putStrLn "incorrect argument number, exit"
            exitFailure
    else do
        let htmlHeader = "<head><meta charset='utf-8' http-equiv='content-type' content='text'> <style>body {background: #1e1e1eff;} .code {font-family: Menlo, Monaco, 'Courier New', monospace; font-size: 14px; margin-left: 5%; margin-right: 5%; margin-top: 2%; } .colon{color: #d4d4d4; } .comma {color: #d4d4d4; } .brace{color: #d16969; } .bracket {color: #f44747; } .key_string{color: #9cdcfe; } .value_string {color: #ce9178; } .value_number {color: #b5cea8; } .value_keyword {color: #569cd6; } .preprocess_string{color: #dcdcaa; } ._tab {margin-left: 2em; } ._space {margin-left: 0.25em; }</style></head> <body><div class='code'>"
        let htmlTail = "</div></body></html>"
        contents <- readFile $ args!!0
        writeFile ( genOutputFileName $ args!!0 ) $ htmlHeader ++ (colorize 0 $ tokenize contents) ++ htmlTail

data Token = OpenBracket | CloseBracket | OpenBrace | CloseBrace | Colon | Comma | StringToken String | StringEsacpedPart String | Keyword String | NumberToken String | Error String 
    deriving(Show)

genOutputFileName :: String -> String
genOutputFileName "" = ".html"
genOutputFileName (c:cs) = if c == '.' then ".html"
                           else [c] ++ genOutputFileName cs

isLetter :: Char -> Bool
isLetter c = if c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' then True
             else False

isDigit :: Char -> Bool
isDigit c = if c >= '0' && c <= '9' then True
            else False

isSpace :: Char -> Bool
isSpace c
    | c == ' ' = True
    | c == '\n' = True
    | c == '\t' = True
    | otherwise = False

isHex :: Char -> Bool
isHex c = if c >= 'a' && c <= 'f' || c >= '0' && c <= '9' then True
          else False

tokenize :: String -> [Token] 
tokenize [] = []
tokenize (c:cs)
    | c == '[' = OpenBracket : tokenize cs
    | c == ']' = CloseBracket : tokenize cs
    | c == '{' = OpenBrace : tokenize cs
    | c == '}' = CloseBrace : tokenize cs
    | c == ':' = Colon : tokenize cs
    | c == ',' = Comma : tokenize cs
    | c == '"' = readStringToken [c] cs
    | c == '-' = readNegNumberToken [c] cs
    | isSpace c = tokenize cs
    | isDigit c = readNumberToken [c] cs
    | isLetter c = readKeyword [c] cs
    | otherwise = [Error [c]] ++ tokenize cs

-- lexing number 
readNumberToken :: String -> String -> [Token]
readNumberToken number [] = [NumberToken number]
readNumberToken number (c:cs)
    | isDigit c = readNumberToken (number ++ [c]) cs
    | c == '.' = readDigitAfterDot  (number ++ [c]) cs
    | c == 'e' = readSignedExponent (number ++ [c]) cs
    | c == 'E' = readSignedExponent (number ++ [c]) cs
    | otherwise = [NumberToken number] ++ tokenize (c:cs)

readNegNumberToken :: String -> String -> [Token]
readNegNumberToken number [] = [Error number]
readNegNumberToken number (c:cs) 
    | isDigit c = readNumberToken (number ++ [c]) cs
    | otherwise = [Error (number ++ [c])] ++ tokenize (c:cs)

readDigitAfterDot :: String -> String -> [Token]
readDigitAfterDot number [] = [Error number]
readDigitAfterDot number (c:cs)
    | isDigit c = readFloatNumberToken (number ++ [c]) cs
    | otherwise = [Error (number ++ [c])] ++ tokenize cs

readFloatNumberToken :: String -> String -> [Token]
readFloatNumberToken number [] = [NumberToken number]
readFloatNumberToken number (c:cs)
    | isDigit c = readFloatNumberToken (number ++ [c]) cs
    | c == 'e' = readSignedExponent (number ++ [c]) cs
    | c == 'E' = readSignedExponent (number ++ [c]) cs
    | otherwise =  [NumberToken number] ++ tokenize (c:cs)

readSignedExponent :: String -> String -> [Token]
readSignedExponent number [] = [NumberToken number]
readSignedExponent number (c:cs)
    | isDigit c = readExponent (number ++ [c]) cs
    | c == '-' = readDigitAfterExpSign (number ++ [c]) cs
    | otherwise = [Error (number ++ [c])] ++ tokenize cs 

readDigitAfterExpSign :: String -> String -> [Token]
readDigitAfterExpSign number [] = [Error number]
readDigitAfterExpSign number (c:cs)
    | isDigit c = readExponent (number ++ [c]) cs
    | otherwise = [Error (number ++ [c])] ++ tokenize cs

readExponent :: String -> String -> [Token]
readExponent number [] = [NumberToken number]
readExponent number (c:cs)
    | isDigit c = readExponent (number ++ [c]) cs
    | otherwise = [NumberToken number] ++ tokenize (c:cs)

-- lexing string
readStringToken :: String -> String -> [Token]
readStringToken str [] = [Error str]
readStringToken str (c:cs)
    | c == '"' = [StringToken (str ++ [c])] ++ tokenize cs
    | c == '\\' =  [StringToken str] ++ readEscapedChar [c] cs
    | otherwise = readStringToken (str ++ [c]) cs

readEscapedChar :: String -> String -> [Token]
readEscapedChar escapedSign [] = [Error escapedSign]
readEscapedChar escapedSign (c:cs)
    | c == '"' = [StringEsacpedPart (escapedSign ++ [c])] ++ readStringToken "" cs
    | c == '\\' = [StringEsacpedPart (escapedSign ++ [c])] ++ readStringToken "" cs
    | c == '/' = [StringEsacpedPart (escapedSign ++ [c])] ++ readStringToken "" cs
    | c == 'b' = [StringEsacpedPart (escapedSign ++ [c])] ++ readStringToken "" cs
    | c == 'f' = [StringEsacpedPart (escapedSign ++ [c])] ++ readStringToken "" cs
    | c == 'n' = [StringEsacpedPart (escapedSign ++ [c])] ++ readStringToken "" cs
    | c == 'r' = [StringEsacpedPart (escapedSign ++ [c])] ++ readStringToken "" cs
    | c == 't' = [StringEsacpedPart (escapedSign ++ [c])] ++ readStringToken "" cs
    | c == 'u' = readUnicode (escapedSign ++ [c]) cs
    | otherwise = [Error (escapedSign ++ [c])] ++ readStringToken "" cs

readUnicode :: String -> String -> [Token]
readUnicode unicode [] = [Error unicode]
readUnicode unicode (c:cs)
    | isHex c = if length unicode == 5 then [StringEsacpedPart (unicode ++ [c])] ++ readStringToken "" cs
                else readUnicode (unicode ++ [c]) cs
    | otherwise = [Error (unicode ++ [c])] ++ readStringToken "" cs

-- lexing keyword
readKeyword :: String -> String -> [Token]
readKeyword str [] = if str == "true" || str == "false" || str == "null" then [Keyword str]
                     else [Error str]
readKeyword str (c:cs)
    | isLetter c = readKeyword (str ++ [c]) cs
    | otherwise = if str == "true" || str == "false" || str == "null" then [Keyword str] ++ tokenize (c:cs)
                  else [Error str] ++ tokenize (c:cs)

-- tokens to html
duplicate :: Int -> String -> String
duplicate n str = [1..n] >>= const str

indents :: Int -> String
indents n = duplicate n "<span class='_tab'></span>"

replaceHtmlSpecialChar :: String -> String
replaceHtmlSpecialChar [] = []
replaceHtmlSpecialChar (c:cs)
    | c == '<' = "&lt" ++ replaceHtmlSpecialChar cs
    | c == '>' = "&gt" ++ replaceHtmlSpecialChar cs
    | c == '&' = "&amp" ++ replaceHtmlSpecialChar cs
    | c == '"' = "&quot" ++ replaceHtmlSpecialChar cs
    | c == '\'' = "&apos" ++ replaceHtmlSpecialChar cs
    | otherwise = c:replaceHtmlSpecialChar cs

colorize :: Int -> [Token] -> String
colorize indentSize [] = []
colorize indentSize ((Error str):ts) = indents indentSize ++ "<span>error:" ++ replaceHtmlSpecialChar str ++ "</span><br/>" ++ colorize indentSize ts 
colorize indentSize ((OpenBrace):ts) = "<br/>" ++ indents indentSize ++ "<span class='brace'>{</span><br/>" ++ indents (indentSize + 1) ++ colorize (indentSize + 1) ts
colorize indentSize ((CloseBrace):ts) = "<br/>" ++ indents  (indentSize - 1) ++ "<span class='brace'>}</span>" ++ colorize (indentSize - 1) ts
colorize indentSize ((OpenBracket):ts) = "<br/>" ++ indents indentSize ++ "<span class='bracket'>[</span><br/>" ++ indents (indentSize + 1) ++ colorize (indentSize + 1) ts
colorize indentSize ((CloseBracket):ts) = "<br/>" ++ indents (indentSize - 1) ++ "<span class='bracket'>]</span>" ++ colorize (indentSize - 1) ts
colorize indentSize ((Comma):ts) = "<span class='comma'>,</span><br/>" ++ indents indentSize ++ colorize indentSize ts
colorize indentSize ((Colon):ts) = "<span class='_space'></span><span class='colon'>:</span><span class='_space'></span>" ++ colorize indentSize ts
colorize indentSize ((Keyword str):ts) = "<span class='value_keyword'>"++ str ++ "</span>" ++ colorize indentSize ts
colorize indentSize ((NumberToken str):ts) = "<span class='value_number'>"++ str ++ "</span>" ++ colorize indentSize ts
colorize indentSize ((StringToken str):ts) = "<span class='value_string'>"++ replaceHtmlSpecialChar str ++ "</span>" ++ colorize indentSize ts
colorize indentSize ((StringEsacpedPart str):ts) = "<span class='preprocess_string'>"++ str ++ "</span>" ++ colorize indentSize ts
