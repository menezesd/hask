import Data.Char

type Parser a = String -> [(a, String)]

ret :: a -> Parser a
ret v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                 []     -> []
                 (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp
-- > parse (ret 1) "abc"
-- [(1, "abc")]
-- > parse failure "abc"
-- []
-- > parse item ""
-- []
-- > parse item "abc"
-- [('a', "bc")]

-- (>>=) bind
-- Parser a -> Parser b -> Parser (a, b)
(=>>) :: Parser a -> (a -> Parser b) -> Parser b
p =>> f = \inp -> case parse p inp of
                    []         -> []
                    [(v, out)] -> parse (f v) out
{-
p1 >>= \v1 ->
p2 >>= \v2 ->
...
pn >>= \vn ->
return (f v1 v2 ... vn)
    |
    v
do v1 <- p1
   v2 <- p2
   ...
   vn <- pn
   return (f v1 v2 ... vn)
-}
-- bind
p :: Parser (Char, Char)
p = item =>> \x ->
    item =>> \_ ->
    item =>> \y ->
    ret (x, y)
-- > parse p "abcdef"
-- [(('a', 'c'), "def")]
-- > parse p "ab"
-- []

-- selection
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    []         -> parse q inp
                    [(v, out)] -> [(v, out)]
-- > parse (item +++ ret 'd') "abc"
-- [('a', "bc")]
-- > parse (item +++ ret 'd') ""
-- [('d', "")]
-- > parse (failure +++ failure) "abc"
-- []

-- single characte parsers
sat :: (Char -> Bool) -> Parser Char
sat p = item =>> \x ->
        if p x then ret x
               else failure

digit :: Parser Char
digit = sat isDigit
-- > parse digit "123"
-- [('1', "23")]

lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)
-- > parse (char 'a') "abc"
-- [('a', "bc")]

string :: String -> Parser String
string []     = ret []
string (x:xs) = char x =>> \_ ->
                string xs =>> \_ ->
                ret (x:xs)
-- > parse (string "abc") "abcdef"
-- [("abc", "def")]

-- string parsers
many' :: Parser a -> Parser [a]
many' p = many1 p +++ ret []
many1 :: Parser a -> Parser [a]
many1 p = p =>> \v ->
          many' p =>> \vs ->
          ret (v:vs)
-- > parse (many' digit) "123abc"
-- [("123", "abc")]
-- > parse (many' digit) "abcdef"
-- [("", "abcdef")]
-- > parse (many1 digit) "abcdef"
-- []

ident :: Parser String
ident = lower =>> \x ->
        many' alphanum =>> \xs ->
        ret (x:xs)
-- > parse ident "abc def"
-- [("abc", " def")]

nat :: Parser Int
nat = many1 digit =>> \xs ->
      ret (read xs)
-- > parse nat "123 abc"
-- [(123, " abc")]

space :: Parser ()
space = many' (sat isSpace) =>> \_ ->
        ret ()
-- > parse space "  abc"
-- [((), "abc")]

token :: Parser a -> Parser a
token p = space =>> \_ ->
          p =>> \v ->
          space =>> \_ ->
          ret v
-- > parse (token nat) "  123  abc"
-- [(123, "abc")]

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

parseList :: Parser [Int] -- parse non-empty list of natural numbers
parseList = symbol "[" =>> \_ ->
            natural =>> \n ->
            many' (symbol "," =>> \_ -> natural) =>> \ns ->
            symbol "]" =>> \_ ->
            ret (n:ns)
-- > parse parseList " [1, 2, 3] "
-- [([1,2,3], "")]
-- > parse parseList " [1, 2,] "
-- []

-- simple formula BNF (assume right assoc)
--   expr ::= term ('+' expr | e)
--   term ::= factor ('*' term | e)
--   factor ::= '(' expr ')' | nat
--   nat ::= '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
--   (e = empty)

expr :: Parser Int
expr = term =>> \t ->
         (symbol "+" =>> \_ ->
           expr =>> \e ->
             ret (t + e))
         +++ ret t

term :: Parser Int
term = factor =>> \f ->
         (symbol "*" =>> \_ ->
           term =>> \t ->
             ret (f * t))
         +++ ret f

factor :: Parser Int
factor = (symbol "(" =>> \_ ->
           expr =>> \e ->
             symbol ")" =>> \_ ->
               ret e)
         +++ natural

eval :: String -> Int
eval xs = case parse expr' xs of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "invalid input"
-- > eval "2*3+4"
-- 10
-- > eval "2 * (3 + 4)"
-- 14
-- > eval "2*3-4"
-- Error: unused input -4
-- > eval "-1"
-- Error: invalid input

-- 8.10 (p.103) ----------------------------------------------------------------

-- 1 -------------------------------------------------------
int :: Parser Int
int = (symbol "-" =>> \_ ->
        natural =>> \i ->
        ret (-i))
      +++ natural

-- 2 -------------------------------------------------------
comment :: Parser ()
comment = symbol "--" =>> \_ ->
          many' (sat (/= '\n')) =>> \_ ->
          char '\n' =>> \_ ->
          ret ()

-- 6 -------------------------------------------------------
-- expr ::= term ('+' expr | '-' expr | e)
-- term ::= factor ('*' term | '/' term | e)

expr' :: Parser Int
expr' = term' =>> \t ->
              (symbol "+" =>> \_ ->
                expr' =>> \e ->
                  ret (t + e))
          +++ (symbol "-" =>> \_ ->
                expr' =>> \e ->
                  ret (t - e))
          +++ ret t

term' :: Parser Int
term' = factor' =>> \f ->
              (symbol "*" =>> \_ ->
                term' =>> \t ->
                  ret (f * t))
          +++ (symbol "/" =>> \_ ->
                term' =>> \t ->
                  ret (f `div` t))
          +++ ret f

factor' :: Parser Int
factor' = (symbol "(" =>> \_ ->
            expr' =>> \e ->
              symbol ")" =>> \_ ->
                ret e)
          +++ int

-- > expr' "2 - 1"
-- [(1, "")]
-- > expr' "4 / 2"
-- [(2, "")]

-- term1 ::= term2 ('*' term1 | '/' term1 | e)
-- term2 ::= factor ('^' term2 | e)

term1 = term2 =>> \t2 ->
               (symbol "*" =>> \_ ->
                 term1 =>> \t1 ->
                   ret (t2 * t1))
           +++ (symbol "/" =>> \_ ->
                 term1 =>> \t1 ->
                   ret (t2 `div` t1))
           +++ ret t2

term2 :: Parser Int
term2 = factor =>> \f ->
             (symbol "^" =>> \_ ->
               term2 =>> \t2 ->
                 ret (f ^ t2))
          +++ ret f
-- > term1 "2^3 * 4"
-- [(32, "")]


