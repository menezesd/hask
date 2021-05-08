
{-  a Parser for things is a Function, from Strings
 to Lists of 
 Pairs of
 things and Strings
-}
type Parser a = String -> [(a, String)]

{- 
-- Given v, return parser that always returns v
return v = \x -> [(v, x)]
-}

-- parser that always fails
failure = \x -> []

item :: Parser Char
item = \inp -> case inp of
	[] -> []
	(x:xs) -> [(x, xs)]


-- just calls the parser
parse p inp = p inp


--- compose two parsers

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
	[] -> []
	[(v,out)] -> parse (f v) out




p = do { x <- item ;
	item ;
	y <- item ;
	return (x,y) }
	

sat p = do { x <- item;
if p x then return x else failure }





