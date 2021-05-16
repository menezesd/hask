datetype Maybe = Just x | None deriving (Eq, Show)



instance Functor Maybe where
	fmap _ None = None
	fmap f (Just a) = Just (f a)


instance Applicative Maybe where
	None <*> _ = None
	pure = Just
	(Just f) <*> x = fmap f x
	
(<*>) :: Applicative => f (a -> b) -> f a -> f b

instance Monad Maybe where
	return a = Just a
	bind f fa = join fmap f fa 
	

	
bind :: Functor f => f a -> (a -> f b) -> f b


(>>=) :: Functor f => f a -> (a -> f b) -> f b
fa >>= f = fmap f fa


fmap f x = do { y <- x; return (f y) }

pure = return
fmap 


{
    "name": "String",
    "age": "?Int",
    "address_history": [{
        "line1": "String",
        "line2": "String",
        "postcode": "String",
    }],
}

data JSONType = String | Integer | Array JSONType | Map [(String, JSONType)] | Nullable JSONType deriving (Eq, Ord, Show)

data JSONValue = JString String | JInt Integer | JArray [JSONValue] | Null | JMap [(String, JSONValue)]

validateh (a,b) (c,d) = if (a == b) then validate b d else Left "Type error"


validate :: JSONType -> JSONValue -> Either Text ()
validate String (Jstring x) = Right ()
validate String _ = Left "Expected string"
validate Integer (JInt x) = Right ()
validate Integer _ = Left "Expected Integer"
validate (Nullable x) Null = Right ()
validate (Nullable x) y = validate x y
validate (Array x) (yArray y) = mapM_ (validate x) y
validate (Map []) JMap [] = Right ()
validate (Map []) _ = Left "should be null list"
validate _ Jmap [] = Left "expected more values"
validate (Map ((a,b):xs)) JMap ((c,d): ys) = case validateh (a,b) (c,d) of 
	| Right () -> validate xs ys
	| _ -> _

