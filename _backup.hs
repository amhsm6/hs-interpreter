import Control.Applicative
import Data.Char
import Data.List

newtype State a = State (Bindings -> (Bindings, a))

instance Monad State where
    return x = State $ \b -> (b, x)
    (State f) >>= g = State $ \b -> f b

instance Applicative State where
    pure = return
    f <*> a = f >>= (\g -> a >>= return . g)

instance Functor State where
    fmap f a = a >>= return . f

currentBindings :: State Bindings
currentBindings = State (\s -> (s, s))

getVar "foo" >>= (\x -> currentBindings >>= (\bindings -> pure $ value b x))

do
    var <- getVar "foo"
    bindings <- currentBindings
    getFromBindings bindings var

type Frame = [(String, Expr)]
type Bindings = [Frame]



newFrame :: Bindings -> Bindings
newFrame b = b ++ [[]]

{-
State a x = a -> (a, x)
IO = State RealWord
-}

execute :: Stmt -> State Bindings ()

addVar :: String -> Expr -> State ()
getVar :: String -> State Expr

add :: Bindings -> String -> Expr -> Bindings
add b name expr = case lookup name lastFrame of Just _ -> error $ "variable " ++ name ++ " already present"
                                                Nothing -> restFrames ++ [lastFrame ++ [(name, expr)]]
    where (lastFrame:restFrames) = if length b == 0 then ([]:[]) else reverse b

change :: Bindings -> String -> Expr -> Bindings --TODO: Fix change
change (_:b) name expr = new --if new == b then error $ "variable " ++ name ++ " not found" else new --FIXME
    where new = map processFrame b
          processFrame frame = case lookup name frame of Just _ -> frame ++ [(name, expr)] --FIXME
                                                         Nothing -> frame

get :: Bindings -> String -> Expr
get b name = case reverse $ filter ((==) name . fst) $ concat b of (e:_) -> snd e
                                                                   [] -> error $ "variable " ++ name ++ " not found"

data Stmt = AddVarStmt Expr Expr
          | ChangeStmt Expr Expr
          | EvalStmt Expr

execute :: Stmt -> Bindings -> Bindings
execute (AddVarStmt (VarExpr name) expr) b = add b name $ value expr b
execute (ChangeStmt cell expr) b = mutate cell b $ value expr b
execute (EvalStmt expr) b = value expr b `seq` b

instance Show Stmt where
    show (AddVarStmt name expr) = show name ++ " := " ++ show expr
    show (ChangeStmt cell expr) = show cell ++ " = " ++ show expr
    show (EvalStmt expr) = show expr

newtype Block = Block [Stmt]

bexecute :: Block -> Bindings -> Bindings
bexecute (Block block) b = foldr execute (newFrame b) $ reverse block

instance Show Block where
    show (Block block) = "{\n" ++ (concat $ map (intercalate "    " . lines . show) block) ++ "}"

type Definition = Stmt

defConst :: String -> Expr -> Definition
defConst name expr = addVar name expr

define :: String -> [String] -> [Stmt] -> Definition
define name args body = addVar name (Function name args (Block body))

type Program = [Definition]

run :: Program -> [Expr] -> Expr
run prog builtins = value (call (get b' "main") []) b'
    where b' = foldr execute b prog
          b = foldl (\acc f@(Builtin name _) -> add acc name f) [] builtins

data Expr = IntExpr Integer
          | BoolExpr Bool
          | TextExpr String
          | AddExpr Expr Expr
          | SubExpr Expr Expr
          | MulExpr Expr Expr
          | DivExpr Expr Expr
          | ModExpr Expr Expr
          | AndExpr Expr Expr
          | OrExpr Expr Expr
          | NotExpr Expr
          | LtExpr Expr Expr
          | LeExpr Expr Expr
          | EqExpr Expr Expr
          | GeExpr Expr Expr
          | GtExpr Expr Expr
          | VarExpr String
          | Pointer Bindings Expr
          | RefExpr Expr
          | DerefExpr Expr
          | Function String [String] Block
          | Builtin String ([Expr] -> Expr)
          | CallExpr Expr [Expr]
          | IOExpr (IO ())

mutate :: Expr -> Bindings -> Expr -> Bindings
mutate (VarExpr name) b new = change b name new
mutate (DerefExpr (Pointer b cell)) _ new = mutate cell b new --TODO: fix mutation

value :: Expr -> Bindings -> Expr
value (CallExpr expr args) b =
    let args' = map (\a -> value a b) args
    in case value expr b of Function name arg_names block -> get (bexecute block arg_bindings) name
                                where arg_bindings = foldl (\acc (a, n) -> add acc n a) (newFrame [b !! 0]) $ zip args' arg_names
                            Builtin _ f -> f args'
                            _ -> error "not a function"
value f@(Builtin _ _) _ = f
value f@(Function _ _ _) _ = f
value (DerefExpr x) b = case value x b of Pointer b' cell -> value cell b'
                                          _ -> error "type error"
value (RefExpr x) b = Pointer b x
value p@(Pointer _ _) _ = p
value (VarExpr name) b = get b name
value x@(IntExpr _) _ = x
value x@(BoolExpr _) _ = x
value x@(TextExpr _) _ = x
value (AddExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> IntExpr $ x + y --TODO: refactor somehow
                                         _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (SubExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> IntExpr $ x - y
                                         _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (MulExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> IntExpr $ x * y
                                         _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (DivExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> IntExpr $ if y == 0 then 0 else x `div` y
                                         _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (ModExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> IntExpr $ if y == 0 then x else x `mod` y
                                         _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (AndExpr l r) b = case (l', r') of ((BoolExpr x), (BoolExpr y)) -> BoolExpr $ x && y
                                         _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (OrExpr l r) b = case (l', r') of ((BoolExpr x), (BoolExpr y)) -> BoolExpr $ x || y
                                        _   -> error "type error"
    where (l', r') = (value l b, value r b)
value (NotExpr a) b = case a' of (BoolExpr x) -> BoolExpr $ not x
                                 _ -> error "type error"
    where a' = value a b
value (LtExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> BoolExpr $ x < y
                                        _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (LeExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> BoolExpr $ x <= y
                                        _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (EqExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> BoolExpr $ x == y
                                        _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (GeExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> BoolExpr $ x >= y
                                        _ -> error "type error"
    where (l', r') = (value l b, value r b)
value (GtExpr l r) b = case (l', r') of ((IntExpr x), (IntExpr y)) -> BoolExpr $ x > y
                                        _ -> error "type error"
    where (l', r') = (value l b, value r b)
value x y = error $ "type error" ++ show x ++ " >>= " ++ show y

instance Show Expr where
    show (CallExpr (VarExpr name) args) = "CALL[" ++ name ++ ", " ++ (concat . intersperse ", " . map show $ args) ++ "]"
    show (Builtin name _) = "<BUILTIN " ++ name ++ ">"
    show (Function _ args block) = "FUNCTION[" ++ (concat $ intersperse ", " args) ++ "] " ++ show block
    show (DerefExpr x) = "*" ++ show x
    show (RefExpr x) = "&" ++ show x
    show (Pointer _ cell) = "<POINTER TO " ++ show cell ++ ">"
    show (VarExpr name) = name
    show (IntExpr x) = show x
    show (BoolExpr x) = if x then "TRUE" else "FALSE"
    show (TextExpr x) = x
    show (AddExpr l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
    show (SubExpr l r) = "(" ++ show l ++ " - " ++ show r ++ ")"
    show (MulExpr l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
    show (DivExpr l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
    show (ModExpr l r) = "(" ++ show l ++ " % " ++ show r ++ ")"
    show (AndExpr l r) = "(" ++ show l ++ " AND " ++ show r ++ ")"
    show (OrExpr l r) = "(" ++ show l ++ " OR " ++ show r ++ ")"
    show (NotExpr x) = "NOT " ++ show x
    show (LtExpr l r) = "(" ++ show l ++ " < " ++ show r ++ ")"
    show (LeExpr l r) = "(" ++ show l ++ " <= " ++ show r ++ ")"
    show (EqExpr l r) = "(" ++ show l ++ " == " ++ show r ++ ")"
    show (GeExpr l r) = "(" ++ show l ++ " >= " ++ show r ++ ")"
    show (GtExpr l r) = "(" ++ show l ++ " > " ++ show r ++ ")"

int x = IntExpr $ read x
bool x = BoolExpr x
text x = TextExpr x
var x = VarExpr x
ref x = RefExpr x
deref x = DerefExpr x
call f args = CallExpr f args

addVar name expr = AddVarStmt (var name) expr
changeS cell expr = ChangeStmt cell expr
eval expr = EvalStmt expr

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

Parser = StateT String Maybe

instance Monad Parser where
    return x = Parser $ \input -> Just (x, input)
    (Parser p) >>= f = Parser $ \input -> p input >>= (\(x,input') -> runParser (f x) input')

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser a) <|> (Parser b) = Parser $ \input -> maybe (b input) return (a input)

instance Applicative Parser where
    pure = return
    f <*> a = f >>= (\g -> a >>= return . g)

instance Functor Parser where
    fmap f a = a >>= return . f

instance MonadFail Parser where
    fail _ = Parser $ \_ -> Nothing

charF :: (Char -> Bool) -> Parser Char
charF f = Parser g
    where g "" = Nothing
          g (x:xs)
              | f x = Just (x, xs)
              | otherwise = Nothing

char :: Char -> Parser Char
char c = charF (==c)

ws :: Parser Char
ws = charF isSpace

digit :: Parser Char
digit = charF isDigit

string :: String -> Parser String
string s = sequence $ map char s

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just $ span f input

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = (p >>= (\x -> (many $ sep >> many ws >> p) >>= return . (x:))) <|> return []

parseInt :: Parser Expr
parseInt = some digit >>= return . int

parseBool :: Parser Expr
parseBool = (string "FALSE" >> (return $ bool False)) <|> (string "TRUE" >> (return $ bool True))

parseText :: Parser Expr
parseText = (char '"' *> spanP (/='"') <* char '"') >>= return . text

keywords :: [String]
keywords = ["DEFINE", "FUNCTION", "CALL"]

verifyVar :: Parser Expr -> Parser Expr
verifyVar p = p >>= (\v@(VarExpr name) -> case elem name keywords of True -> empty
                                                                     False -> return v)

parseVar :: Parser Expr
parseVar = charF (\x -> isLetter x || x == '_') >>= (\c -> spanP (\x -> isLetter x || isDigit x || x == '_') >>= return . (c:)) >>= verifyVar . return . var

ops :: [(String, Expr -> Expr -> Expr)]
ops = [ ("+", AddExpr)
      , ("-", SubExpr)
      , ("*", MulExpr)
      , ("/", DivExpr)
      , ("%", ModExpr)
      , ("AND", AndExpr)
      , ("OR", OrExpr)
      , ("<", LtExpr)
      , ("<=", LeExpr)
      , ("==", EqExpr)
      , (">=", GeExpr)
      , (">", GtExpr)
      ]

parseBinOp :: Parser (Expr -> Expr -> Expr)
parseBinOp = (foldl (<|>) empty $ map (string . fst) ops) >>= (\x -> case lookup x ops of Just e -> return e
                                                                                          Nothing -> undefined)

parseBinary :: Parser Expr
parseBinary = char '(' *> (parseExpr >>= (\x -> some ws >> parseBinOp >>= (\o -> some ws >> parseExpr >>= (\y -> return $ o x y)))) <* char ')'

parseNot :: Parser Expr
parseNot = char '(' *> (string "NOT" >> some ws >> parseExpr >>= (\e -> return $ NotExpr e)) <* char ')'

parseRef :: Parser Expr
parseRef = char '&' >> parseVar >>= return . ref

parseDeref :: Parser Expr
parseDeref = char '*' >> parseVar >>= return . deref

parseUnary :: Parser Expr
parseUnary = parseNot <|> parseRef <|> parseDeref

parseAddVar :: Parser Stmt
parseAddVar = do
    (VarExpr v) <- parseVar
    many ws
    string ":="
    many ws
    expr <- parseExpr
    return $ addVar v expr

parseChange :: Parser Stmt
parseChange = do
    cell <- parseVar <|> parseDeref
    many ws
    char '='
    many ws
    expr <- parseExpr
    return $ changeS cell expr

parseEval :: Parser Stmt
parseEval = parseExpr >>= return . eval

parseStmt :: Parser Stmt
parseStmt = parseAddVar <|> parseChange <|> parseEval

parseFunction :: Parser Expr
parseFunction = do
    string "FUNCTION["
    args <- sepBy (char ',') parseVar
    char ']'
    many ws
    char '{'
    many ws
    block <- sepBy ws parseStmt
    many ws
    char '}'
    return $ Function "" (map (\(VarExpr name) -> name) args) (Block block)

parseCall :: Parser Expr
parseCall = do
    string "CALL["
    f <- parseVar
    many $ char ','
    many ws
    args <- sepBy (char ',') parseExpr
    char ']'
    return $ call f args

parseExpr :: Parser Expr
parseExpr = parseInt <|> 
            parseBool <|>
            parseText <|>
            parseBinary <|>
            parseUnary <|>
            parseVar <|>
            parseFunction <|>
            parseCall

parseDefinition :: Parser Definition
parseDefinition = do
    string "DEFINE"
    some ws
    (VarExpr name) <- parseVar
    many ws
    string ":="
    many ws
    expr <- parseExpr
    case expr of Function _ args (Block body) -> return $ define name args body
                 _ -> return $ defConst name expr

parseProgram :: Parser Program
parseProgram = sepBy ws parseDefinition

foo :: [Expr] -> Expr
foo [(IntExpr x)] = IOExpr $ print x
foo [(TextExpr x)] = IOExpr $ print x
foo [(BoolExpr x)] = IOExpr $ print x
foo _ = error "wrong argument to print function"

builtinFunctions :: [Expr]
builtinFunctions = [Builtin "print" foo]

main = do
    input <- getContents
    case runParser parseProgram input of 
        Just (prog, _)
            | null prog -> error "parse error"
            | otherwise -> case run prog builtinFunctions of IOExpr io -> io
                                                             _ -> undefined
        Nothing -> error "parse error"
