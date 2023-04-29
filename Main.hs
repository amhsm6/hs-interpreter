--getVar "foo" >>= (\x -> currentBindings >>= (\bindings -> pure $ value b x))
{-
do
    var <- getVar "foo"
    bindings <- currentBindings
    getFromBindings bindings var
-}

--Parser = StateT String Maybe
{-
State a x = a -> (a, x)
IO = State RealWord
-}
{-
execute :: Stmt -> State Bindings ()

addVar :: String -> Expr -> State ()
getVar :: String -> State Expr
-}

import Control.Applicative
import Data.Char
import Data.List

newtype State a = State { runState :: Bindings -> (Bindings, a) }

instance Monad State where
    return = pure
    (State m) >>= g = State $ \b -> let (b', x) = m b
                                    in runState (g x) b'

instance Applicative State where
    pure x = State $ \b -> (b, x)
    f <*> a = f >>= \g -> a >>= pure . g

instance Functor State where
    fmap f a = a >>= pure . f

put :: Bindings -> State ()
put b = State $ \_ -> (b, ())

currentBindings :: State Bindings
currentBindings = State $ \b -> (b, b)

type Frame = [(String, Expr)]
type Bindings = [Frame]

newFrame :: Bindings -> Bindings
newFrame b = b ++ [[]]

add :: String -> Expr -> State ()
add name expr = State $ \b -> let (lastFrame:restFrames) = if length b == 0 then ([]:[]) else reverse b
                              in case lookup name lastFrame of
                                     Just _ -> error $ "variable " ++ name ++ " already present"
                                     Nothing -> (restFrames ++ [lastFrame ++ [(name, expr)]], ())

change :: String -> Expr -> State () --TODO: Fix change
change name expr = State $ \(_:b) -> let new = map processFrame b
                                         processFrame frame = case lookup name frame of
                                                                  Just _ -> frame ++ [(name, expr)] --FIXME
                                                                  Nothing -> frame
                                     in (new, ()) --FIXME: report an error when variable not found

get :: String -> State Expr
get name = State $ \b -> case reverse $ filter ((==) name . fst) $ concat b of
                             (e:_) -> (b, snd e)
                             [] -> error $ "variable " ++ name ++ " not found"

data Stmt = AddVarStmt Expr Expr
          | ChangeStmt Expr Expr
          | EvalStmt Expr

addVar :: Expr -> Expr -> Stmt
addVar = AddVarStmt

changeS :: Expr -> Expr -> Stmt
changeS = ChangeStmt

eval :: Expr -> Stmt
eval = EvalStmt

execute :: Stmt -> State ()
execute (AddVarStmt (VarExpr name) expr) = add name expr
execute (ChangeStmt cell expr) = undefined--mutate cell $ value expr
execute (EvalStmt expr) = undefined

instance Show Stmt where
    show (AddVarStmt name expr) = show name ++ " := " ++ show expr
    show (ChangeStmt cell expr) = show cell ++ " = " ++ show expr
    show (EvalStmt expr) = show expr

newtype Block = Block [Stmt]

bexecute :: Block -> State ()
bexecute (Block block) = sequence_ $ map execute block

instance Show Block where
    show (Block block) = "{\n" ++ (concat $ map (intercalate "    " . lines . show) block) ++ "}"

type Definition = Stmt

{-
defConst :: String -> Expr -> Definition
defConst name expr = addVar name expr

define :: String -> [String] -> [Stmt] -> Definition
define name args body = addVar name (Function name args (Block body))

type Program = [Definition]

run :: Program -> [Expr] -> Expr
run prog builtins = value (call (get b' "main") []) b'
    where b' = foldr execute b prog
          b = foldl (\acc f@(Builtin name _) -> add acc name f) [] builtins
-}

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

int :: Integer -> Expr
int = IntExpr

bool :: Bool -> Expr
bool = BoolExpr

text :: String -> Expr
text = TextExpr

var :: String -> Expr
var = VarExpr

ptr :: Bindings -> Expr -> Expr
ptr = Pointer

ref :: Expr -> Expr
ref = RefExpr

deref :: Expr -> Expr
deref = DerefExpr

func :: String -> [String] -> Block -> Expr
func = Function

call :: Expr -> [Expr] -> Expr
call = CallExpr

mutate :: Expr -> Expr -> State ()
mutate (VarExpr name) new = change name new
mutate (DerefExpr expr) new = value expr >>= \p ->
    case p of
        Pointer _ cell -> mutate cell new --FIXME
        _ -> error "type error"

value :: Expr -> State Expr
value (CallExpr expr args) = do
    values <- sequence $ map value args --TODO: Replace with mapM or something
    f <- value expr
    case f of
        Function name arg_names block -> do
            b <- currentBindings
            put $ newFrame [b !! 0]
            sequence_ $ zipWith add arg_names values
            bexecute block
            ret <- get name
            put b
            pure ret
        Builtin _ f -> pure $ f values
        _ -> error "type error"
value f@(Builtin _ _) = pure f
value f@(Function _ _ _) = pure f
value (DerefExpr x) = value x >>= \p -> case p of Pointer _ cell -> value cell --FIXME
                                                  _ -> error "type error"
value (RefExpr x) = currentBindings >>= \b -> pure $ ptr b x
value p@(Pointer _ _) = pure p
value (VarExpr name) = get name
value (IntExpr x) = pure $ int x
value (BoolExpr x) = pure $ bool x
value (TextExpr x) = pure $ text x
value (AddExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ int $ x + y
        _ -> error "type error"
value (SubExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ int $ x - y
        _ -> error "type error"
value (MulExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ int $ x * y
        _ -> error "type error"
value (DivExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ int $ if y == 0 then 0 else x `div` y
        _ -> error "type error"
value (ModExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ int $ if y == 0 then 1 else x `mod` y
        _ -> error "type error"
value (AndExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (BoolExpr x, BoolExpr y) -> pure $ bool $ x && y
        _ -> error "type error"
value (OrExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (BoolExpr x, BoolExpr y) -> pure $ bool $ x || y
        _ -> error "type error"
value (NotExpr a) = do
    a' <- value a
    case a' of
        BoolExpr x -> pure $ bool $ not x
        _ -> error "type error"
value (LtExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ bool $ x < y
        _ -> error "type error"
value (LeExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ bool $ x <= y
        _ -> error "type error"
value (EqExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ bool $ x == y
        _ -> error "type error"
value (GeExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ bool $ x >= y
        _ -> error "type error"
value (GtExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ bool $ x > y
        _ -> error "type error"

instance Show Expr where
    show (CallExpr (VarExpr name) args) = "CALL[" ++ name ++ ", " ++ (concat . intersperse ", " . map show $ args) ++ "]"
    show (Builtin name _) = "<BUILTIN " ++ name ++ ">"
    --show (Function _ args block) = "FUNCTION[" ++ (concat $ intersperse ", " args) ++ "] " ++ show block
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

{-
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Monad Parser where
    pure = return
    (Parser p) >>= f = Parser $ \input -> p input >>= (\(x,input') -> runParser (f x) input')

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser a) <|> (Parser b) = Parser $ \input -> maybe (b input) pure (a input)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    f <*> a = f >>= \g -> a >>= pure . g

instance Functor Parser where
    fmap f a = a >>= pure . f

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
sepBy sep p = (p >>= (\x -> (many $ sep >> many ws >> p) >>= pure . (x:))) <|> return []

parseInt :: Parser Expr
parseInt = some digit >>= pure . int

parseBool :: Parser Expr
parseBool = (string "FALSE" >> (pure $ bool False)) <|> (string "TRUE" >> (return $ bool True))

parseText :: Parser Expr
parseText = (char '"' *> spanP (/='"') <* char '"') >>= pure . text

keywords :: [String]
keywords = ["DEFINE", "FUNCTION", "CALL"]

verifyVar :: Parser Expr -> Parser Expr
verifyVar p = p >>= (\v@(VarExpr name) -> case elem name keywords of True -> empty
                                                                     False -> pure v)

parseVar :: Parser Expr
parseVar = charF (\x -> isLetter x || x == '_') >>= (\c -> spanP (\x -> isLetter x || isDigit x || x == '_') >>= pure . (c:)) >>= verifyVar . return . var

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
parseBinOp = (foldl (<|>) empty $ map (string . fst) ops) >>= (\x -> case lookup x ops of Just e -> pure e
                                                                                          Nothing -> undefined)

parseBinary :: Parser Expr
parseBinary = char '(' *> (parseExpr >>= (\x -> some ws >> parseBinOp >>= (\o -> some ws >> parseExpr >>= (\y -> pure $ o x y)))) <* char ')'

parseNot :: Parser Expr
parseNot = char '(' *> (string "NOT" >> some ws >> parseExpr >>= (\e -> pure $ NotExpr e)) <* char ')'

parseRef :: Parser Expr
parseRef = char '&' >> parseVar >>= pure . ref

parseDeref :: Parser Expr
parseDeref = char '*' >> parseVar >>= pure . deref

parseUnary :: Parser Expr
parseUnary = parseNot <|> parseRef <|> parseDeref

parseAddVar :: Parser Stmt
parseAddVar = do
    (VarExpr v) <- parseVar
    many ws
    string ":="
    many ws
    expr <- parseExpr
    pure $ addVar v expr

parseChange :: Parser Stmt
parseChange = do
    cell <- parseVar <|> parseDeref
    many ws
    char '='
    many ws
    expr <- parseExpr
    pure $ changeS cell expr

parseEval :: Parser Stmt
parseEval = parseExpr >>= pure . eval

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
    pure $ Function "" (map (\(VarExpr name) -> name) args) (Block block)

parseCall :: Parser Expr
parseCall = do
    string "CALL["
    f <- parseVar
    many $ char ','
    many ws
    args <- sepBy (char ',') parseExpr
    char ']'
    pure $ call f args

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
    case expr of Function _ args (Block body) -> pure $ define name args body
                 _ -> pure $ defConst name expr

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
-}
main :: IO ()
main = pure ()
