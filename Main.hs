import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.IORef
import Data.Char
import qualified Data.Map as M
import Data.List

type ProgramState = StateT Bindings IO 

type Frame = IORef (M.Map String Expr)
type Bindings = [Frame]

newFrame :: ProgramState ()
newFrame = get >>= \b -> (liftIO $ newIORef M.empty) >>= \f -> put $ b ++ [f]

removeFrame :: ProgramState ()
removeFrame = get >>= put . init

add :: String -> Expr -> ProgramState ()
add name expr = do
    b <- get
    if length b == 0 then newFrame else pure ()

    b' <- get
    let lastFrameRef = last b'
    liftIO $ readIORef lastFrameRef >>= \lastFrame ->
        case M.lookup name lastFrame of
            Just  _ -> error $ "variable " ++ name ++ " already present"
            Nothing -> modifyIORef lastFrameRef $ M.insert name expr

change :: String -> Expr -> ProgramState ()
change name expr = do
    b <- get
    let lookups = forM b $ \r -> readIORef r >>= pure . (const (Just r) <=< M.lookup name)
    liftIO $ lookups >>= \refs ->
        case foldl (<|>) Nothing $ reverse refs of
            Just r -> modifyIORef r $ M.update (const $ Just expr) name
            Nothing -> error $ "variable " ++ name ++ " not found"

fetch :: String -> ProgramState Expr
fetch name = do
    b <- get
    let lookups = forM b $ \r -> readIORef r >>= pure . M.lookup name
    liftIO $ lookups >>= \exprs ->
        case foldl (<|>) Nothing $ reverse exprs of
            Just expr -> pure expr
            Nothing -> error $ "variable " ++ name ++ " not found"

data Stmt = Block [Stmt]
          | AddVarStmt Expr Expr
          | ChangeStmt Expr Expr
          | EvalStmt Expr
          | IfStmt Expr Stmt
          | IfElseStmt Expr Stmt Stmt
          | WhileStmt Expr Stmt

addVar :: String -> Expr -> Stmt
addVar name = AddVarStmt $ var name

changeS :: Expr -> Expr -> Stmt
changeS = ChangeStmt

eval :: Expr -> Stmt
eval = EvalStmt

ifS :: Expr -> Stmt -> Stmt
ifS = IfStmt

ifElseS :: Expr -> Stmt -> Stmt -> Stmt
ifElseS = IfElseStmt

while :: Expr -> Stmt -> Stmt
while = WhileStmt

execute :: Stmt -> ProgramState ()
execute (Block stmts) = do
    newFrame
    mapM_ execute stmts
    removeFrame
execute (AddVarStmt (VarExpr name) expr) = value expr >>= add name
execute (ChangeStmt cell expr) = value expr >>= mutate cell
execute (EvalStmt expr) = value expr >> pure ()
execute (IfStmt cond block) = value cond >>= \x ->
    case x of BoolExpr y -> if y then execute block else pure ()
              _ -> error "type error"
execute (IfElseStmt cond blockTrue blockFalse) = value cond >>= \x ->
    case x of BoolExpr y -> execute $ if y then blockTrue else blockFalse
              _ -> error "type error"
execute while@(WhileStmt cond block) = value cond >>= \x ->
    case x of BoolExpr y -> if y then execute block >> execute while else pure ()
              _ -> error "type error"

instance Show Stmt where
    show (Block block) = "{\n" ++ (concat $ map (intercalate "    " . lines . show) block) ++ "}"
    show (AddVarStmt name expr) = show name ++ " := " ++ show expr
    show (ChangeStmt cell expr) = show cell ++ " = " ++ show expr
    show (EvalStmt expr) = show expr
    show (IfStmt cond block) = intercalate " " ["IF", show cond, show block]
    show (IfElseStmt cond blockTrue blockFalse) = intercalate " " ["IF", show cond, show blockTrue, "ELSE", show blockFalse]
    show (WhileStmt cond block) = intercalate " " ["WHILE", show cond, show block]

type Definition = Stmt

defineConst :: String -> Expr -> Definition
defineConst name expr = addVar name expr

defineFunc :: String -> [String] -> Stmt -> Definition
defineFunc name args body = addVar name $ func name args body

type Program = [Definition]

run :: Program -> [Expr] -> ProgramState ()
run prog builtins = mapM_ (\f@(Builtin name _) -> add name f) builtins >> mapM_ execute prog >> (execute $ eval $ call (var "main") [])

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
          | Function String [String] Stmt
          | Builtin String ([Expr] -> ProgramState Expr)
          | CallExpr Expr [Expr]

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

func :: String -> [String] -> Stmt -> Expr
func = Function

call :: Expr -> [Expr] -> Expr
call = CallExpr

mutate :: Expr -> Expr -> ProgramState ()
mutate (VarExpr name) new = change name new
mutate (DerefExpr expr) new = value expr >>= \p ->
    case p of
        Pointer b cell -> do
            b' <- get
            put b
            mutate cell new
            put b'
        _ -> error "type error"

value :: Expr -> ProgramState Expr
value (CallExpr expr args) = do
    values <- mapM value args
    f <- value expr
    case f of
        Function name argNames block -> do
            b <- get
            put [b !! 0]
            newFrame
            zipWithM_ add argNames values
            execute block
            ret <- fetch name
            put b
            pure ret
        Builtin _ g -> g values
        _ -> error "type error"
value f@(Builtin _ _) = pure f
value f@(Function _ _ _) = pure f
value (DerefExpr x) = value x >>= \p ->
    case p of
        Pointer b cell -> do
            b' <- get
            put b
            v <- value cell
            put b'
            pure v
        _ -> error "type error"
value (RefExpr x) = get >>= \b -> pure $ ptr b x
value p@(Pointer _ _) = pure p
value (VarExpr name) = fetch name
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
    show (CallExpr name args) = "CALL[" ++ show name ++ ", " ++ (intercalate ", " . map show $ args) ++ "]"
    show (Builtin name _) = "<BUILTIN " ++ name ++ ">"
    show (Function _ args block) = "FUNCTION[" ++ (intercalate ", " args) ++ "] " ++ show block
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

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> p input >>= \(x,input') -> runParser (f x) input'

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    f <*> a = f >>= \g -> a >>= pure . g

instance Functor Parser where
    fmap f a = a >>= pure . f

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser a) <|> (Parser b) = Parser $ \input -> maybe (b input) pure (a input)

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
sepBy sep p = (p >>= \x -> (many $ sep >> many ws >> p) >>= pure . (x:)) <|> pure []

parseInt :: Parser Expr
parseInt = some digit >>= pure . int . read

parseBool :: Parser Expr
parseBool = (string "FALSE" >> (pure $ bool False)) <|> (string "TRUE" >> (pure $ bool True))

parseText :: Parser Expr
parseText = (char '\"' *> spanP (/='\"') <* char '\"') >>= pure . text

keywords :: [String]
keywords = ["DEFINE", "FUNCTION", "CALL", "IF", "ELSE", "WHILE"]

verifyVar :: Parser Expr -> Parser Expr
verifyVar p = p >>= \v@(VarExpr name) -> case elem name keywords of True -> empty
                                                                    False -> pure v

parseVar :: Parser Expr
parseVar = charF (\x -> isLetter x || x == '_') >>= \c -> spanP (\x -> isLetter x || isDigit x || x == '_') >>= pure . (c:) >>= verifyVar . pure . var

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
parseBinOp = (foldl (<|>) empty $ map (string . fst) ops) >>= \x -> case lookup x ops of Just e -> pure e
                                                                                         Nothing -> undefined

parseBinary :: Parser Expr
parseBinary = char '(' *> (parseExpr >>= \x -> some ws >> parseBinOp >>= \o -> some ws >> parseExpr >>= \y -> pure $ o x y) <* char ')'

parseNot :: Parser Expr
parseNot = char '(' *> (string "NOT" >> some ws >> parseExpr >>= \e -> pure $ NotExpr e) <* char ')'

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

parseIf :: Parser Stmt
parseIf = do
    string "IF"
    some ws
    cond <- parseExpr
    many ws
    char '{'
    many ws
    block <- sepBy ws parseStmt
    many ws
    char '}'
    pure $ ifS cond (Block block)

parseIfElse :: Parser Stmt
parseIfElse = do
    string "IF"
    some ws
    cond <- parseExpr
    many ws
    char '{'
    many ws
    blockTrue <- sepBy ws parseStmt
    many ws
    char '}'
    many ws
    string "ELSE"
    some ws
    char '{'
    many ws
    blockFalse <- sepBy ws parseStmt
    many ws
    char '}'
    pure $ ifElseS cond (Block blockTrue) (Block blockFalse)

parseWhile :: Parser Stmt
parseWhile = do
    string "WHILE"
    some ws
    cond <- parseExpr
    many ws
    char '{'
    many ws
    block <- sepBy ws parseStmt
    many ws
    char '}'
    pure $ while cond (Block block)

parseStmt :: Parser Stmt
parseStmt = parseAddVar <|> parseChange <|> parseEval <|> parseIfElse <|> parseIf <|> parseWhile

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
    pure $ func "" (map (\(VarExpr name) -> name) args) (Block block)

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
    case expr of Function _ args body -> pure $ defineFunc name args body
                 _ -> pure $ defineConst name expr

parseProgram :: Parser Program
parseProgram = sepBy ws parseDefinition

printF :: [Expr] -> ProgramState Expr
printF xs = liftIO (putStrLn $ intercalate " " $ map show xs) >> pure (int 0)

builtinFunctions :: [Expr]
builtinFunctions = [Builtin "print" printF]

main :: IO ()
main = do
    input <- getContents
    let parsed = runParser parseProgram input
    let prog = maybe (error "parse error") (\(x, _) -> if null x then error "parse error" else x) parsed 
    evalStateT (run prog builtinFunctions) []
