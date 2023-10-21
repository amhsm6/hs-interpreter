import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.IORef
import qualified Data.Map as M
import Data.List
import Data.Char

type Frame = IORef (M.Map String Expr)
type Bindings = [Frame]

type Action = StateT Bindings IO 

newFrame :: Action ()
newFrame = get >>= \b -> liftIO (newIORef M.empty) >>= \f -> put $ b ++ [f]

removeFrame :: Action ()
removeFrame = get >>= put . init

add :: String -> Expr -> Action ()
add name expr = do
    b <- get >>= \b -> when (null b) newFrame >> get

    let lastFrameRef = last b
    lastFrame <- liftIO $ readIORef lastFrameRef
    case M.lookup name lastFrame of
        Just  _ -> error $ "variable " ++ name ++ " already present"
        Nothing -> liftIO $ modifyIORef lastFrameRef $ M.insert name expr

change :: String -> Expr -> Action ()
change name expr = do
    b <- get
    liftIO $ do
        refs <- forM b $ \r -> readIORef r >>= pure . (M.lookup name >=> const (Just r))
        case foldl (<|>) Nothing refs of
            Just r -> modifyIORef r $ M.adjust (const expr) name
            Nothing -> error $ "variable " ++ name ++ " not found"

fetch :: String -> Action Expr
fetch name = do
    b <- get
    liftIO $ do
        exprs <- forM b $ \r -> readIORef r >>= pure . M.lookup name
        case foldl (<|>) Nothing exprs of
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
addVar = AddVarStmt . VarExpr

execute :: Stmt -> Action ()
execute (Block stmts) = do
    newFrame
    mapM_ execute stmts
    removeFrame
execute (AddVarStmt (VarExpr name) expr) = value expr >>= add name
execute (ChangeStmt cell expr) = value expr >>= mutate cell
execute (EvalStmt expr) = void $ value expr
execute (IfStmt cond block) = do
    x <- value cond
    case x of
        BoolExpr y -> when y $ execute block
        _ -> error "type error"
execute (IfElseStmt cond blockTrue blockFalse) = do
    x <- value cond
    case x of
        BoolExpr y -> execute $ if y then blockTrue else blockFalse
        _ -> error "type error"
execute while@(WhileStmt cond block) = do
    x <- value cond
    case x of
        BoolExpr y -> when y $ execute block >> execute while
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
defineFunc name args body = addVar name $ Function name args body

type Program = [Definition]

run :: Program -> [Expr] -> Action ()
run prog builtins = do
    mapM_ (\f@(Builtin name _) -> add name f) builtins
    mapM_ execute prog
    execute $ EvalStmt $ CallExpr (VarExpr "main") []

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
          | Builtin String ([Expr] -> Action Expr)
          | CallExpr Expr [Expr]
          | ArrayExpr [Expr]
          | IndexExpr Expr Expr
          | LengthExpr Expr

mutate :: Expr -> Expr -> Action ()
mutate (VarExpr name) new = change name new
mutate (DerefExpr expr) new = value expr >>= \p ->
    case p of
        Pointer b cell -> do
            b' <- get
            put b
            mutate cell new
            put b'
        _ -> error "type error"
mutate (IndexExpr i array) new = do
    i' <- value i
    let idx = case i' of
                    IntExpr x -> fromIntegral x
                    _ -> error "type error"

    array' <- value array
    let elems = case array' of
                    ArrayExpr elems -> elems
                    _ -> error "type error"

    if idx < 0 || idx >= length elems then error "index error" else pure ()

    let elems' = zipWith (\idx' e -> if idx == idx' then new else e) [0..] elems
    mutate array $ ArrayExpr elems'

value :: Expr -> Action Expr
value (LengthExpr array) = do
    array' <- value array
    case array' of
        ArrayExpr elems -> pure $ IntExpr $ fromIntegral $ length elems
        _ -> error "type error"
value (IndexExpr i array) = do
    i' <- value i
    let idx = case i' of
                    IntExpr x -> fromIntegral x
                    _ -> error "type error"

    array' <- value array
    let elems = case array' of
                    ArrayExpr elems -> elems
                    _ -> error "type error"

    if idx < 0 || idx >= length elems then error "index error" else pure ()

    pure $ elems !! idx
value a@(ArrayExpr _) = pure a
value (CallExpr expr args) = do
    values <- mapM value args

    f <- value expr
    case f of
        Function name argNames block -> do
            b <- get
            put [head b]
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
value (RefExpr x) = get >>= \b -> pure $ Pointer b x
value p@(Pointer _ _) = pure p
value (VarExpr name) = fetch name
value (IntExpr x) = pure $ IntExpr x
value (BoolExpr x) = pure $ BoolExpr x
value (TextExpr x) = pure $ TextExpr x
value (AddExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ IntExpr $ x + y
        _ -> error "type error"
value (SubExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ IntExpr $ x - y
        _ -> error "type error"
value (MulExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ IntExpr $ x * y
        _ -> error "type error"
value (DivExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ IntExpr $ if y == 0 then 0 else x `div` y
        _ -> error "type error"
value (ModExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ IntExpr $ if y == 0 then 1 else x `mod` y
        _ -> error "type error"
value (AndExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (BoolExpr x, BoolExpr y) -> pure $ BoolExpr $ x && y
        _ -> error "type error"
value (OrExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (BoolExpr x, BoolExpr y) -> pure $ BoolExpr $ x || y
        _ -> error "type error"
value (NotExpr a) = do
    a' <- value a
    case a' of
        BoolExpr x -> pure $ BoolExpr $ not x
        _ -> error "type error"
value (LtExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ BoolExpr $ x < y
        _ -> error "type error"
value (LeExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ BoolExpr $ x <= y
        _ -> error "type error"
value (EqExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ BoolExpr $ x == y
        _ -> error "type error"
value (GeExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ BoolExpr $ x >= y
        _ -> error "type error"
value (GtExpr l r) = do
    l' <- value l
    r' <- value r
    case (l', r') of
        (IntExpr x, IntExpr y) -> pure $ BoolExpr $ x > y
        _ -> error "type error"

instance Show Expr where
    show (LengthExpr array) = "LENGTH[" ++ show array ++ "]"
    show (IndexExpr index array) = "INDEX[" ++ show array ++ ", " ++ show index
    show (ArrayExpr elems) = "ARRAY[" ++ (intercalate ", " $ map show elems) ++ "]"
    show (CallExpr name args) = "CALL[" ++ show name ++ ", " ++ (intercalate ", " $ map show args) ++ "]"
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
    (Parser f) >>= g = Parser $ \input -> f input >>= \(x, input') -> runParser (g x) input'

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    m1 <*> m2 = m1 >>= \f -> m2 >>= pure . f

instance Functor Parser where
    fmap f m = m >>= pure . f

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser a) <|> (Parser b) = Parser $ \input -> maybe (b input) pure (a input)

instance MonadFail Parser where
    fail _ = Parser $ \_ -> Nothing

charP :: (Char -> Bool) -> Parser Char
charP pred = Parser f
    where f "" = Nothing
          f (x:xs)
              | pred x = Just (x, xs)
              | otherwise = Nothing

char :: Char -> Parser Char
char = charP . (==)

ws :: Parser Char
ws = charP isSpace

digit :: Parser Char
digit = charP isDigit

string :: String -> Parser String
string = mapM char

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just $ span f input

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = (p >>= \x -> (many $ sep >> many ws >> p) >>= pure . (x:)) <|> pure []

parseInt :: Parser Expr
parseInt = some digit >>= pure . IntExpr . read

parseBool :: Parser Expr
parseBool = (string "FALSE" >> (pure $ BoolExpr False)) <|> (string "TRUE" >> (pure $ BoolExpr True))

parseText :: Parser Expr
parseText = (char '\"' *> spanP (/='\"') <* char '\"') >>= pure . TextExpr

keywords :: [String]
keywords = ["DEFINE", "FUNCTION", "CALL", "IF", "ELSE", "WHILE", "ARRAY", "INDEX", "LENGTH"]

verifyVar :: Parser Expr -> Parser Expr
verifyVar p = p >>= \v@(VarExpr name) -> case elem name keywords of True -> empty
                                                                    False -> pure v

parseVar :: Parser Expr
parseVar = charP (\x -> isLetter x || x == '_') >>= \c -> spanP (\x -> isLetter x || isDigit x || x == '_') >>= pure . (c:) >>= verifyVar . pure . VarExpr

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
parseBinOp = foldl (<|>) empty (map (string . fst) ops) >>= \x -> maybe undefined pure $ lookup x ops

parseBinary :: Parser Expr
parseBinary = do
    char '('
    x <- parseExpr
    many ws
    o <- parseBinOp
    many ws
    y <- parseExpr
    char ')'
    pure $ o x y

parseNot :: Parser Expr
parseNot = char '(' *> (string "NOT" >> some ws >> parseExpr >>= \e -> pure $ NotExpr e) <* char ')'

parseRef :: Parser Expr
parseRef = char '&' >> parseExpr >>= pure . RefExpr

parseDeref :: Parser Expr
parseDeref = char '*' >> parseVar >>= pure . DerefExpr

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
    cell <- parseVar <|> parseDeref <|> parseIndex
    many ws
    char '='
    many ws
    expr <- parseExpr
    pure $ ChangeStmt cell expr

parseEval :: Parser Stmt
parseEval = EvalStmt <$> parseExpr

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
    pure $ IfStmt cond (Block block)

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
    pure $ IfElseStmt cond (Block blockTrue) (Block blockFalse)

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
    pure $ WhileStmt cond (Block block)

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
    pure $ Function "" (map (\(VarExpr name) -> name) args) (Block block)

parseCall :: Parser Expr
parseCall = do
    string "CALL["
    f <- parseVar
    many $ char ','
    many ws
    args <- sepBy (char ',') parseExpr
    char ']'
    pure $ CallExpr f args

parseArray :: Parser Expr
parseArray = do
    string "ARRAY["
    elems <- sepBy (char ',') parseExpr
    char ']'
    pure $ ArrayExpr elems

parseIndex :: Parser Expr
parseIndex = do
    string "INDEX["
    array <- parseExpr
    char ','
    many ws
    idx <- parseExpr
    char ']'
    pure $ IndexExpr idx array

parseLength :: Parser Expr
parseLength = do
    string "LENGTH["
    array <- parseExpr
    char ']'
    pure $ LengthExpr array

parseExpr :: Parser Expr
parseExpr = parseInt <|> 
            parseBool <|>
            parseText <|>
            parseBinary <|>
            parseUnary <|>
            parseVar <|>
            parseFunction <|>
            parseCall <|>
            parseArray <|>
            parseIndex <|>
            parseLength

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

printF :: [Expr] -> Action Expr
printF xs = liftIO (putStrLn $ intercalate " " $ map show xs) >> pure (IntExpr 0)

builtinFunctions :: [Expr]
builtinFunctions = [Builtin "print" printF]

main :: IO ()
main = do
    input <- getContents
    let parsed = runParser parseProgram input
        prog = maybe (error "parse error") (\(x, _) -> if null x then error "parse error" else x) parsed 
    evalStateT (run prog builtinFunctions) []
