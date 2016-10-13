import Data.Char

type Name = String
type Domain = [Integer]
type Valuation = [(Name, Integer)]

data Expr = Val Integer
  | Var Name
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Expr :%: Expr
  
par :: String -> String
par str = "(" ++ str ++ ")"

instance Show Expr where
  show (Val n)          = show n
  show (Var nm)         = nm
  show (p :+: q)        = par(show p ++ "+" ++ show q)
  show (p :-: q)        = par(show p ++ "-" ++ show q)
  show (p :*: q)        = par(show p ++ "*" ++ show q)
  show (p :/: q)        = par(show p ++ "/" ++ show q)
  show (p :%: q)        = par(show p ++ "%" ++ show q)

vars :: Expr -> [String]
vars (Val n)            = []
vars (Var nm)           = [nm]
vars (p :+: q)          = vars p ++ vars q
vars (p :-: q)          = vars p ++ vars q
vars (p :*: q)          = vars p ++ vars q
vars (p :/: q)          = vars p ++ vars q
vars (p :%: q)          = vars p ++ vars q

isComplVal :: Expr -> Valuation -> Bool
isComplVal (Val n) _    = True
isComplVal (Var nm) vs  = elem nm (map fst vs)
isComplVal (p :+: q) vs = (isComplVal p vs) && (isComplVal q vs)    
isComplVal (p :-: q) vs = (isComplVal p vs) && (isComplVal q vs)    
isComplVal (p :*: q) vs = (isComplVal p vs) && (isComplVal q vs)    
isComplVal (p :/: q) vs = (isComplVal p vs) && (isComplVal q vs)    
isComplVal (p :%: q) vs = (isComplVal p vs) && (isComplVal q vs)

evalExpr :: Expr -> Valuation -> Integer
evalExpr expr val 
  | not (isComplVal expr val) = error "Not a complete/fitting valuation for this expression"
  | otherwise           =  eval expr val

eval :: Expr -> Valuation -> Integer
eval (Val n) _      = n
eval (Var nm) vs    = sure (lookup nm vs) where sure (Just b) = b
eval (p :+: q) vs   = (eval p vs) + (eval q vs)
eval (p :-: q) vs   = (eval p vs) - (eval q vs)
eval (p :*: q) vs   = (eval p vs) * (eval q vs)
eval (p :/: q) vs   = (eval p vs) `div` (eval q vs)
eval (p :%: q) vs   = (eval p vs) `mod` (eval q vs)

valuations :: [(Name, Domain)] -> [Valuation]
valuations [] = []
valuations ((nm,dom):[]) = [[(nm,val)] | val <- dom]
valuations ((nm, dom) : nmdoms) = [(nm,val):vals| (nm,val) <- [(nm, val) | val <- dom], vals <- (valuations nmdoms)]

pytriples n = filter check (valuations [("a",[1..n]),("b",[1..n]),("c",[1..n])])
  where check valuation = ((asqrd valuation + bsqrd valuation) == csqrd valuation) && (a valuation <= b valuation)
        asqrd valuation = evalExpr (Var "a" :*: Var "a") valuation
        bsqrd valuation = evalExpr (Var "b" :*: Var "b") valuation
        csqrd valuation = evalExpr (Var "c" :*: Var "c") valuation
        a valuation = sure(lookup "a" valuation)
        b valuation = sure(lookup "b" valuation)
        sure (Just b) = b

isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp '%' = True
isOp '(' = True
isOp ')' = True
isOp _   = False

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:str)
  | c == ' '    = tokenize str
  | isAlpha c   = [c: (takeWhile (isAlphaNum) str)] ++ (tokenize (dropWhile (isAlphaNum) str))
  | isOp c      = [[c]] ++ tokenize str
  | isDigit c     = [c: (takeWhile (isDigit) str)] ++ (tokenize (dropWhile (isDigit) str))
  | otherwise   = error "Parse error"
  
toExpr :: String -> Expr
toExpr string = fst (parseE (tokenize string))
  
parseE :: [String] -> (Expr, [String])
parseE tokens = (accE', accTokens)
  where (accT, accToks) = parseT tokens
        (accE', accTokens) = parseE' accT accToks

parseE' :: Expr -> [String] -> (Expr, [String])
parseE' exp ("+":tokens) = ((exp :+: accE'), accE'tokens)
  where (accT, accToks) = parseT tokens
        (accE', accE'tokens) = parseE' accT accToks
parseE' exp ("-":tokens) = ((exp :-: accE'), accE'tokens)
  where (accT, accToks) = parseT tokens
        (accE', accE'tokens) = parseE' accT accToks
parseE' exp tokens = (exp, tokens)

parseT :: [String] -> (Expr, [String])
parseT tokens = (accT', accTokens)
  where (accF, accToks) = parseF tokens
        (accT', accTokens) = parseT' accF accToks

parseT' :: Expr -> [String] -> (Expr, [String])
parseT' exp ("*":tokens) = ((exp :*: accT'), accT'tokens)
  where (accF, accToks) = parseF tokens
        (accT', accT'tokens) = parseT' accF accToks
parseT' exp ("/":tokens) = ((exp :/: accT'), accT'tokens)
  where (accF, accToks) = parseF tokens
        (accT', accT'tokens) = parseT' accF accToks
parseT' exp ("%":tokens) = ((exp :%: accT'), accT'tokens)
  where (accF, accToks) = parseF tokens
        (accT', accT'tokens) = parseT' accF accToks        
parseT' exp tokens = (exp, tokens)

parseF :: [String] -> (Expr, [String])
parseF ("(":tokens) = ((accE), accTokens)
  where (accEs, accToks) = parseE tokens
        (accE, accTokens) = (accEs, (drop 1 accToks))
parseF (t:tokens)
  | isDigit (head t)  = (Val (read t), tokens)
  | isAlpha (head t)  = (Var t, tokens)
  | otherwise         = error "syntax error"
