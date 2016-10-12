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

--valuate :: (Name, Domain) -> Valuation
--valuate (nm, dom) = [(nm, val) | val <- dom]

--valuateAll :: [(Name, Domain)] -> [Valuation]
--valuateAll nmdoms = [valuate (nm, dom) | (nm, dom) <- nmdoms]

--valuationsHelper :: [Valuation] -> [Valuation]
--valuationsHelper ([] : valss) = (valuationsHelper valss)
--valuationsHelper ((val:vals) : valss) = [[val]] ++ (valuationsHelper (valss)) ++ valuationsHelper (vals : valss)

--valuations :: [(Name, Domain)] -> [Valuation]
--valuations ((nm, []) : vs) = valuations vs
--valuations ((nm, dom) : vs) = 

isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp '%' = True
isOp _   = False

isNum :: Char -> Bool
isNum c = ord c >= ord '0' && ord c <=  ord '9'

tokenize :: String -> [String]
tokenize [] = []
tokenize (c:str)
  | c == ' '    = tokenize str
  | isAlpha c   = [c: (takeWhile (isAlphaNum) str)] ++ (tokenize (dropWhile (isAlphaNum) str))
  | isOp c      = [[c]] ++ tokenize str
  | isNum c     = [c: (takeWhile (isNum) str)] ++ (tokenize (dropWhile (isNum) str))
  | otherwise   = error "Parse error"

