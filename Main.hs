import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
-- Avaliação de variáveis e literais (null, strings, inteiros)
evalExpr env NullLit = return Nil
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (StringLit str) = return (String str)
evalExpr env (IntLit int) = return $ Int int

-- Avaliação de listas
evalExpr env (ArrayLit list) =
    case list of
        [] -> return (List [])
        (x:xs) -> do
            hd <- evalExpr env x
            (List tl) <- evalExpr env (ArrayLit xs)
            return (List (hd:tl))

-- Avaliação de números negativos
evalExpr env (PrefixExpr PrefixMinus expr) = do
    exprEval <- evalExpr env expr
    case exprEval of
        (Int int) -> return $ Int (-int)
        _ -> return $ Error "Invalid use of prefix minus."

-- Avaliação de operações infixas
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2

-- Avaliação de atribuições
-- Se stateLookup retornar um erro, é por que tentamos atribuir a uma variável não declarada
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    case v of
        (Error _) -> do -- Variável não declarada, vamos criá-la agora:
            varDecl env (VarDecl (Id var) Nothing)
            exprEval <- evalExpr env expr
            setVar var exprEval
        _ -> do -- Com a variável criada setamos o novo valor.
            e <- evalExpr env expr
            setVar var e

-- Avaliação de incremento/decremento (++i/i++, --i/i--)
evalExpr env (UnaryAssignExpr inc (LVar var)) =
    case inc of
        PrefixInc -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
        PrefixDec -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))
        PostfixInc -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
        PostfixDec -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))

-- Acesso em listas
evalExpr env (BracketRef expr1 expr2) = do
    evaluedExpr1 <- evalExpr env expr1
    evaluedExpr2 <- evalExpr env expr2
    evalElementAt env evaluedExpr1 evaluedExpr2

-- Avaliação de funções
evalExpr env (CallExpr nameExp args) = do
    res <- evalExpr env nameExp
    case res of
        (Error _) -> error ("Function not declared: " ++ show nameExp)
        -- Pré-definições de funções.
        (Function (Id "head") _ _) -> do -- head
            list <- evalExpr env (head args)
            case list of
                (List []) -> error "Empty list."
                (List (x:xs)) -> return x
        (Function (Id "tail") _ _) -> do -- tail
            list <- evalExpr env (head args)
            case list of
                (List []) -> error "Empty list."
                (List (x:xs)) -> return (List xs)
        (Function (Id "concat") _ _) -> do -- concat
            case args of
                [] -> return (List [])
                (x:xs) -> do
                    (List list) <- evalExpr env x
                    (List fim) <- evalExpr env (CallExpr nameExp xs)
                    return (List (list++fim))
        -- Executando a função
        (Function name argsName stmts) -> ST $ \s ->
            let (ST f) = mapM (evalExpr env) args
                (ST t) = aux1 env (BlockStmt stmts)
                (_, justLocal) = t s
                (ST x) = aux justLocal (BlockStmt stmts)
                (_, automaticGlobal) = x s
                (params, _) = f s
                parameters = fromList (zip (Prelude.map (\(Id a) -> a) argsName) params)
                local = union parameters s
                (ST g) = evalStmt env (BlockStmt stmts)
                (val, finalState) = g local
            in
            case val of
                -- Interseção entre o estado final da execução da função (excluindo os parametros) e estado global da memória, removendo variáveis locais e atualizando valores
                -- unida a interseção entre o estado global da memória e o estado final (caso alguma variável de uma função tenha sido passada como parametro [msm nome]
                -- e ainda vai ser utilizada ao voltar)
                (Return ret) -> (ret, intersection (difference finalState parameters) automaticGlobal `union` intersection automaticGlobal finalState)
                _ -> (val, intersection (difference finalState parameters) automaticGlobal `union` intersection automaticGlobal finalState)


-- Leitura de bloco de código de função
aux :: StateT -> Statement -> StateTransformer Value
aux env (BlockStmt []) = return Nil
aux env (BlockStmt (ExprStmt (AssignExpr OpAssign (LVar var) expr):xs)) = do
    v <- stateLookup env var
    case v of
    -- Variável não declarada, vamos criá-la como global agora.
        (Error _) -> do
            evalStmt env (VarDeclStmt [VarDecl (Id var) Nothing])
            aux env (BlockStmt xs)
        _ -> aux env (BlockStmt xs)
aux env (BlockStmt (x:xs)) =
    case x of
        (IfStmt expr ifBlock elseBlock) -> do
            aux env ifBlock
            aux env elseBlock
            aux env (BlockStmt xs)
        (IfSingleStmt expr ifBlock) -> do
            aux env ifBlock
            aux env (BlockStmt xs)
        (ForStmt initialize expr1 expr2 stmt) -> do
            case initialize of
                (ExprInit e) -> do
                    aux env (BlockStmt [ExprStmt e])
                    aux env stmt
                    aux env (BlockStmt xs)
                _ -> do
                    aux env stmt
                    aux env (BlockStmt xs)
        (ExprStmt (CallExpr nameExp args)) -> do
            res <- evalExpr env nameExp
            case res of
                (Error _) -> aux env (BlockStmt xs)
                (Function name argsName stmts) -> do
                    aux env (BlockStmt stmts)
                    aux env (BlockStmt xs)
        _ -> aux env (BlockStmt xs)

-- Leitura local do corpo da função
aux1 :: StateT -> Statement -> StateTransformer Value
aux1 env (BlockStmt []) = return Nil
aux1 env (VarDeclStmt []) = return Nil
aux1 env (VarDeclStmt (decl:ds)) = do
    varDecl env decl
    aux1 env (VarDeclStmt ds)
aux1 env (BlockStmt (x:xs)) =
    case x of
        (IfStmt expr ifBlock elseBlock) -> do
            aux1 env ifBlock
            aux1 env elseBlock
            aux1 env (BlockStmt xs)
        (IfSingleStmt expr ifBlock) -> do
            aux1 env ifBlock
            aux1 env (BlockStmt xs)
        (ForStmt initialize expr1 expr2 stmt) -> do
            aux1 env stmt
            aux1 env (BlockStmt xs)
        (VarDeclStmt (y:ys)) -> do
            varDecl env y
            aux1 env (BlockStmt xs)
        (ExprStmt (CallExpr nameExp args)) -> do
            res <- evalExpr env nameExp
            case res of
                (Error _) -> aux1 env (BlockStmt xs)
                (Function name argsName stmts) -> do
                    aux1 env (BlockStmt stmts)
                    aux1 env (BlockStmt xs)
        _ -> aux1 env (BlockStmt xs)

listVarDecl :: [Id] -> [Expression] -> [VarDecl]
listVarDecl (x:xs) (y:ys) = VarDecl x (Just y):listVarDecl xs ys
listVarDecl [] [] = []

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr

-- Avaliação de if-single
evalStmt env (IfSingleStmt expr ifBlock) = do
    condition <- evalExpr env expr
    case condition of
        (Bool cond) -> if cond then
            evalStmt env ifBlock
        else
            return Nil
        error@(Error _) -> return error

-- Avaliação de if-else
evalStmt env (IfStmt expr ifBlock elseBlock) = do
    condition <- evalExpr env expr
    case condition of
        (Bool cond) -> if cond then
            evalStmt env ifBlock
         else
            evalStmt env elseBlock
        (Error _) -> return $ Error "Condition error in if statement"

-- Avaliação de bloco de expressões
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt ((BreakStmt Nothing):xs)) = return Break
evalStmt env (BlockStmt (x:xs)) = do
    ret <- evalStmt env x
    case ret of
        (Return val) -> return (Return val)
        Break -> return Break
        _ -> evalStmt env (BlockStmt xs)

-- Avaliação de loops
evalStmt env (ForStmt initialize expr1 expr2 stmt) =
    let stmtIni = case initialize of
            NoInit -> EmptyStmt
            (VarInit listVarDecl) -> VarDeclStmt listVarDecl
            (ExprInit expr) -> ExprStmt expr
        in do
        evalStmt env stmtIni
        let e1 = case expr1 of
                (Just expr) -> expr
                Nothing -> NullLit
            e2 = case expr2 of
                (Just expr) -> expr
                Nothing -> NullLit
            in do
            condition <- evalExpr env e1
            case condition of
                (Bool cond) -> if cond then do
                    ret <- evalStmt env stmt
                    case ret of
                        Break -> return Nil
                        (Return val) -> return (Return val)
                        _ -> do
                            evalExpr env e2
                            evalStmt env (ForStmt NoInit (Just e1) (Just e2) stmt)
                        else return Nil
                Nil -> do
                    ret <- evalStmt env stmt
                    case ret of
                        (Return val) -> return (Return val)
                        Break -> return Nil
                        _ -> do
                            evalExpr env e2
                            evalStmt env (ForStmt NoInit (Just e1) (Just e2) stmt)
                error@(Error _) -> return error

-- While
evalStmt env (WhileStmt expr stmt) = do
    evaluedExpr <- evalExpr env expr
    case evaluedExpr of
        Bool True -> do
            evaluedStmt <- evalStmt env stmt
            case evaluedStmt of
                Break -> return Nil
                Return r -> return (Return r)
                _ -> evalStmt env (WhileStmt expr stmt)
        Bool False -> return Nil

-- Avaliação do return
evalStmt env (ReturnStmt expression) =
    case expression of
        Nothing -> return (Return Nil)
        (Just expr) -> do
            exprEval <- evalExpr env expr
            return (Return exprEval)

-- Avaliação de declarações de função
evalStmt env (FunctionStmt name args body) = funcDecl env (name, args, body)


-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env [stmt] = evalStmt env stmt
evaluate env (s:ss) = evalStmt env s >> evaluate env ss

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

-- Comparação de listas
infixOp env OpEq   (List []) (List []) = return $ Bool True
infixOp env OpEq   (List []) (List _) = return $ Bool False
infixOp env OpEq   (List _) (List []) = return $ Bool False
infixOp env OpEq   (List v1) (List v2) = do
    b1 <- infixOp env OpEq (head v1) (head v2)
    b2 <- infixOp env OpEq (List (tail v1)) (List (tail v2))
    infixOp env OpLAnd b1 b2
infixOp env OpNEq  (List list1) (List list2) = do
    (Bool notAns) <- infixOp env OpEq  (List list1) (List list2)
    return $ Bool $ not notAns

-- Operações com variáveis
infixOp env op (Var x) v2 = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op val v2
infixOp env op v1 (Var x) = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op v1 val

--
-- Environment and auxiliary functions
--

-- Iniciando o environment com as funções concat, head e tail pré definidas
environment :: Map String Value
environment =
    insert "concat" (Function (Id "concat") [Id "list1", Id "list2"] []) $ insert "tail" (Function (Id "tail") [Id "list"] []) $ insert "head" (Function (Id "head") [Id "list"] []) empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    (maybe (Error $ "Variable " ++ show var ++ " not defined") id (Map.lookup var (union s env)), s)

evalElementAt :: StateT -> Value -> Value -> StateTransformer Value
evalElementAt env (List []) (Int n) = return Nil
evalElementAt env (List (l:ls)) (Int 0) = return l
evalElementAt env (List (l:ls)) (Int n) = do
    evalElementAt env (List ls) (Int (n-1))

-- Auxiliares de declaração de função (funcDecl e setFunc)
funcDecl :: StateT -> (Id, [Id], [Statement]) -> StateTransformer Value
funcDecl env (Id id, args, body) = setFunc id (Function (Id id) args body)

setFunc :: String -> Value -> StateTransformer Value
setFunc name description = ST $ \s -> (description, insert name description s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) =
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "JS/Fatorial.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
