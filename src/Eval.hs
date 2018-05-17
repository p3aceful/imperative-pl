module Eval where

import Syntax
import Primitive
import Pretty
import Debug.Trace (trace)

import Control.Monad
import Data.IORef

data Tree a = Empty | Node a [Tree a]
    deriving Show
type Thread = (Int, (Ast, Env, [Ctx]))

addVar :: String -> Value -> Env -> Env
addVar s v env = (s, v):env

addVars :: [String] -> [Value] -> Env -> Env
addVars ss vs env = zip ss vs ++ env

findVar :: String -> Env -> Value
findVar s env =
  let (Just v) = lookup s env in v -- assumes that a variable is always found

findInTree :: Int -> Tree Int -> Bool
findInTree id (Node uid []) = id == uid
findInTree id (Node uid ls) = (id == uid) || any (findInTree id) ls

removeChildThreads :: [Thread] -> Tree Int -> [Thread]
removeChildThreads [] tree = []
removeChildThreads ((id, cfg):ts) tree = 
    if findInTree id tree then removeChildThreads ts tree else (id, cfg) : removeChildThreads ts tree

getSubTree :: Int -> Tree Int -> Tree Int
getSubTree id tree = head $ getSubTree' id tree

getSubTree' :: Int -> Tree Int -> [Tree Int]
getSubTree' id (Node id' [])  = [Node id' [] | id == id']
getSubTree' id (Node id' ts) = if id == id' then [Node id' ts] else concatMap (getSubTree' id) ts

cutBranch :: Int -> Tree Int -> Tree Int
cutBranch id (Node i ls) = Node i (map (cutBranch id) ls')
    where
        cutBranch' (Node id' leafs) = if id == id' then [] else [Node id' leafs]
        ls' = concatMap cutBranch' ls

addNode :: Int -> Int -> Tree Int -> Tree Int
addNode id nid (Node id' ls) | id == id' = Node id' (Node nid [] : ls)
addNode id nid (Node id' ls) = Node id' (map (addNode id nid) ls)

getParent :: Int -> Tree Int -> Int
getParent id tree = head $ getParent' id tree

getParent' id (Node tid []) = []
getParent' id (Node tid ls) = if childHasId then [tid] else concatMap (getParent' id) ls
    where
        childHasId = any ((== id) . (\ (Node i _) -> i)) ls

insertAtRoot :: Tree Int -> Tree Int -> Tree Int
insertAtRoot tree (Node _ ls) = Node 1 (tree:ls)

findThread :: Int -> [Thread] -> (Thread, [Thread])
findThread id [] = error ("Couldnt identify tid " ++ show id)
findThread id ((id', cfg):ts) = if id == id' then ((id', cfg), ts) else findThread id ts

exec :: Ast -> IO ()
exec e = steps [(1, (e, primitives, []))] 2 (Node 1 [])

steps :: [(Int, (Ast, Env, [Ctx]))] -> Int -> Tree Int -> IO ()
-- steps ((id, (ast, env, ctx)):ts) uid tree | trace ("\n\n\nBegin: " ++ show id ++ "\n\nAST:\n" ++ show ast ++ " \n\nCTX:\n" ++ show ctx) False = undefined
-- steps ((id, (ast, env, ctx)):ts) uid tree | trace (if id == 1 then "\n\n\nBegin: " ++ show id ++ "\n\nAST:\n" ++ show ast ++ " \n\nCTX:\n" ++ show ctx else []) False = undefined
steps [(id, (SSkip, _, []))] uid tree = return ()

-- Remove children threads from queue and tree
steps ((id, (SSkip, _, [])) : ts) uid tree = 
    if null ts' then return () else steps ts' uid tree'
    where
        subTree = getSubTree id tree
        ts' = removeChildThreads ts subTree
        tree' = cutBranch id tree

steps ((id, (ESpawn f, env, ctx)) : ts) uid tree | isValue f = steps ts' (uid + 1) (addNode id uid tree)
    where
        t' = (EVal (VInt uid), env, ctx)
        ts' = ts ++ [(uid, (SExpr (ECall f [] []), env, []))] ++ [(id, t')]
    

steps ((id, (EDetach f, env, ctx)):ts) uid tree | isValue f = steps ts' uid tree'
    where
        (VInt f') = expr2val f
        t' = (EVal VVoid, env, ctx)
        ts' = ts ++ [(id, t')]
        tree' = insertAtRoot (getSubTree f' tree) (cutBranch f' tree)

steps ((id, (EJoin v, env, ctx)) : ts) uid tree | isValue v = force thrd >>= \t -> steps (ts' ++ [(id, (EVal VVoid, env, ctx))] ++ [(tid, t)]) uid tree
    where
        (VInt v') = expr2val v
        ((tid, thrd), ts') = findThread v' ts

steps ((id, t) : ts) uid tree = rr 5 t >>= \t' -> steps (ts ++ [(id, t')]) uid tree


rr :: Int -> (Ast, Env, [Ctx]) -> IO (Ast, Env, [Ctx])
rr 0 st = return st
rr i (SSkip, e, []) = return (SSkip, e, [])
rr i (ESpawn e, env, ctx) | isValue e = return (ESpawn e, env, ctx)
rr i (EDetach e, env, ctx) | isValue e = return (EDetach e, env, ctx)
rr i (EJoin e, env, ctx) | isValue e = return (EJoin e, env, ctx)
rr i st = step st >>= rr (i-1)

force :: (Ast, Env, [Ctx]) -> IO (Ast, Env, [Ctx])
force (SSkip, env, []) = return (SSkip, env, [])
force st = step st >>= force

step :: (Ast, Env, [Ctx]) -> IO (Ast, Env, [Ctx])
-- step (ast, e, c) | trace ("Ast:\n" ++ show ast ++ "\n--\nEnv:\n" ++ show e ++ "\n--\nCtx:\n" ++ show c ++ "\n\n-----next-----") False = undefined
-- step (ast, e, c) | trace (show ast ++ "\n--\n" ++ show c ++ "\n") False = undefined

-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx) = return (e, env, SExpr Hole : ctx)
step (v, env, SExpr Hole : ctx) | isValue v = return (SSkip, env, ctx)

-- Blocks
step (SBlock s, env, ctx) = return (s, env, SBlock (HoleWithEnv env) : ctx)
step (SSkip, _, SBlock (HoleWithEnv env) : ctx) = return (SSkip, env, ctx) -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx) = return (s1, env, SSeq Hole s2 : ctx)
step (SSkip, env, SSeq Hole s2 : ctx) = return (s2, env, ctx)

-- If and while
step (SIf cond s1 s2, env, ctx) = return (cond, env, SIf Hole s1 s2 : ctx)
step (EVal (VBool True), env, SIf Hole s1 _ : ctx) = return (SBlock s1, env, ctx)
step (EVal (VBool False), env, SIf Hole _ s2 : ctx) = return (SBlock s2, env, ctx)

step (w@(SWhile cond s), env, ctx) = return (SIf cond (SSeq s w) SSkip, env, ctx)

step (SDoWhile cond s, env, ctx) = return (SSeq s (SIf cond (SSeq s (SWhile cond s)) SSkip), env, ctx)
-- Variable declaration
step (SVarDecl s e, env, ctx) = return (e, env, SVarDecl s Hole : ctx)
step (v, env, SVarDecl s Hole : ctx) | isValue v 
  = return (SSkip, addVar s (expr2val v) env, ctx)

-- Assignment
step (SAssign s e, env, ctx) = return (e, env, SAssign s Hole : ctx)
step (v, env, SAssign s Hole : ctx) | isValue v =
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return (SSkip, env, ctx)
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""
  

-- Variable reference: get from environment
step (EVar s, env, ctx) = return (EVal $ findVar s env, env, ctx)

-- Box a value
step (ERef e, env, ctx) = return (e, env, ERef Hole : ctx)
step (v, env, ERef Hole : ctx) | isValue v = do
  nv <- newIORef (expr2val v)
  return (EVal (VRef nv), env, ctx)

-- Dereference a ref
step (EDeref e, env, ctx) = return (e, env, EDeref Hole : ctx)
step (v, env, EDeref Hole : ctx) | isValue v = do
  let (VRef nv) = expr2val v
  v' <- readIORef nv
  return (EVal v', env, ctx)

-- Function becomes a closure
step (EFun pars body, env, ctx) = return (EVal $ VClosure pars body env, env, ctx)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx) =
  return (s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx)

-- Return statements
step (SReturn e, env, ctx) | notValue e = return (e, env, SReturn Hole : ctx)
step (v, env, SReturn Hole :ctx) | isValue v = return (SReturn v, env, ctx)
step (SReturn v, _, ECall (HoleWithEnv env) _ _ : ctx) | isValue v = return (v, env, ctx) 
step (SReturn v, env, c : ctx) | isValue v = return (SReturn v, env, ctx)

-- Try/Catch
step (STry s1 evar s2, env, ctx) = return (s1, env, STry Hole evar s2 : ctx)
step (SSkip, env, STry Hole evar s2 : ctx) = return (SSkip, env, ctx)

-- Throw stmt
step (SThrow e, env, STry Hole evar s2 :ctx) = return (s2, addVar evar (expr2val e) env, ctx)
step (SThrow e, env, c :ctx) = return (SThrow e, env, ctx) 

step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx) = return (EVal VVoid, env, ctx)
  -- function body fully evaluated, return VVoid

step (ECall (EVal (VPrimFun f)) [] vs, env, ctx) =
  return (EVal $ f (reverse vs), env, ctx)

step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx) = do
  res  <- f (reverse vs)
  return (EVal res, env, ctx)

step (ECall (EVal (VCont e c)) [] [arg], env, ctx) = 
    return (EVal arg, env, c ++ (ECall (HoleWithEnv env) [] [arg] : ctx))

step (ECall f [] _, _, _) | isValue f = error $ "a call to non-function " ++ show f
-- Reduce on function position
step (ECall f args [], env, ctx) | notValue f = return (f, env, ECall Hole args [] : ctx)
step (f, env, ECall Hole args [] : ctx) | isValue f = return (ECall f args [], env, ctx)
step (ECall f (a:args) vs, env, ctx) | isValue f = return (a, env, ECall f (Hole:args) vs : ctx)
step (v, env, ECall f (Hole:args) vs : ctx) | isValue v = return (ECall f args (expr2val v : vs), env, ctx)



step (EReset f, env, ctx) = return (f, env, EReset Hole : ctx)
step (EVal (VClosure p b e), env, EReset Hole : ctx) = 
    return (ECall (EVal (VClosure p b e)) [] [], env, EReset Hole : ctx)

step (EShift e, env, ctx) = return (e, env, EShift Hole : ctx)
step (EVal (VClosure p b e), env, EShift Hole : ctx) =
    return (ECall (EVal (VClosure p b e)) [EVal (VCont env ctx'')] [], env, ctx')
    where
        (ctx', ctx'') = continuation ctx
        continuation ctx = continuation' ctx []
        continuation' (ECall{} : EReset Hole : ctx) rest = (ctx, reverse rest)
        continuation' (c:ctx) rest = continuation' ctx (c:rest)


-- Spawn / Detach / Join
step (ESpawn f, env, ctx) | notValue f =  return (f, env, ESpawn Hole : ctx)
step (f, env, ESpawn Hole : ctx) | isValue f = return (ESpawn f, env, ctx)
        
step (EDetach e, env, ctx) | notValue e = return (e, env, EDetach Hole : ctx)
step (v, env, EDetach Hole : ctx) | isValue v = return (EDetach v, env, ctx)

step (EJoin e, env, ctx) | notValue e = return (e, env, EJoin Hole : ctx)
step (v, env, EJoin Hole : ctx) = return (EJoin v, env, ctx)
