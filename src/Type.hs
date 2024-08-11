module Type
  ( typecheck
  ) where
import Term
import Control.Monad.Trans.State (StateT (StateT), runStateT, get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Maybe (isJust)
import Control.Monad (forM_, foldM)
import Debug.Trace (trace)

-- referencing https://okmij.org/ftp/ML/generalization.html

data Context = Context
  { vars :: [(String, Type)]
  , instTypeBuffer :: [(String, Int)]
  , subst :: [(Int, Maybe Type)]
  , nextVar :: Int
  , level :: Level }
  deriving Show

type CheckM a = StateT Context (Either String) a

traceContext :: String -> CheckM ()
traceContext s =
  do
    g <- get
    trace ("\n" ++ s ++ ": context is " ++ show g ++ "\n") $ return ()

local :: CheckM a -> CheckM a
local f =
  do
    s <- get
    a <- f
    s' <- get
    put s'{ vars = vars s, instTypeBuffer = instTypeBuffer s }
    return a

throw :: String -> CheckM a
throw e =
  do
    g <- get
    lift $ Left $ e ++ " (in context " ++ show g ++ ")"

modError :: (String -> String) -> CheckM a -> CheckM a
modError f m = StateT $ \s ->
  case runStateT m s of
    Left e -> Left (f e)
    Right v -> Right v

getTypeVar :: Type -> CheckM Type
getTypeVar (TVar _ i) =
  do
    g <- get
    case lookup i (subst g) of
      Just (Just t) -> return t
      _ -> throw $ "type variable $" ++ show i ++ " was never set"
getTypeVar _ = error "getTypeVar only works on type variables"

setTypeVar :: Type -> Type -> CheckM ()
setTypeVar (TVar _ i) t =
  do
    g <- get
    let (xs, ys) = break (\(j, _) -> i == j) $ subst g
    case ys of
      [] -> throw $ "could not find type variable $" ++ show i ++ " to set"
      (_, Nothing) : ys' -> put g{ subst = xs ++ (i, Just t) : ys' }
      (_, Just _) : _ ->
        throw $ "type variable " ++ show i ++ " was already set"
setTypeVar _ _ = error "setTypeVar only works on type variables"

hasTypeVar :: Type -> CheckM Bool
hasTypeVar (TVar _ i) =
  do
    g <- get
    case lookup i (subst g) of
      Just (Just _) -> return True
      _ -> return False
hasTypeVar _ = error "hasTypeVar only works on type variables"

nextLevel :: CheckM ()
nextLevel =
  do
    g <- get
    put g{ level = incrLevel $ level g }

prevLevel :: CheckM ()
prevLevel =
  do
    g <- get
    put g{ level = decrLevel $ level g }

getLevel :: CheckM Level
getLevel = level <$> get

putVar :: String -> Type -> CheckM ()
putVar x t =
  do
    g <- get
    put g{ vars = (x, t) : vars g }

hasVar :: String -> CheckM Bool
hasVar x = isJust . lookup x . vars <$> get

getVar :: String -> CheckM Type
getVar x =
  do
    g <- get
    case lookup x $ vars g of
      Just t -> return t
      Nothing -> throw $ "could not find var " ++ x

setVar :: String -> Type -> CheckM ()
setVar x t =
  do
    g <- get
    let (xs, ys) = break (\(y, _) -> x == y) $ vars g
    case ys of
      [] -> throw $ "could not find var " ++ x ++ " to substitute"
      _ : ys' -> put g{ vars = xs ++ (x, t) : ys' }

popVar :: CheckM (String, Type)
popVar =
  do
    g <- get
    case vars g of
      [] -> throw "tried to pop empty variable context"
      x : xs -> do
        put g{ vars = xs }
        return x

instType' :: Type -> CheckM Type
instType' t@(TVar _ _) =
  do
    b <- hasTypeVar t
    if b then do
      t' <- getTypeVar t
      instType' t'
    else return t
instType' (TParam p) =
  do
    g <- get
    case lookup p $ instTypeBuffer g of
      Just i -> return $ TVar (level g) i
      Nothing -> do
        t <- newTypeVar
        case t of
          TVar _ i -> put g{ instTypeBuffer = (p, i) : instTypeBuffer g }
          _ -> throw "newTypeVar always returns TVar"
        return t
instType' (TArrow a r) =
  do
    a' <- instType' a
    r' <- instType' r
    return $ TArrow a' r'
instType' (TApp f a) =
  do
    f' <- instType' f
    a' <- instType' a
    return $ TApp f' a'
instType' t = return t

instType :: Type -> CheckM Type
instType = local . instType'

newTypeVar :: CheckM Type
newTypeVar =
  do
    g <- get
    let v = nextVar g
    put g
      { subst = (v, Nothing) : subst g
      , nextVar = v + 1 }
    return $ TVar (level g) v

occurs :: (Level, Int) -> Type -> CheckM Type
occurs (_, i) t@(TVar _ i') | i == i' =
  throw $ "occurs check failed with " ++ show t ++ " and " ++ show t
occurs v@(l, _) t@(TVar l' i) =
  do
    b <- hasTypeVar t
    if b then do
      t' <- getTypeVar t
      occurs v t'
    else return $ TVar (minLevel l l') i
occurs v (TArrow a r) =
  do
    a' <- occurs v a
    r' <- occurs v r
    return $ TArrow a' r'
occurs v (TApp f a) =
  do
    f' <- occurs v f
    a' <- occurs v a
    return $ TApp f' a'
occurs _ t = return t

unify :: Type -> Type -> CheckM Type
unify t t' | t == t' = return t
unify v@(TVar l i) v'@(TVar _ _) =
  do
    b <- hasTypeVar v
    if b then do
      t <- getTypeVar v
      unify t v'
    else do
      b' <- hasTypeVar v'
      if b' then do
        t' <- getTypeVar v'
        unify v t'
      else do
        t <- occurs (l, i) v'
        setTypeVar v t
        return t
unify v@(TVar l i) t =
  do
    b <- hasTypeVar v
    if b then do
      t' <- getTypeVar v
      unify t' t
    else do
      t' <- occurs (l, i) t
      setTypeVar v t'
      return t'
unify t v@(TVar l i) =
  do
    b <- hasTypeVar v
    if b then do
      t' <- getTypeVar v
      unify t t'
    else do
      t' <- occurs (l, i) t
      setTypeVar v t'
      return t'
unify (TArrow a r) (TArrow a' r') =
  do
    a'' <- unify a a'
    r'' <- unify r r'
    return $ TArrow a'' r''
unify (TApp f a) (TApp f' a') =
  do
    f'' <- unify f f'
    a'' <- unify a a'
    return $ TApp f'' a''
unify t t' = throw $ "cant unify " ++ show t ++ " and " ++ show t'

generalise :: Type -> CheckM Type
generalise t@(TVar l i) =
  do
    b <- hasTypeVar t
    if b then do
      t' <- getTypeVar t
      generalise t'
    else do
      l' <- getLevel
      if l > l' then
        return $ TParam $ "a" ++ show i
      else return t
generalise (TArrow a r) =
  do
    a' <- generalise a
    r' <- generalise r
    return $ TArrow a' r'
generalise (TApp f a) =
  do
    f' <- generalise f
    a' <- generalise a
    return $ TApp f' a'
generalise t = return t

check :: Term -> CheckM Type
check (EVar x) =
  do
    traceContext "var"
    t <- getVar x
    t' <- instType t
    trace ("resulting in " ++ show t' ++ "\n") $ return t'
check (EInt _) =
  return $ TCons "Int"
check (ELam x (Just t) e) =
  do
    t' <- local $ do
      putVar x t
      check e
    return $ TArrow t t'
check (ELam x Nothing e) =
  do
    local $ do
      t <- newTypeVar
      putVar x t
      t' <- check e
      return $ TArrow t t'
check (EApp f a) =
  do
    t0 <- check f
    t1 <- check a
    t' <- newTypeVar
    traceContext "app"
    t'' <- trace (show t0 ++ " vs " ++ show (TArrow t1 t') ++ "\n") $ unify t0 (TArrow t1 t')
    traceContext "app but after unification"
    case t'' of
      TArrow _ t -> return t
      _ -> error "impossible to not be TArrow _ _ at this point"
check (ELet x e e') =
  do
    nextLevel
    t' <- check e
    prevLevel
    t <- generalise t'
    local $ do
      putVar x t
      check e'
check (EAnn e t) =
  do
    t' <- check e
    unify t t'

checkTop :: [Top] -> CheckM ()
checkTop [] = return ()
checkTop (TAssign f args e : xs) =
  modError (("in "++f++": ")++) (do
    nextLevel
    forM_ args $ \arg -> do
      t <- newTypeVar
      putVar arg t
    t_ <- check e
    t <- foldM (\b _ -> do
      (_, t') <- popVar
      return $ TArrow t' b)
      t_ args
    prevLevel
    t' <- trace ("now we got: " ++ show t) $ getVar f
    t'' <- unify t t'
    setVar f t'') >> checkTop xs
checkTop (TTyping f t : xs) =
  do
    t' <- getVar f
    t'' <- unify t t'
    setVar f t''
    checkTop xs

generaliseTop :: [(String, Type)] -> CheckM [(String, Type)]
generaliseTop [] = return []
generaliseTop ((f, t) : xs) =
  do
    t' <- generalise t
    xs' <- generaliseTop xs
    return $ (f, t') : xs'

-- get the list of defined values
extractVars :: [Top] -> CheckM ()
extractVars [] = return ()
extractVars (TTyping _ _ : xs) = extractVars xs
extractVars (TAssign f _ _ : xs) =
  do
    b <- hasVar f
    if b then return () else do
      t <- newTypeVar
      putVar f t
    extractVars xs

typecheck :: [Top] -> Either String [(String, Type)]
typecheck ts =
  do
    (v, _) <- runStateT (do
      nextLevel
      extractVars ts
      checkTop ts
      prevLevel
      g <- get
      let ts' = vars g
      put g{ vars = [] }
      generaliseTop ts') Context
      { vars = []
      , instTypeBuffer = []
      , subst = []
      , nextVar = 0
      , level = initLevel }
    return $ reverse v
