module Term
  ( Kind (..)
  , Type (..)
  , Term (..)
  , Top (..)
  ) where

data Kind =
    KStar
  | KArrow Kind Kind
  deriving Eq

instance Show Kind where
  show KStar = "*"
  show (KArrow KStar r) = "* -> " ++ show r
  show (KArrow a r) = "(" ++ show a ++ ") -> " ++ show r

data Type =
    TCons String
  | TVar Int
  | TParam String
  | TApp Type Type
  | TArrow Type Type
  | TDeBruijn Int
  | TForall Type
  deriving Eq

instance Show Type where
  show (TCons c) = c
  show (TVar i) = "$" ++ show i
  show (TParam p) = "'" ++ p
  show (TApp c@(TArrow _ _) a@(TArrow _ _)) =
    "(" ++ show c ++ ") (" ++ show a ++ ")"
  show (TApp c@(TArrow _ _) a) =
    "(" ++ show c ++ ") " ++ show a
  show (TApp c a@(TArrow _ _)) =
    show c ++ " (" ++ show a ++ ")"
  show (TApp c a) = show c ++ " " ++ show a
  show (TArrow a@(TArrow _ _) r) = "(" ++ show a ++ ") -> " ++ show r
  show (TArrow a r) = show a ++ " -> " ++ show r
  show (TDeBruijn n) = "#" ++ show n
  show (TForall t@(TForall _)) = "∀" ++ show t
  show (TForall t) = "∀." ++ show t

data Term =
    EVar String
  | EInt Int
  | EAnn Term Type
  | ELam String (Maybe Type) Term
  | EApp Term Term
  | ELet String Term Term
  deriving Eq

data Top =
    TAssign String [String] Term
  | TTyping String Type

paren :: Int -> Int -> String -> String
paren current wanted pretty =
  if current > wanted then
    "(" ++ pretty ++ ")"
  else pretty

prettyTerm' :: Int -> Term -> String
prettyTerm' _ (EVar x) = x
prettyTerm' _ (EInt i) = show i
prettyTerm' l (EAnn e t) = paren l 0 $
  prettyTerm' 1 e ++ ": " ++ show t
prettyTerm' l (ELam x Nothing e) = paren l 2 $
  "λ" ++ x ++ "." ++ prettyTerm' 2 e
prettyTerm' l (ELam x (Just t) e) = paren l 2 $
  "λ(" ++ x ++ ": " ++ show t ++ "." ++ prettyTerm' 2 e
prettyTerm' l (EApp f a) = paren l 3 $
  prettyTerm' 3 f ++ " " ++ prettyTerm' 4 a
prettyTerm' _ (ELet x e e') =
  "let " ++ x ++ " = " ++ prettyTerm' 0 e ++ " in " ++ prettyTerm' 0 e'

instance Show Term where
  show = prettyTerm' 0

instance Show Top where
  show (TAssign x args e) =
    "def " ++ x ++ foldl (\b a -> b ++ " " ++ a) "" args ++ " = " ++ show e
  show (TTyping x t) = "val " ++ x ++ " : " ++ show t

-- toDeBruijn' :: [String] -> Term -> Term
-- toDeBruijn' ctx (TVar x) =
--   case elemIndex x ctx of
--     Just i -> TDeBruijn i
--     Nothing -> TVar x
-- toDeBruijn' ctx (TLam x t e) = TLam x t (toDeBruijn' (x:ctx) e)
-- toDeBruijn' ctx (TAnn e t) = TAnn (toDeBruijn' ctx e) (toDeBruijn' ctx t)
-- toDeBruijn' ctx (TPi (Just x) a r) =
--     TPi (Just x) (toDeBruijn' ctx a) (toDeBruijn' (x:ctx) r)
-- toDeBruijn' ctx (TPi Nothing a r) =
--   TPi Nothing (toDeBruijn' ctx a) (toDeBruijn' ctx r)
-- toDeBruijn' ctx (TApp f a) = TApp (toDeBruijn' ctx f) (toDeBruijn' ctx a)
-- toDeBruijn' _ t = t

-- toDeBruijn :: Term -> Term
-- toDeBruijn = toDeBruijn' []

-- fromDeBruijn :: [String] -> Term -> Term
-- fromDeBruijn ctx (TDeBruijn n) = TVar $ ctx !! n
-- fromDeBruijn ctx (TLam x t e) = TLam x t (fromDeBruijn (x:ctx) e)
-- fromDeBruijn ctx (TAnn e t) = TAnn (fromDeBruijn ctx e) (fromDeBruijn ctx t)
-- fromDeBruijn ctx (TPi (Just x) a r) =
--   TPi (Just x) (fromDeBruijn ctx a) (fromDeBruijn (x:ctx) r)
-- fromDeBruijn ctx (TPi Nothing a r) =
--   TPi Nothing (fromDeBruijn ctx a) (fromDeBruijn ctx r)
-- fromDeBruijn ctx (TApp f a) = TApp (fromDeBruijn ctx f) (fromDeBruijn ctx a)
-- fromDeBruijn _ t = t

-- remAssignArgs :: Top -> Top
-- remAssignArgs (TAssign f (x:xs) e) =
--   remAssignArgs (TAssign f xs (TLam x Nothing e))
-- remAssignArgs t = t
