module Term
  ( Kind (..)
  , Level
  , initLevel
  , minLevel
  , incrLevel
  , decrLevel
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

newtype Level = Level Int
  deriving (Eq, Ord, Show)

initLevel :: Level
initLevel = Level 0

minLevel :: Level -> Level -> Level
minLevel (Level l) (Level l') | l < l' = Level l
minLevel _ l = l

incrLevel :: Level -> Level
incrLevel (Level l) = Level $ l + 1

decrLevel :: Level -> Level
decrLevel (Level l) = Level $ l - 1

data Type =
    TCons String
  | TVar Level Int
  | TParam String
  | TApp Type Type
  | TArrow Type Type

instance Eq Type where
  TCons c == TCons c' = c == c'
  TVar _ i == TVar _ i' = i == i'
  TParam p == TParam p' = p == p'
  TApp f a == TApp f' a' = f == f' && a == a'
  TArrow a r == TArrow a' r' = a == a' && r == r'
  _ == _ = False
  

instance Show Type where
  show (TCons c) = c
  show (TVar (Level l) i) = "$" ++ show i ++ "%" ++ show l
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

data Term =
    EVar String
  | EInt Int
  | ELam String (Maybe Type) Term
  | EApp Term Term
  | ELet String Term Term
  | EAnn Term Type
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
  "λ(" ++ x ++ ": " ++ show t ++ ")." ++ prettyTerm' 2 e
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
