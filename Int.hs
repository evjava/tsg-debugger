{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Int where

import Data.List

data Exp = ATOM Atom
         | PVA  VName
         | PVE  VName
         | CVA  Int
         | CVE  Int
         | CONS Exp Exp deriving (Eq, Show)

type Atom  = String
type AVar  = Exp
type EVar  = Exp
type Parm  = Exp
type EVal  = Exp
type AVal  = Exp 
type Var   = Exp
type AExp  = Exp

type Prog  = [FDef]

type FName = String
type VName = String

data FDef  = DEFINE FName [Parm] Term deriving (Eq, Show)

type State = (Term, Env)

data Bind  = Var := Exp deriving Show
type Env   = [Bind]

data Term = ALT Cond Term Term
          | CALL FName [Exp]
          | RETURN Exp deriving (Eq, Show)

data Cond = EQA' AExp AExp
          | CONS' Exp EVar EVar AVar deriving (Eq, Show)

idProg :: Prog
idProg = [
  (DEFINE "Id" [e'x]
    (RETURN e'x))
  ] where e'x = (PVE "x")


class APPLY a b where (/.) :: a -> b -> a

instance APPLY Exp Env where
 (ATOM a) /. env = ATOM a
 (CONS h t) /. env = CONS (h /. env) (t /. env)
 var /. env = head [ x | (v := x) <- env, v == var ]
 
instance APPLY a b => APPLY [a] b where
  xs /. y = map (/. y) xs
  

instance Eq Bind where
 (var1 := _) == (var2 := _) = (var1 == var2)


class UPDATE a where (+.) :: a -> a -> a

instance UPDATE Env where
  binds +. binds' = nub (binds' ++ binds)

mkEnv :: [Var] -> [EVal] -> Env
mkEnv = zipWith (\var val -> (var := val))

getDef :: FName -> Prog -> FDef
getDef fn p = head [ fd | fd@(DEFINE f _ _) <- p, f == fn ]


------------------

int :: Prog -> [EVal] -> EVal
int p d = eval s p
            where (DEFINE f prms _) : p' = p
                  e = mkEnv prms d
                  s = ((CALL f prms), e)


eval :: State -> Prog -> EVal
eval s@((CALL f args), e) p = eval s' p
                              where DEFINE _ prms t' = getDef f p
                                    e' = mkEnv prms (args /. e)
                                    s' = (t', e')

eval s@((ALT c t1 t2), e) p = case cond c e of
                               TRUE ue -> eval (t1, e +. ue) p
                               FALSE ue -> eval (t2, e +. ue) p

eval s@(RETURN exp, e) p = exp /. e

data CondRes = TRUE Env | FALSE Env

cond :: Cond -> Env -> CondRes
cond (EQA' x y)         e = let x' = x /. e; y' = y /. e in
                            case (x', y') of
                               (ATOM a, ATOM b) | a == b -> TRUE  []
                               (ATOM a, ATOM b)          -> FALSE []

cond (CONS' x vh vt va) e = let x' = x/.e in
                            case x' of
                                CONS h t -> TRUE  [vh := h, vt := t]
                                ATOM a   -> FALSE [va := x']

match_prog :: Prog
match_prog = [
  (DEFINE "Match"[e_substr,e_string]
    (CALL "CheckPos"[e_substr,e_string,e_substr,e_string])
  ),
  (DEFINE "NextPos"[e_substring,e_string]
    (ALT (CONS' e_string e__ e_string_tail a__)
      (CALL "Match"[e_substring, e_string_tail])
      (RETURN _failResURE)
  )),
  (DEFINE "CheckPos" [e_subs,e_str,e_substring,e_string]
    (ALT (CONS' e_subs e_subs_head e_subs_tail a__)
      (ALT (CONS' e_subs_head e__ e__ a_subs_head)
      (RETURN _failResURE)
      (ALT (CONS' e_str e_str_head e_str_tail a__)
        (ALT (CONS' e_str_head e__ e__ a_str_head)
          (RETURN _failResURE)
          (ALT (EQA' a_subs_head a_str_head)
            (CALL "CheckPos"[e_subs_tail,e_str_tail,
                              e_substring,e_string])
            (CALL "NextPos"[e_substring,e_string])
          ))
        (RETURN _failResURE)))
    (RETURN _SUCCESS)))]
      where
        e_substr = PVE "substr"
        e_string = PVE "string"
        e_string_tail = PVE "e_string_tail"
        e_substring = PVE "substring"
        e__ = PVE "_"
        e_string-tail = PVE "string-tail"
        a__ = PVA "_"
        e_subs = PVE "subs"
        e_str = PVE "str"
        a_subs_head = PVA "subs_head"
        e_subs_head = PVE "subs_head"
        e_subs_tail = PVE "subs_tail"
        a_str_head = PVA "str_head"
        e_str_head = PVE "str_head"
        e_str_tail = PVE "str_tail"
        _failResURE = ATOM "_failResURE"
        _SUCCESS = ATOM "_SUCCESS"

--------------

type CVar = Exp
type CExp = Exp
type CBind = Bind
type CEnv = Env
type CExps = [CExp]
type CVars = [CVar]

data InEq  = CExp :=/=: CExp deriving Show
data Restr = INCONSISTENT | RESTR [InEq] deriving (Show, Eq)

instance Eq InEq where
    (l1 :=/=: r1) == (l2 :=/=: r2) | (l1==l2) && (r1==r2) = True
                                   | (l1==r2) && (r1==l2) = True
                                   | otherwise            = False

isContradiction, isTautology :: InEq -> Bool
isContradiction (left :=/=: right) = (left == right)

isTautology (ATOM a :=/=: ATOM b) = (a /= b)
isTautology (left   :=/=: right)  = False

cleanRestr :: Restr -> Restr
cleanRestr INCONSISTENT         = INCONSISTENT
cleanRestr (RESTR ineqs)
    | any isContradiction ineqs = INCONSISTENT
    | otherwise                 = RESTR $ nub $ filter (not . isTautology) ineqs

instance UPDATE Restr where
  INCONSISTENT +. _            = INCONSISTENT
  _            +. INCONSISTENT = INCONSISTENT
  (RESTR r1)   +. (RESTR r2)   = cleanRestr $ RESTR(r1 ++ r2)

class CVARS a where cvars :: a -> [CExp]

instance CVARS CExp where
  cvars (ATOM _)     = []
  cvars cvar@(CVA _) = [cvar]
  cvars cvar@(CVE _) = [cvar]
  cvars (CONS h t)   = nub $ (cvars h) ++ (cvars t)

instance CVARS InEq where
  cvars (ax :=/=: bx) = nub $ (cvars ax) ++ (cvars bx)

instance CVARS CBind where
  cvars (pvar := cx) = cvars cx

instance CVARS c => CVARS [c] where
  cvars cxs = nub $ concat $ map cvars cxs

instance CVARS Restr where
  cvars INCONSISTENT = []
  cvars (RESTR ineqs) = cvars ineqs

instance (CVARS a, CVARS b) => CVARS (a, b) where
  cvars (ax, bx) = nub $ (cvars ax) ++ (cvars bx)


--------------
data SBind = CVar :-> CExp deriving Show

type Subst = [SBind]

dom :: Subst -> [CExp]
dom subst = [ cvar | (cvar :-> _ ) <- subst ]

instance APPLY CExp Subst where
  (ATOM a)   /. s          = ATOM a
  (CONS h t) /. s          = CONS (h/.s) (t/.s)
  cvar       /. s
    | cvar `notElem` dom s = cvar
    | otherwise            =  head[ cexp | (cv:->cexp) <- s, cv==cvar ]


instance APPLY InEq Subst where
  (l :=/=: r) /. subst = (l /. subst) :=/=: (r /. subst)

instance APPLY CBind Subst where
  (pvar := cexpr) /. subst = pvar := (cexpr /. subst)

--instance APPLY a subst => APPLY [a] subst where
--  cxs /. subst = map (/. subst) cxs

instance (APPLY a Subst, APPLY b Subst) => APPLY (a,b) Subst where
    (ax,bx) /. subst = ( (ax /. subst) , (bx /. subst) )

instance APPLY Restr Subst where
  INCONSISTENT  /. subst = INCONSISTENT
  (RESTR ineqs) /. subst = cleanRestr $ RESTR $ ineqs /. subst


instance APPLY Term Subst where
  (ALT cond t1 t2) /. subst = (ALT (cond /.subst) (t1 /.subst) (t2 /. subst))
  (CALL name exps) /. subst = (CALL name (exps /. subst))
  (RETURN exp) /. subst = (RETURN (exp /. subst))

instance APPLY Cond Subst where
  (EQA' x y) /. subst = (EQA' (x /. subst) (y /. subst))
  (CONS' exp h t a) /. subst = (CONS' (exp /. subst) h t a)
-------------

type UnifRes = (Bool, Subst)

failRes :: UnifRes
failRes = (False, [])

data Clash = CExp :=: CExp
type Clashes = [Clash]

unify :: CExps -> CExps -> UnifRes
unify ces1 ces2
      | (length ces1) /= (length ces2) = failRes
      | otherwise                      = unify' [] chs
                                         where chs = zipWith (\a b -> (a :=: b)) ces1 ces2

unify' :: Clashes -> Clashes -> UnifRes
unify' rchs []            = (True, subst)
                            where subst = (map (\ (a :=: b) -> (a :-> b)) rchs)

unify' rchs chs@(ch:chs') =
  case ch of
      ATOM a       :=: ATOM b | a==b -> unify' rchs chs'
      ATOM a       :=: cex           -> failRes
      cvar@(CVA _) :=: caex@(ATOM _) -> moveCl rchs chs
      cvar@(CVA _) :=: caex@(CVA _)  -> moveCl rchs chs
      cvar@(CVA _) :=: cex           -> failRes
      cvar@(CVE _) :=: cex           -> moveCl rchs chs
      CONS a1 b1   :=: CONS a2 b2    -> unify' rchs (p ++ chs')
                                        where p = [a1 :=: a2, b1 :=: b2]
      CONS a1 b1   :=: cex           -> failRes

moveCl :: Clashes -> Clashes -> UnifRes
moveCl rchs chs@(ch@(cvar :=: cexp):chs') =
  case [y | (x :=: y) <- rchs, x == cvar] of
       []              -> unify' (ch:rchs) chs'
       [y] | y == cexp -> unify' rchs chs'
       [y] | otherwise -> failRes

-------------
type Class = (CExps, Restr)
type Conf = ((Term, CEnv), Restr)
data Contr = S Subst | R Restr deriving Show
type Split = (Contr, Contr)
type Classes = [Class]

idC, emptC :: Contr
idC = S []
emptC = R INCONSISTENT

instance APPLY c Subst => APPLY (c, Restr) Contr where
  (cx, rs) /. (S subst) = (cx /. subst, rs /. subst)
  (cx, rs) /. (R restr) = (cx,          rs +. restr)

instance APPLY Clash Subst where
  (l :=: r) /. subst = (l /. subst) :=: (r /. subst)

instance APPLY Clash Contr where
  clash /. (S subst) = clash /. subst
  clash /. (R restr) = clash

-------------
type FreeIndex = Int

freeindex :: FreeIndex -> Class -> FreeIndex
freeindex i c = 1 + maximum (i:(map index (cvars c)))
                where index :: CVar -> Int
                      index (CVA i) = i
                      index (CVE i) = i


mkCAVar, mkCEVar :: FreeIndex -> (CVar, FreeIndex)
mkCEVar i = ((CVE i), i+1 )
mkCAVar i = ((CVA i), i+1 )

splitA :: CVar -> CExp -> Split
splitA cv@(CVA _) caexp = (S[cv:->caexp], R (RESTR[cv:=/=:caexp]))

splitE :: CVar -> FreeIndex -> (Split,FreeIndex)
splitE cv@(CVE _) i = ((S[cv:->(CONS cvh cvt)], S[cv:->cva]), i')
                       where (cvh,i1)=mkCEVar i
                             (cvt,i2)=mkCEVar i1
                             (cva,i')=mkCAVar i2

data Tree = LEAF Conf | NODE Conf [ Branch ]
type Branch = (Contr, Tree)

-- type TreeShower = (Tree, Int)
-- instance (Show TreeShower) where
--   show = "abc"

tabs :: Int -> String
tabs x = concat $ replicate x "\t"

showTree :: Int -> Tree -> String
showTree x t = showTree2 0 x t

showTree2 :: Int -> Int -> Tree -> String
showTree2 i _ (LEAF c)      = "LEAF " ++ (show c)
showTree2 i 0 (NODE c bs)   = "NODE " ++ (show c) ++ " branches: " ++ (show $ length bs)
showTree2 i x n@(NODE c bs) = (showTree2 i 0 n) ++ " " ++ (unwords $ (map showTreeHelper bs))
  where
    x' = x - 1
    showTreeHelper (bcontr, btree) = "\r\n" ++ (tabs i) ++ (show bcontr) ++ (showTree2 (i+1) x' btree)

leafs :: Tree -> [Conf]
leafs (LEAF c) = [c]
leafs (NODE c []) = []
leafs (NODE conf (b:bs)) = (leafs (snd b)) ++ (concat $ map (leafs . snd) bs)

ptr :: Prog -> Class -> Tree
ptr p cl@(ces, r) = evalT c p i
                      where
                        (DEFINE f prms _) : p' = p
                        ce = mkEnv prms ces
                        c = ((CALL f prms, ce), r)
                        i = freeindex 0 cl

xptr :: Prog -> Class -> Tree
xptr p cl = NODE c (cutT brs)
  where NODE c brs = ptr p cl

cutT :: [Branch] -> [Branch]
cutT [] = []
cutT (b@(cnt, tree):bs) =
  case tree of
    LEAF (_, INCONSISTENT)   -> cutT bs
    NODE (_, INCONSISTENT) _ -> cutT bs
    LEAF c                   -> b:(cutT bs)
    NODE c bs'               -> b':(cutT bs)
      where b' = (cnt, NODE c (cutT bs'))


evalT :: Conf -> Prog -> FreeIndex -> Tree
evalT c@((CALL f args , ce), r) p i =
                     NODE c [ (idC, evalT c' p i) ]
                     where
                       DEFINE _ prms t' = getDef f p
                       ce' = mkEnv prms (args/.ce)
                       c' = ((t',ce'), r)

evalT c@((ALT cnd t1 t2 , ce), r) p i =
                     NODE c [(cnt1,evalT c1' p i'),(cnt2,evalT c2' p i')]
                     where
                       ((cnt1,cnt2), uce1, uce2, i') = ccond cnd ce i
                       ((_,ce1),r1) = c/.cnt1
                       c1' = ((t1, ce1+.uce1), r1)
                       ((_, ce2),r2) = c/.cnt2
                       c2' = ((t2, ce2+.uce2), r2)

evalT c@((RETURN exp,ce),r) p i = LEAF c

ccond :: Cond->CEnv->FreeIndex -> (Split,CEnv,CEnv,FreeIndex)
ccond (EQA' x y) ce i =
                let x' = x/.ce; y' = y/.ce in
                  case (x', y') of
                    (a, b) | a==b    -> ((idC ,emptC), [], [], i)
                    (ATOM a, ATOM b) -> ((emptC, idC), [], [], i)
                    (CVA _, a)       -> (splitA x' a,  [], [], i)
                    (a, CVA _)       -> (splitA y' a,  [], [], i)

ccond (CONS' x vh vt va) ce i =
                let x' = x/.ce in
                  case x' of
                    CONS h t -> ((idC,emptC), [vh:=h,vt:=t],    [],       i)
                    ATOM a   -> ((emptC,idC), [],               [va:=x'], i)
                    CVA _    -> ((emptC,idC), [],               [va:=x'], i)
                    CVE _    -> (split      , [vh:=ch, vt:=ct], [va:=ca], i')
                      where
                        (split, i') = splitE x' i
                        (S[_:->(CONS ch ct)], S[_:->ca]) = split

-- hw, coursework, debugger
