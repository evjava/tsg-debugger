{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TsgDebugger where

import Text.Printf
import System.IO
import Control.Monad
import Debug.Trace
import Text.Read

import Int
import TsgExamples

---- utilities

joinLines0 :: String -> [String] -> String
joinLines0 sep (x:y:xs) = joinLines0 sep $ (x ++ sep ++ y):xs
joinLines0 sep (x:[]) = x
joinLines0 sep [] = ""

joinLines = joinLines0 "\n"

printLines :: Show a => [a] -> IO ()
printLines x = putStrLn $ joinLines $ map show x

make_input :: [Char] -> [Char] -> [Exp]
make_input substr str = map convert_str [substr, str]

make_input_sum :: [Char] -> [Char] -> [Exp]
make_input_sum substr str = map convert_str_sum [substr, str]

convert_str :: [Char] -> Exp
convert_str "" = (ATOM "NIL")
convert_str (x:xs) = (CONS (ATOM [x]) (convert_str xs))

convert_str_sum :: [Char] -> Exp
convert_str_sum (x:y:xs) = (CONS (ATOM [x]) (convert_str_sum (y:xs)))
convert_str_sum (x:[]) = (ATOM [x])

----

data FTree = ROOT Prog Int [FTree]
           | DEF FDef Int FTree
           | TERM Term Int [FTree] deriving (Eq, Show)

defLine :: FTree -> FName -> Int
defLine ft@(ROOT _ _ ts) fn = find ts
  where
    find :: [FTree] -> Int
    find (fd@(DEF (DEFINE f _ _) s _):defs) = if (f == fn)
      then 1
      else s + (find defs)

defTerm :: FTree -> FName -> FTree
defTerm ft@(ROOT _ _ ts) fn = find ts
  where
    find :: [FTree] -> FTree
    find (fd@(DEF (DEFINE f _ _) s ft):defs) = if (f == fn)
      then ft
      else find defs

size :: FTree -> Int
size (ROOT _ x _) = x
size (DEF  _ x _) = x
size (TERM _ x _) = x

desc :: FTree -> [FTree]
desc (ROOT _ _ ts) = ts
desc (DEF _ _ t)   = [t]
desc (TERM (ALT _ _ _) _ ts) = ts
desc (TERM t s []) = []

firstLine :: FTree -> String
firstLine (ROOT _ _ _) = "PROGRAM"
firstLine (DEF (DEFINE f args _) _ _)   = "DEFINE " ++ f ++ (show args)
firstLine (TERM (ALT cond _ _) _ _) = "ALT (" ++ (show cond) ++ ")"
firstLine (TERM t _ []) = (show t)

-- tree builder

class TBUILDER a where buildTree :: a -> FTree

instance TBUILDER Prog where
  buildTree p = ROOT p (sum $ map size trees) trees
    where trees = map buildTree p

instance TBUILDER FDef where
  buildTree fd@(DEFINE f _ t) = DEF fd (s + 1) tr
    where tr@(TERM _ s _) = buildTree t

instance TBUILDER Term where
  buildTree t@(CALL name exps) = TERM t 1 []
  buildTree t@(RETURN exp) = TERM t 1 []
  buildTree t@(ALT cond t1 t2) =
    let
      tr1@(TERM _ s1 _) = buildTree t1
      tr2@(TERM _ s2 _) = buildTree t2
    in TERM t (1 + s1 + s2) [tr1, tr2]

-- printing program

type PrefixPrinter = (Int, Int) -- (indent, num)

child :: PrefixPrinter -> PrefixPrinter
child (i, num) = (i + 1, num + 1)
sibling :: PrefixPrinter -> FTree -> PrefixPrinter
sibling (i, num) ft = (i, num + (size ft))

layout0 :: PrefixPrinter -> FTree -> [String]
layout0 pp@(x, num) ft = let
  prefHeader = (printf "%3d)" num) ++ (concat (replicate x "\t"))
  header = prefHeader ++ (firstLine ft)
  footer = layout1 (child pp) $ desc ft
  in header:footer

layout1 :: PrefixPrinter -> [FTree] -> [String]
layout1 x (ft:fts) = (layout0 x ft) ++ (layout1 (sibling x ft) fts)
layout1 x [] = []

layout :: FTree -> [String]
layout ft = layout0 (0, 0) ft

printProg :: FTree -> IO ()
printProg x = putStrLn $ joinLines $ layout x

--- eval state
type BreakEnv = [(Int, Env)]
data EvalState = ES {
  prog :: FTree,        -- full program (TODO specify ROOT?)
  benv :: BreakEnv,     -- breakpoint environment
  state :: State,       -- current state
  ftree :: FTree,       -- current FTree
  pos :: Int,           -- current position
  cnt :: Int,           -- total count of steps
  shortifyEnv :: Bool
  } deriving Show

mkEvalState :: Prog -> [EVal] -> EvalState
mkEvalState p args =
  let
    (DEFINE f prms _) : p' = p
    t = (CALL f prms)
    s = (t, mkEnv prms args)
    ft = TERM t 0 []
  in
    ES {prog = buildTree p, benv = [], state = s, ftree = ft, pos = 0, cnt = 0, shortifyEnv = False}

toggleBreak :: Int -> EvalState -> EvalState
toggleBreak bid (es@ES{ benv = bs }) =
  let bsNoBid = [ b | b@(bi, bb) <- (benv es), bi /= bid ] in
    if ((length bsNoBid) == (length bs))
    then es { benv = (bid,[]):bs}
    else es { benv = bsNoBid }

toggleShortifyEnv :: EvalState -> EvalState
toggleShortifyEnv (es@ES { shortifyEnv = se }) = es { shortifyEnv = (not se) }

showExp :: EvalState -> (Exp -> String)
showExp (es@ES{ shortifyEnv=se }) = if se then shortShow else show
  where shortShow x = case x of
          (ATOM a)   -> if (length a == 1) then a else "(" ++ a ++ ")"
          (CONS a b) -> (shortShow a) ++ (shortShow b)

envInfo :: EvalState -> String
envInfo (es@ES{ state=(_, binds)}) = joinLines $ map showBind binds
  where
    showBind (k := v) = printf "\t %s := %s" (show k) (showExp es $ v)

evalStep :: EvalState -> EvalState
evalStep es@(ES { prog=pr, benv=b, state=(t, env), ftree=ft, pos=ps, cnt=cnt }) = case t of
  (CALL f args) -> es { state=s', ftree=ft', pos=fLine + 1, cnt=cnt+1  }
    where (ROOT p _ _) = pr
          fLine = defLine pr f
          DEFINE _ prms t' = getDef f p
          env' = mkEnv prms (args /. env)
          s' = (t', env')
          ft' = defTerm pr f
  (ALT c t1 t2) -> es { state=s', ftree=ft', pos=pos', cnt=cnt+1 }
    where (TERM _ _ [ft1, ft2]) = ft
          (s', pos', ft') = case cond c env of
            TRUE ue -> ((t1, env +. ue), ps + 1, ft1)
            FALSE ue -> ((t2, env +. ue), ps + (size ft1) + 1, ft2)
  (RETURN exp) -> es

isDone :: EvalState -> Bool
isDone ES{ state=(RETURN _, _) } = True
isDone _ = False

isBreak :: EvalState -> Bool
isBreak ES{ benv=bs, pos=p } = elem p $ map fst bs

evalTill :: (EvalState -> Bool) -> EvalState -> EvalState
evalTill isStop es = case (isDone es || (isStop es)) of
  True -> es
  False -> evalTill isStop (evalStep es)

evalTillBreak = evalTill isBreak
evalTillReturn = evalTill (\_ -> False)

evalTillLine :: Int -> (EvalState -> EvalState)
evalTillLine x = evalTill (\ES{ pos=pos } -> pos == x)

evalSteps :: Int -> EvalState -> EvalState
evalSteps x es = if (isDone es || x == 0)
  then es
  else (evalSteps (x-1) (evalStep es))

-- REPL

data Command = PRINT_PROGRAM
             | BREAK Int
             | RUN
             | JUMP Int
             | STEP Int
             | TOGGLE_ENV deriving Show

parseInt :: String -> (Int -> a) -> Maybe a
parseInt x callback = case (readMaybe x :: Maybe Int) of
  Just xx -> Just $ callback xx
  Nothing -> Nothing

parseCommand :: String -> Maybe Command
parseCommand "p" = Just $ PRINT_PROGRAM
parseCommand ('b':x) = parseInt x (\xx -> BREAK xx)
parseCommand "r" = Just $ RUN
parseCommand ('j':x) = parseInt x (\xx -> JUMP xx)
parseCommand "s" = Just $ STEP 1
parseCommand "n" = Just $ STEP 1
parseCommand "t" = Just $ TOGGLE_ENV
parseCommand ('s':x) = parseInt x (\xx -> STEP xx)
parseCommand _ = Nothing

applyCommand :: EvalState -> Command -> (EvalState, String)
applyCommand s PRINT_PROGRAM = (s, ((joinLines.layout.prog) s) ++ "\n=====")
applyCommand s (BREAK b) = (toggleBreak b s, "evaluated: toggle breakpoint: " ++ (show b))
-- todo message if done
applyCommand s RUN = (evalTillBreak $ evalStep s, "evaluated: RUN")
applyCommand s (JUMP x) = (evalTillLine x s, "evaluated: JUMP " ++ (show x))
applyCommand s (STEP x) = (evalSteps x s, "evaluated: STEP " ++ (show x))
applyCommand s TOGGLE_ENV = (toggleShortifyEnv s, "evaluated: TOGGLE_ENV")

helpMessage = "HELP \n\
\ <p>: print program \n\
\ <b x:Int>: toggle breakpoint for line x \n\
\ <r>: run \n\
\ <n>: do 1 step \n\
\ <t>: toggle long/short vars \n\
\ <s x:Int>: do x steps \n\
\ <j x:Int>: run untill line x \n\
\ <q>, <e>: quit"

evaluator :: (EvalState, String) -> (EvalState, String)
evaluator (s, event) = case parseCommand event of
  Just cmd -> applyCommand s cmd
  Nothing -> (s, helpMessage)

result :: EvalState -> [String]
result s = case isDone s of
  True -> ["RESULT: " ++ (showExp s $ e /. sub)]
    where ((RETURN e), sub) = state s
  False -> []
  

info :: EvalState -> String
info s = joinLines $
         [ "current line: " ++ (show $ pos s)
         -- , "breakpoints: " ++ (show $ benv s)
         , "breakpoints2: " ++ (show $ map fst $ benv s)
         , "env:\n" ++ (envInfo s)
         , "cnt: " ++ (show $ cnt s)
         ] ++ (result s)

runner :: EvalState -> IO ()
runner i = do
  input <- putStr "REPL> " >> hFlush stdout >> getLine
  unless (input == "q" || input == "e")
    $ let (i', out) = evaluator (i, input) in putStrLn ((info i') ++ "\n" ++ out) >> runner i'
  
  
-- tmp
est = mkEvalState match_prog $ make_input "mpu" "metacomputation"
r1 = runner est

est2 = toggleShortifyEnv $ toggleBreak 13 $ mkEvalState tsgSumProg $ make_input "110100" "10010"
r2 = runner est2

est3 = toggleShortifyEnv $ mkEvalState reverseProg [convert_str "abcdef"]
r3 = runner est3
