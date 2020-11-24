module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
--stepComm Skip = return ()
--stepComm (Let v e) = evalExp e >>= \r -> update v r
-- Capaz hay que hacer un case por si es skip
stepComm (Seq c1 c2) = stepComm c1 >>= \c1' -> return (Seq c1' c2)
stepComm (IfThenElse e cthen celse) =
    evalExp e >>= \r -> return (if r then cthen else celse)
-- stepComm (While e c) =


-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
-- Int
evalExp (Const x) = return x
evalExp (Var x) = lookfor x
evalExp (UMinus x) = evalExp x >>= \y -> return (-y)
evalExp (Plus x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' + y')

evalExp (Minus x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' - y')
evalExp (Times x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' * y')
evalExp (Div x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' `div` y')
-- Bool
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' < y')
evalExp (Gt x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' > y')
evalExp (And x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' && y')
evalExp (Or x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' || y')
evalExp (Not x) = evalExp x >>= \y -> return (not y)
evalExp (Eq x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' == y')
evalExp (NEq x y) = 
    do 
        x' <- evalExp x
        y' <- evalExp y
        return (x' /= y')
evalExp (EAssgn v exp) =
    do
        r <- evalExp exp
        update v r
        return r
evalExp (ESeq e1 e2) = 
    evalExp e1 >>= \r -> evalExp e2

