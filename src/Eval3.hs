module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva monada que lleve el costo de las 
-- operaciones efectuadas en la computacion, ademas de manejar errores y 
-- estado, y de su instancia de mónada. Llamela StateErrorCost.
newtype StateErrorCost a =
  StateErrorCost { runStateErrorCost :: Env -> Either Error (Pair a (Env, Cost)) }

-- Para calmar al GHC
instance Functor StateErrorCost where
  fmap = liftM

instance Applicative StateErrorCost where
  pure  = return
  (<*>) = ap

instance Monad StateErrorCost where
  return x = StateErrorCost (\s -> Right (x :!: (s, 0)))
  m >>= f = StateErrorCost (\s ->
                            do (v :!: (s', c)) <- runStateErrorCost m s
                               (v' :!: (s'', c')) <- runStateErrorCost (f v) s'
                               return (v' :!: (s'', c + c')))

-- Ejercicio 3.c: Dar una instancia de MonadCost para StateErrorCost.
instance MonadCost StateErrorCost where
  addCost c = StateErrorCost (\s -> Right (() :!: (s, c)))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorCost.
instance MonadError StateErrorCost where
  throw e = StateErrorCost (\_ -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorCost.
instance MonadState StateErrorCost where
  lookfor v = StateErrorCost (lookfor' v)
    where lookfor' v s = case M.lookup v s of Nothing -> Left UndefVar
                                              Just x -> Right (x :!: (s, 0))
  update v i = StateErrorCost (\s -> Right (() :!: (update' v i s, 0)))
    where update' = M.insert

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorCost.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error (Env, Cost)
eval p = runStateErrorCost (stepCommStar p) initEnv >>= \(() :!: s) -> return s

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadCost m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v e) =
    do
        r <- evalExp e
        update v r
        return Skip
stepComm (Seq Skip c2) = stepComm c2
stepComm (Seq c1 c2) = stepComm c1 >>= \c1' -> return (Seq c1' c2)
stepComm (IfThenElse e cthen celse) =
    evalExp e >>= \r -> return (if r then cthen else celse)
stepComm w@(While e c) =
    evalExp e >>= \r -> return (if r then Seq c w else Skip)

-- Evalua una expresion 
evalExp :: (MonadState m, MonadError m, MonadCost m) => Exp a -> m a
-- Int
evalExp (Const x) = return x
evalExp (Var x) = lookfor x
evalExp (UMinus x) =
    do 
        y <- evalExp x 
        addCost 1
        return (-y)
evalExp (Plus x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 1
        return (x' + y')
evalExp (Minus x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 1
        return (x' - y')
evalExp (Times x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 2
        return (x' * y')
evalExp (Div x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 2
        if y' == 0
            then throw DivByZero
            else return (x' `div` y')
-- Bool
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 1
        return (x' < y')
evalExp (Gt x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 1
        return (x' > y')
evalExp (And x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 1
        return (x' && y')
evalExp (Or x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 1
        return (x' || y')
evalExp (Not x) =
    do 
        y <- evalExp x
        addCost 1
        return (not y)
evalExp (Eq x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 1
        return (x' == y')
evalExp (NEq x y) =
    do
        x' <- evalExp x
        y' <- evalExp y
        addCost 1
        return (x' /= y')
evalExp (EAssgn v exp) =
    do
        r <- evalExp exp
        update v r
        return r
evalExp (ESeq e1 e2) = evalExp e1 >> evalExp e2
