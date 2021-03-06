module Eval2
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

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> runStateError m s >>= \(v :!: s') -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:

instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (lookfor' v)
    where lookfor' v s = case M.lookup v s of Nothing -> Left UndefVar
                                              Just x -> Right (x :!: s)
  update v i = StateError (\s -> Right (() :!: update' v i s))
    where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = runStateError (stepCommStar p) initEnv >>= \(() :!: s) -> return s

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
evalExp (ESeq e1 e2) = evalExp e1 >> evalExp e2
