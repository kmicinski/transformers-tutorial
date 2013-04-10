module Transformers where
import Control.Monad.Identity
import Control.Monad.Error
--import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

-- Using a toy language as an example, we will build up a monadic
-- evaluator by combining monad transformers.
data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)
         
data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving(Show)

type Env = Map.Map Name Value

emptyenv :: Env = Map.empty
-- {y -> 3}
env0 :: Env = Map.singleton "y" $ IntVal 3
-- {y -> clos(\x. x, {})}
env1 :: Env = Map.singleton "y" $ FunVal emptyenv "x" $ Var "x"
-- {y -> clos(\x. y, {y -> 3})}
env3 :: Env = Map.singleton "y" $ FunVal env0 "x" $ Var "y"

-- Simple (non monadic) evaluator function

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var x) = fromJust (Map.lookup x env)
eval0 env (Plus e1 e2) = IntVal (i1 + i2)
                    where 
                      IntVal i1 = eval0 env e1
                      IntVal i2 = eval0 env e2
eval0 env (Abs x e) = FunVal env x e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                           FunVal env' x body -> eval0 (Map.insert x val2 env') body

-- Note that the above code can potentially throw errors

-- Example terms to play around with
t1 = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
t2 = App (Abs "x" $ App (Var "x") (Var "x")) (Abs "x" $ App (Var "x") (Var "x")) -- omega
t3 = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))
t4 = App (App (Abs "x" $ Var "y") $ Lit 3) $ Lit 3
t5 = Var "y"

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                              FunVal env' x body ->
                                eval1 (Map.insert x val2 env') body

-- class (Monad m) => MonadError e m | m -> e where
--       throwError :: e -> m a
--       catchError :: m a -> (e -> m a) -> m a

class (Monad m) => MonadReader r m | m -> r where
      ask   :: m r
      local :: (r -> r) -> m a -> m a

newtype Reader r a = Reader { runReader :: r -> a }
instance Monad (Reader r) where
         return a  = Reader $ \_ -> a
         m >>= k   = Reader $ \r -> runReader (k (runReader m r)) r

instance MonadReader r (Reader r) where
         ask = Reader id
         local f m = Reader $ runReader m . f

-- Here the Error 
type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runErrorT

-- Here I'm promising that I'm going to return an (Eval2 Value), which
-- is ErrorT String Identity Value.  This is a monad stack where we
-- have an Identity at the core (just a wrapper around a value), and
-- an error type of String.  when we perform `runErrorT`, we will get
-- back the underlying monad (the thing below `ErrorT`), which is just
-- Identity: a wrapper around values.

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = case (Map.lookup n env) of
                          Just v -> return v
                          Nothing -> fail $ "Could not lookup " ++ n
eval2a env (Plus e1 e2) = do i1 <- eval2a env e1
                             i2 <- eval2a env e2
                             case (i1,i2) of
                                  (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                  _ -> fail "type error!"
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                                 FunVal env' x body ->
                                        eval2a (Map.insert x val2 env') body
