# Monad Transformers Explained through Abstract machines.

I don't do much Haskell programming, but it's been on my to do list
for quite a while.  I understand monads pretty well, but not monad
transformers.  Monad transformers form the basis of [`mtl`](), a
popular Haskell library for building programs out of common monads by
"stacking" them together.  I have found a few good monad transformer
tutorials, but none of them really get at the core idea of how I think
about functional programming: things slamming into things.  This
tutorial attempts to give copious explanations, diagrams, and
examples.  As a note, this presentation is not my own: the examples
follow 

The problem with monads (that transformers solve) is basically that
you have tons of useful monads: IO, Maybe, List, etc... Monads give us
an API for writing succinct programs that don't have to worry about
the underlying implementation details.  This allows us to write
programs that throw errors, read from an environment, and so on.  But
when we want to *combine* the behavior of multiple monads, we
essentially have to recreate the behavior: and we might end up doing
it wrong.  To demonstrate how monad transformers work here, we'll use
them to build interpreters (for a small lambda calculus language)
which incorporate other effects (such as tracing computation, throwing
errors, etc...).

# Running example: an interpreter for a small language

The running example of the tutorial is an interpreter for a small call
by value lambda calculus implementation.  (This is not to be confused
with an interpreter for Haskell's take on the lambda calculus, which
has a [call by need]() semantics.)

    type Name = String
	
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

Here, we represent variable names by Strings.  The `Exp` type is the
type of expressions in our language.  For illustration, here are some
example terms of our language.

    t1 = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
	-- encodes 12 + (λx. x) (4 + 2)
	t2 = App (Abs "x" $ App (Var "x") (Var "x")) (Abs "x" $ App (Var "x") (Var "x")) -- omega
	-- encodes (λx. x x) (λx. x x) = Ω
    t3 = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))
	-- encodes 12 + (\x. y) (4 + 2)
	t4 = Lit 12 `Plus` (Abs "x" $ Var "y")

Thinking in a call by value language, we can reduce these expressions
in our head:

    t₁: 
      12 + (λx. x) (4 + 2)
	→ 12 + (λx. x) (6)
	→ 12 + 6
	→ 18
	t₂:
	  (λx. x x) (λx. x x)
	→ (λx. x x) (λx. x x)
	→ (λx. x x) (λx. x x)
	→ ...
	〚t₂〛= ⊥
	t₃:
	  12 + (λx. y) (4 + 2)
	→ 12 + y
	→ ???

What's going on in t₃?  The rule (beta, in this case) for reducing
lambda expressions to values (normal form) is simple: reduce the
argument to a value, and then "slam" the value into occurences of x.
But in this case: there are no occurences of x inside the body
`(λx. y)`, which is just `y`.  Now we run into the problem: when we
want to add 12 to `y`, how do we know to use for the value `y`?  The
answer is that `y` is a *free variable*, which must be given meaning
by an *environment*.  An environment maps variables to values:

    type Env = Map.Map Name Value
    emptyenv :: Env = Map.empty

We can think of this as a function `{Name → Value}`.  (`emptyenv` is
just an empty environment, which is the map that doesn't map
anything.)  So this means that we can map things like `"y"` to things
like `IntVal 3` (since `y` is of type `Name`, and `IntVal 3` is type
`Value`).  But we can also map `y` to values like `λx. x`, by wrapping
the function value in a `FunVal emptyenv "x" (Var "x")`.  Note that in
this case, we are not only holding functional values in the domain of
the map, but [closures](): we map to lambda expressions, along with an
environment which gives us a way to interpret the free variables.  How
does this help us solve the problem in the reduction of t₃?  Well, we
can use a mapping from names to values to look up a value for `y`.
Let's say that we suppose our environment has a mapping from `y` to
`3`:

	t₃, env = {y → 3}:
	  12 + (λx. y) (4 + 2)
	→ 12 + y
	→ 12 + 3
    → 15
	
Note that I've tacticly glossed over a potentially ugly point here:
I've wrriten env = {y → 3}, when (in reality) it should be {"y" →
IntVal 3} (to get the types right).  But let's say we have an
environment that maps `y` to a closure instead:

	t₃, env = {y → clos(λx. x, {})}:
	  12 + (λx. y) (4 + 2)
	→ 12 + y
	→ 12 + (λx. x)
    → ???

(Note that here, you can read clos(λx. x, {}) as simply the closed
lambda term λx. x.)  Here we also have an error: how we add an integer
and a function?  The answer is that this also doesn't make sense.  How about this?

    t₄ = ((λx. y) 3) 3
	t₄, env = {y → 4}:
	  ((λx. y) 3) 3
	→ y 3
	→ 4 3
	→ ???

Throughout the examples, we've shown a few sources of errors:

  - It is invalid to try to lookup a variable if it doesn't have a
    binding in the environment.
  - We can only apply a value to a function (we can't apply a value to
    an integer, where do we slam the `x` into?)
  - When adding two terms, they must both reduce to integers (we can't
    add 3 to λx. x).

There are two main approaches within programming languages to solve
these issues:

  1. Assign *types* to expressions, and statically check them, to rule
    out these errors at compile time.

  2. Run the program: if it doesn't make sense, just stop and spit out
    an error.

(Note that typically you have to prove that the type system you wrote
in point 1 does indeed rule out the runtime errors in point 2.)  These
are the opposite sides of the programming langauge spectrum, and
define the separation between *static* (point 1) and *dynamic* (point
2) type systems.

# A first try at a dynamicly typed evaluator

Let's say we want to write a function which will evalute the mini
language we made here.  (So we don't have to run the examples in our
head!)  What would be the *type* of the interpreter function?  Well,
we passed in an expression, and an environment, when we did the
pencil-and-paper reductions in the previous sections: so it's sensible
to think that we want the type of our evaluator function to be:

    eval0 :: Env -> Exp -> Value

`eval0` takes an envionment (to look up free variables), an expression
(to crunch and compute on), and spits out a value (the final result).
Without further ado, here's the evaluator function:

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

We can even play with this:

    *Transformers> :load "transformers.hs"
	[1 of 1] Compiling Transformers     ( transformers.hs, interpreted )
	Ok, modules loaded: Transformers.
	*Transformers> eval0 emptyenv t1
	IntVal 18
	*Transformers> eval0 env0 t1
	IntVal 18
	*Transformers> eval0 env1 t1
	IntVal 18
	*Transformers> eval0 env1 t5
	FunVal (fromList []) "x" (Var "x")
	*Transformers> eval0 env0 t5
	IntVal 3
	*Transformers> eval0 env0 t2
	^CInterrupted.
	*Transformers> eval0 emptyenv t3
	IntVal *** Exception: Maybe.fromJust: Nothing
	*Transformers> eval0 env0 t3
    IntVal 15

Note that evaluating t₂ (the Ω term) diverged, and had to be stopped
manually.  Also note that t₃ threw an error (could not look up `y`)
with the empty environment, but properly looked up `3` when we
provided the proper environment.

## A few errors

In the previous section, we illustrated various ways in which our
interpreter could "break".  How are these hazards reified in our
interpreter `eval0`?  Let's see what happens (I've superimposed the
previous rules aligned with running our evaluator on these samples):

  - It is invalid to try to lookup a variable if it doesn't have a
    binding in the environment.
    
        *Transformers> eval0 emptyenv t3
        IntVal *** Exception: Maybe.fromJust: Nothing

  - We can only apply a value to a function (we can't apply a value to
    an integer, where do we slam the `x` into?)

        *Transformers> eval0 env1 t4
		IntVal 3
		*Transformers> eval0 env0 t4
		*** Exception: transformers.hs:(47,28)-(48,84): Non-exhaustive patterns in case

  - When adding two terms, they must both reduce to integers (we can't
    add 3 to λx. x).

        *Transformers> eval0 env0 t3
		IntVal 15
		*Transformers> eval0 env1 t3
		IntVal *** Exception: transformers.hs:43:23-46: Irrefutable
		pattern failed for pattern Transformers.IntVal i2

In each of the cases we get errors thrown because we allowed the
errors in our language to percolate up to the metalanguage.  That is,
we simply tried to look up values in a map, regardless of whether they
were in the map or not, we included non exhaustive pattern matching
for dynamic type errors (applying something to a non function, for
example).  In the next series of examples

# Now a monadic evaluator

Now let's say we want to write a *monadic* evaluator.  That is to say,
let's take our `eval0` function and rewrite it into a monadic style.
To lift our eval0 function to a monadic evaluator, we are going to
create an evaluator which generates a *computation*.  The basic idea
is this:

> `eval1` is a function that takes an environment and an expression,
> and produces a computation which will evaluate the expression.  To
> actually evaluate the expression, you have to take the computation,
> and run it to completion.

This is in contrast to `eval0`, which *evaluates* the expression.  Our
monadic evaluator, `eval1`, simply creates a *computation* to run the
expression.  This means that --- because of lazy evaluation in Haskell
--- the actual expression will not be evaluated until you *run* that
expression.

The type of our evaluator function, `eval1`, will be:

    eval1 :: Monad m => Env -> Exp -> m Value

In this case, for any monad `m`, if you give me an environment, and an
expression, I will produce you a computation which carries values.

Let's create a monadic type which will carry the result of the
evaluator.  This type *suspends* the evaluation of the expression.

    type Eval1 a = Identity a

Here we make `Eval1` a type synonym for the `Identity` monad:

    newtype Identity a = Identity { runIdentity :: a }

    instance Monad Identity where
        return a = Identity a
	    m >>= k  = k (runIdentity m)

And, without further ado, the code for `eval1`.

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

