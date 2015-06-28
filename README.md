prompt
======

Monad (and transformer) for delayed-effect "pure" prompt-and-respose queries.

Prompt
------

`Prompt a b r` represents a "pure" computation producing an `r` that can "ask" or
"prompt" with an `a` and get `b`'s as responses/answers.

By "pure", I mean that the actual eventual process of *answering* the prompts
might be effectful (it might involve IO, or state, or STM...like database
queries or prompts to a user).  When we're writing our actual logic, we never
involve anything with IO, State, etc., so we don't unleash a whole can of
worms by using, for example, a monad transformer over `IO`.

~~~haskell
import Control.Monad.Prompt

data Foo = Foo { fooBar :: String
               , fooBaz :: Int
               } deriving Show

promptFoo :: Prompt String String Foo
promptFoo = Foo <$> prompt "bar" <*> (length <$> prompt "baz")
~~~

Here we build a `Foo` from a context where we can ask with strings and get
strings in return.

Let's build a `Foo` from stdin/stdout:

~~~haskell
ghci> :t runPromptM
runPromptM :: Monad m => Prompt a b r -> (a -> m b) -> m r
ghci> runPromptM promptFoo $ \prmpt -> do putStrLn prmpt; getLine
bar         -- stdout prompt
> hello!    -- stdin response typed in
baz         -- stdout prompt
> i am baz  -- stdin response typed in
Foo "hello!" 8  -- result
~~~

(by the way, that's also `interactP promptFoo`)

Now let's build one by asking for environment variables

~~~haskell
ghci> import System.Environment
ghci> setEnv "bar" "hello!"
ghci> setEnv "baz" "i am baz"
ghci> runPromptM promptFoo getEnv
Foo "hello!" 8
~~~

`promptFoo` is completely "pure", and doesn't ever involve IO or anything, and
doesn't even have IO in the type.  We can run `promptFoo` in `IO` if we
wanted, like above...or we can even run "without" IO, too:

~~~haskell
ghci> import qualified Data.Map as M
ghci> let testMap = M.fromList [("bar", "hello!"), ("baz", "i am baz")]
ghci> :t runPrompt
runPrompt :: Prompt a b r -> (a -> b) -> r
ghci> runPrompt promptFoo (testMap M.!)
Foo "hello!" 8
~~~

Now you can do things like querying databases, prompting the user, etc.,
without ever involving `IO` at all in your logic.  With a `Prompt`, we can
worry that it will never produce arbitrary IO effects!  You can be certain
that a `Prompt` will never call `launchMissiles`, like a `getFoo :: IO Foo`
might!

With Transformers
-----------------

`Prompt a b` can be used as monad to transform for any monad transformer to
give an "interactive source" at the bottom of any monad transformer.

Have you ever wanted to have `State`, with some aspect of IO, like writing to
a database, doing network interactions, or querying a database, but didn't
want to have an ugly terrible `StateT s IO`?  Well, wish no more!  You can
have `StateT s (Prompt String String) a`, for a `State s` computation that can
occasionally depend on asking the user, or the environment variables, or a
network connection, or a database in IO or whatever.  But now you can be sure
it won't ever do arbitrary IO --- it'll only do exactly what IO it needs that
you specify when you "run" it.  Your "pure" computation doesn't involve IO at
all!  All you added was an extra "promptable source".

You can also use this to get short-circuiting behavior with `MaybeT`, etc.

~~~haskell
import Control.Monad.Trans
import Control.Monad.Prompt
import Text.Read

promptFoo2 :: MaybeT (Prompt String String) Foo
promptFoo2 = do
    bar <- lift $ prompt "bar"
    x   <- lift $ prompt "baz"
    case readMaybe x of
      Just baz -> return baz
      Nothing  -> mzero
~~~

~~~haskell
ghci> runPromptM (runMaybeT promptFoo2) getEnv
Nothing
ghci> runPromptM (runMaybeT (promptFoo2 <|> return (Foo "error" 0))) getEnv
Just (Foo "error" 0)
ghci> setEnv "baz" "19"
ghci> runPromptM (runMaybeT (promptFoo2 <|> return (Foo "error" 0))) getEnv
Just (Foo "hello!" 19)
~~~

This becomes pretty nice with `ExceptT` or any instance of `MonadError`, where
you can use `throwError`, `catchError`, etc., to have actual data with your
errors.

You can also play with using for the return type.  For example:

~~~haskell
logEvens :: StateT Int (Prompt String ()) ()
logEvens = do
    modify (+1)
    x <- get
    when (even x) . lift $ prompt (show x)
~~~

~~~haskell
> runPromptM (runStateT (replicateM 10 logEvens) 0) putStrLn
2
4
6
8
10
~~~

That gives you streaming logging, or streaming writing-to-a-database, etc.

Note that `Prompt () r` is equivalent to `Reader r`.

### Typeclass

There's also the typeclass `MonadPrompt` offered, which allows you to write
code polymorphic over all things that can be "prompted".  For example, the
above example can be written as:

~~~haskell
promptFoo2 :: (MonadPlus m, MonadPrompt String String m) => m Foo
promptFoo2 = do
    bar <- prompt "bar"
    x   <- prompt "baz"
    case readMaybe x of
      Just baz -> return baz
      Nothing  -> mzero

promptFoo :: (MonadPrompt String String m) => m Foo
promptFoo = Foo <$> prompt "bar" <*> (length <$> prompt "baz")
~~~

~~~haskell
ghci> interactP . runMaybeT $ promptFoo2 <|> promptFoo
bar
> hello!
baz
> 19
Foo "hello!" 19
ghci> interactP . runMaybeT $ promptFoo2 <|> promptFoo
bar
> hello!
baz
> i am baz
bar         -- failure to parse, so retry with `promptFoo`
> hello!
baz
> i am baz
Foo "hello!" 8
~~~

PromptT
-------

`PromptT a b t r` allows your prompting-and-responding to take place in the
context of `Traversable` `t`, so you can do things like short-circuiting with
`Either e` or `Maybe`, or multiple branches for `[]`, etc --- all "purely",
without worrying about the eventual effects like IO.

In some ways, this is a bit redundant, because `ParserT a b Maybe` is somewhat
equivalent to `MaybeT (Parser a b)`.  However, using `ParserT` can be more
convenient because you can use arbitrary Traversables, and also there are
functions given to make this work "out of the box", instead of manually
unwrapping with `runMaybeT`, `runExceptT`, etc.

~~~haskell
ghci> interactPT $ promptFoo2 <|> promptFoo
bar
> hello!
baz
> 19
Foo "hello!" 19
ghci> interactPT $ promptFoo2 <|> promptFoo
bar
> hello!
baz
> i am baz
bar         -- failure to parse, so retry with `promptFoo`
> hello!
baz
> i am baz
Foo "hello!" 8
~~~

Here we used `promptFoo2` that was written polymorphically, and specialized it
to `PromptT String String Maybe a`.  We can write it monomorphically too, as:

~~~haskell
promptFoo2 :: PromptT String String Maybe a
promptFoo2 = do
    bar <- prompt "bar"
    x   <- prompt "baz"
    lift $ readMaybe x
~~~

But I really think the polymorphic version (with `mzero` and `return`) looks
nicer!  `Alternative`, `MonadPlus`, `MonadError`, `MonadWriter`, etc. are all
supported.  And you can specify your logic, etc;, and your prompting can
involve IO.  But your logic doesn't ever involve `IO` at all!

One big advanage of this way over the extra transformer is that your
"prompting function" has access to the underlying `Traversable` `t` as well,
so you can communicate with the underlying prompt using your "prompt response"
function.

Which leads to the big finale!

~~~haskell
import Control.Monad.Error.Class
import Control.Monad.Prompt
import Text.Read
import qualified Data.Map as M

type Key = String
type Val = String

data MyError = MENoParse Key Val
             | MENotFound Key
             deriving Show

promptRead :: (MonadError MyError m, MonadPrompt Key Val m, Read b)
           => Key -> m b
-- promptRead :: Read b => Key -> PromptT Key Val (Either MyError) b
-- promptRead :: Read b => Key -> ExceptT MyError (Prompt Key Val) b
promptRead k = do
    resp <- prompt k
    case readMaybe resp of
      Nothing -> throwError $ MEParse k resp
      Just v  -> return v

promptFoo3 :: MonadPrompt Key Val m => m Foo
-- promptFoo3 :: PromptT Key Val m Foo
promptFoo3 = Foo <$> prompt "bar" <*> promptRead "baz"

throughEnv :: IO (Either MyError Foo)
throughEnv = runPromptTM parseFoo3 $ \k -> do
    env <- lookupEnv k
    case env of
      Nothing -> return . Left  $ MENotFound k
      Just v  -> return . Right $ v

throughStdIO :: IO (Either MyError Foo)
throughStdIO = interactPT parseFoo3

throughStdIOBlankIsError :: IO (Either MyError Foo)
throughStdIOBlankIsError = runPromptTM parseFoo3 $ \k -> do
    putStrLn k
    resp <- getLine
    if null resp
      then return . Left  $ MENotFound k
      else return . Right $ resp

throughMap :: M.Map Key Val -> Either MyError Foo
throughMap m = runPromptT parseFoo3 $ \k ->
    case M.lookup k m of
      Nothing -> Left (MENotFound k)
      Just v  -> Right v
~~~


Comparisons
-----------

To lay it all on the floor,

~~~haskell
data PromptT a b t r = PromptT { runPromptTM :: forall m. Monad m => (a -> m (t b)) -> m (t r) }
~~~

There is admittedly a popular misconception that I've seen going around that
equates this sort of type to `Free` from the *free* package.  However, `Free`
doesn't really have anything significant to do with this.  Sure, you might be
able to generate this type by using `FreeT` over a specifically chosen
Functor, but...this is the case for literally any Monad ever, so that doesn't
really mean much :)

It's also unrelated in this same manner to `Prompt` from the *MonadPrompt*
package, and `Program` from *operational* too.

This type is also similar in structure to `Bazaar`, from the *lens* package.
The biggest difference that makes `Bazaar` unusable is because the RankN
constraint is only `Applicative`, not `Monad`, so a `Monad` instance is
impossible.  Ignoring that (or if it's okay for you to only use the
`Applicative` instance), `Bazaar` forces the "prompting effect" to take place
in the same context as the `Traversable` `t`...which really defeats the
purpose of this whole thing in the first place (the idea is to be able to
separate your prompting effect from your application logic).  If the
`Traversable` you want to transform has a "monad transformer" version, then
you can somewhat simulate `PromptT` for that specifc `t` with the transformer
version.

It's also somewhat similar to the `Client` type from *pipes*, but it's also
a bit tricky to use that with a different effect type than the logic
`Traversable`, as well...so it has the same difference as `Bazaar` here.

But this type is common/simple enough that I'm sure someone has it somewhere
in a library that I haven't been able to find.  If you find it, let me know!

