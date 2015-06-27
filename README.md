prompt
======

Monad and transformer for delayed-effect "pure" prompt-and-respose queries.

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

PromptT
-------

`PromptT a b t r` allows your prompting-and-responding to take place in the
context of `Traversable` `t`, so you can do things like short-circuiting with
`Either e` or `Maybe`, or multiple branches for `[]`, etc --- all "purely",
without worrying about the eventual effects like IO.

~~~haskell
import Control.Monad.Trans
import Control.Monad.Prompt
import Text.Read

promptFoo2 :: PromptT String String Maybe Foo
promptFoo2 = do
    bar <- prompt "bar"
    x   <- prompt "baz"
    case readMaybe x of
      Just baz -> return baz
      Nothing  -> mzero
    -- also is `lift (readMaybe x)`
~~~

So now we have the ability to add pure short-circuiting.

~~~haskell
ghci> :t runPromptTM
runPromptTM :: Monad m => PromptT a b t r -> (a -> m (t b)) -> m (t r)
ghci> runPromptTM promptFoo2 (fmap Just . getEnv)
Nothing
ghci> runPromptTM (promptFoo2 <|> return (Foo "error" 0)) (fmap Just . getEnv)
Just (Foo "error" 0)
ghci> setEnv "baz" "19"
ghci> runPromptTM promptFoo2 (fmap Just . getEnv)
Just (Foo "hello!" 19)
~~~

~~~haskell
ghci> runPromptTM (promptFoo2 <|> promptFoo) $ \prmpt -> putStrLn prmpt; Just <$> getLine
bar         -- stdout prompt
> hello!    -- stdin response typed in
baz         -- stdout prompt
> 19        -- stdin response typed in
Just (Foo "hello!" 19)  -- result
ghci> interactPT $ promptFoo2 <|> promptFoo     -- the same thing, using `interactPT`
bar         -- stdout prompt
> hello!    -- stdin response typed in
baz         -- stdout prompt
> world!    -- stdin response typed in
bar         -- stdout prompt, trying promptFoo, because the first time failed
> hello!    -- stdin response typed in
baz         -- stdout prompt
> i am baz  -- stdin response typed in
Just (Foo "hello!" 8)  -- result
~~~

Now, you can program in a short-circuiting context, etc., and not ever open
the door to arbitrary `IO` like a `MaybeT IO a` would!

For more advanced usage, there is a `MonadError` instance, so you can have
`PromptT a b (Either e) r` with things like `throwError` and `catchError` to
"catch" an error value and respond/recover.

You can work over any `Traversable` `t`, so, for example, working under
`Writer w` will let you log items as you receive or prompt them.  All without
ever involving `IO`, etc.!

Your "prompting effect" also has access to the underlying `Traversable` `t`,
so you can mix and match sources of error from between your `Prompt` and also
your prompt effect result.

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

promptRead :: Key -> PromptT Key Val (Either MyError) b
promptRead k = do
    resp <- prompt k
    case readMaybe resp of
      Nothing -> throwError $ MEParse k resp
      Just v  -> return v

promptFoo3 :: PromptT Key Val (Either MyError) b
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
However, `Bazaar` forces the "prompting effect" to take place in the same
context as the `Traversable` `t`...which really defeats the purpose of this
whole thing in the first place (the idea is to be able to separate your
prompting effect from your application logic).  You would be able to model
something like `Prompt` and `runPrompt` if you keep all your `Bazaar`
functions parameterized over all `m`, but even compared to `runPromptM`,
`Bazaar` is too unconstrained.

It's also somewhat similar to the `Client` type from *pipes*, but it's also
a bit tricky to use that with a different effect type than the logic
`Traversable`, as well...so it has the same difference as `Bazaar` here.

But this type is common/simple enough that I'm sure someone has it somewhere
in a library that I haven't been able to find.  If you find it, let me know!

