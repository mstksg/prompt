prompt
=====

Monad and monad transformer for delayed-effect "pure" prompt-and-respose
queries.

Prompt
------

`Prompt a b r` represents a "pure" computation producing an `r` that can "ask" or
"prompt" with an `a` and get `b`'s as responses/answers.

By "pure", I mean that the actual eventual process of *answering* the prompts
might be effectful (it might involve IO, or state, or STM...like database
queries or prompts to a user).  However, we don't worry about those for now.
We **pretend that the prompting is pure**, and move on from there.

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
ghci> runPromptM promptFoo $ \prmpt -> do putStrLn prmpt; getLine
bar         -- stdout prompt
> hello!    -- stdin response typed in
baz         -- stdout prompt
> i am baz  -- stdin response typed in
Foo "hello!" 8  -- result
~~~

Now let's build one by asking for environment variables

~~~haskell
ghci> import System.Environment
ghci> setEnv "bar" "hello!"
ghci> setEnv "baz" "i am baz"
ghci> runPromptM promptFoo getEnv
Foo "hello!" 8
~~~

`promptFoo` is completely "pure" --- we *pretend* that `prompt "bar"` is pure,
because we don't ever assume IO or anything in the type.  So we can run
`promptFoo` in `IO` if we wanted, like above...or we can even run "without"
IO, too:

~~~haskell
ghci> import qualified Data.Map as M
ghci> let testMap = M.fromList [("bar", "hello!"), ("baz", "i am baz")]
ghci> runPrompt promptFoo (testMap M.!)
Foo "hello!" 8
~~~

PromptT
-------

`PromptT a b t r` allows your prompting-and-responding to take place in the context of
`Traversable` `t`, so you can do things like short-circuiting with `Either e`
or `Maybe`, or multiple branches for `[]`, etc --- all "purely", without
worrying about the eventual effects like IO.

~~~haskell
import Text.Read

promptFoo2 :: PromptT String String (Either Int) Foo
promptFoo2 = do
    bar <- prompt "bar"
    x   <- prompt "baz"
    lift $ readMaybe x
~~~

So now we have the ability to add pure short-circuiting.

~~~haskell
ghci> runPromptTM promptFoo2 (fmap Just . getEnv)
Nothing
ghci> runPromptTM (promptFoo2 <|> return (Foo "error" 0)) (fmap Just . getEnv)
Just (Foo "error" 0)
ghci> setEnv "baz" "19"
ghci> runPromptTM promptFoo2 (fmap Just . getEnv)
Just (Foo "hello!" 19)
~~~

~~~haskell
ghci> runPromptTM (promptFoo2 <|> promptFoo2) $ \prmpt -> putStrLn prmpt; Just <$> getLine
bar         -- stdout prompt
> hello!    -- stdin response typed in
baz         -- stdout prompt
> 19        -- stdin response typed in
Just (Foo "hello!" 19)  -- result
ghci> runPromptTM (promptFoo2 <|> promptFoo) $ \prmpt -> putStrLn prmpt; Just <$> getLine
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

For more advanced usage, there is a `MonadError` instance, so you can have
`PromptT a b (Either e) r` with things like `throwError` and `catchError` to
"catch" an error value and respond/recover.
