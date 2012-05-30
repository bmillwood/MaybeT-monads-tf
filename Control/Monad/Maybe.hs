{-# LANGUAGE TypeFamilies #-}

{- |
Copyright    : 2007 Eric Kidd
License      : BSD3
Stability    : experimental
Portability  : non-portable (type families)

The 'MaybeT' monad.  See
<http://www.haskell.org/haskellwiki/New_monads/MaybeT> for more widely-used
version.  Our 'Functor' instance and our implementation of '>>=' are
borrowed from there.

[Computation type:] Computations which may fail or return nothing.

[Binding strategy:] Failure returns the value 'Nothing', bypassing any
bound functions which follow.  Success returns a value wrapped in 'Just'.

[Useful for:] Building computations from steps which may fail.  No error
information is returned.  (If error information is required, see
'Control.Monad.Error'.)

-}

module Control.Monad.Maybe (
  MaybeT(..)

  -- * Example
  -- $MaybeExample
  ) where

import Control.Monad.Trans()
import Control.Monad.Trans.Maybe
import Control.Monad.Fix

instance (MonadFix m) => MonadFix (MaybeT m) where
  -- Ref: http://www.haskell.org/pipermail/libraries/2011-April/016202.html
  mfix f = MaybeT (mfix (runMaybeT . f . unJust))
   where
    unJust (Just x) = x
    -- if f is strict then mfix produces bottom, so f must produce a
    -- constructor without inspecting its argument. if that's Nothing, f
    -- cannot inspect its argument at all; if it's Just, then the call to
    -- unJust is safe.
    unJust Nothing = error "MaybeT mfix: Nothing! (this should be impossible)"

{- $MaybeExample

Here is an example that shows how to use 'MaybeT' to propagate an
end-of-file condition in the IO monad.  In the example below, both
@maybeReadLine@ and @failIfQuit@ may cause a failure, which will propagate
out to @main@ without further intervention.

>import System.Console.Readline
>import Data.Maybe
>import Control.Monad
>import Control.Monad.Trans
>import Control.Monad.Maybe
>
>-- 'MaybeIO' is the type of computations which do IO, and which may fail.
>type MaybeIO = MaybeT IO
>
>-- 'readline' already has type 'String -> IO (Maybe String)'; we just need
>-- to wrap it.
>maybeReadLine :: String -> MaybeIO String
>maybeReadLine prompt = MaybeT (readline prompt)
>
>-- Fail if 'str' equals "quit".
>failIfQuit :: (Monad m) => String -> m ()
>failIfQuit str = when (str == "quit") (fail "Quitting")
>
>-- This task may fail in several places.  Try typing Control-D or "quit" at
>-- any prompt.
>concatTwoInputs :: MaybeIO ()
>concatTwoInputs = do
>  s1 <- maybeReadLine "String 1> "
>  failIfQuit s1
>  s2 <- maybeReadLine "String 2> "
>  failIfQuit s2
>  liftIO (putStrLn ("Concatenated: " ++ s1 ++ s2))
>
>-- Loop until failure.
>main :: IO ()
>main = do
>  result <- runMaybeT concatTwoInputs
>  if isNothing result
>    then putStrLn "Bye!"
>    else main

-}
