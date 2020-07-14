bounded-queue
=======================

Implementation of a bounded queue

```haskell
module Main where

import Data.BQueue
import Control.Monad.State

deq :: MonadState (BQueue a) m => m (Maybe a)
deq = state dequeue

enq :: MonadState (BQueue a) m => a -> m (Maybe a)
enq x = state (enqueue x)

main :: IO ()
main = do
  print =<< do
    flip execStateT (empty @Int 2) $ do
      enq 1
      enq 2
      enq 3
      deq
      enq 4
      enq 5
      deq
      deq
      deq
```