import Control.Concurrent (forkIO)
#if defined(STRICT)
import Control.Concurrent.Chan.Strict
#else
import Control.Concurrent.Chan
#endif
import System.Environment

-- Fork some computation processes, print their results
main = do
    n <- getArgs >>= readIO . head
    f1 <- run fibonacci
    f2 <- run fibonacci2
    mapM_ print . take n $ zip f1 f2

  -- fork a process, return any messages it produces as a list
  where
    run f = do
        c <- newChan
        l <- getChanContents c
        forkIO (writeList2Chan c f)
        return l

--
-- very computationally expensive jobs:

fibonacci = map fib [0..]

fibonacci2 = map fib [1..] -- to defeat CSE

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
