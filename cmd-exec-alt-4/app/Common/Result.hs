module Common.Result (Result, pureResultToIO) where

import Common.Err

type Result a = Either Error a

-- public

-- | Wrap Result to IO with possibility of throwing an Exception
pureResultToIO :: Result a -> IO a
pureResultToIO (Left e) = throwIO e
pureResultToIO (Right r) = return r
