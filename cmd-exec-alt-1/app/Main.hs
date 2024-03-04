module Main (main) where

import Common.Err
import Common.Result

main :: IO ()
main = run >>= handleResult

-- Run in try block, converts IO error to 'Result ()'
run :: IO (Result ())
run = try runProgram

runProgram :: IO ()
runProgram = undefined

handleResult :: Result () -> IO ()
handleResult (Left err) = putStrLn (show err)
handleResult (Right _) = return ()
