module Main (main) where

import Common.Err
import Common.Result
import Machines

main :: IO ()
main = run >>= handleResult

-- Run in try block, converts IO error to 'Result ()'
run :: IO (Result ())
run = try runProgram

runProgram :: IO ()
runProgram = do
  let ms = getMachines
  let m1 = getMachine ms DevelopmentMachine
  let m2 = getMachine ms BuildMachine
  let m3 = getMachine ms TestMachine
  putStrLn (show m1)
  putStrLn (show m2)
  putStrLn (show m3)

handleResult :: Result () -> IO ()
handleResult (Left err) = putStrLn (show err)
handleResult (Right _) = return ()
