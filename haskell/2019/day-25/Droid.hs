import Data.Char
import IntCode

ord' = fromIntegral . ord
chr' = chr . fromIntegral

interactive :: String -> IO ()
interactive program = do
  let (status, outputs) = runProgram program []
  printOutput outputs
  loop status
  where
    loop (ExecutionWaiting f) = do
      command <- getLine
      let inputs = map ord' command ++ [10]
      let (status, outputs) = f inputs
      printOutput outputs
      loop status
    loop ExecutionFinished = pure ()

printOutput outputs = putStrLn $ map chr' outputs

main = interactive =<< readFile "input.txt"
  
-- prime number, asterisk, mutex, mug

