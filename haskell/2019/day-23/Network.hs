import Data.List.Split
import qualified Data.Map as M
import IntCode

type Queues = M.Map Integer [Integer]
type Computers = M.Map Integer ExecutionStatus
type S = (Computers, Queues)

main = do
  program <- readFile "input.txt"
  -- Initialise computers
  let computers = M.fromList $ [ (address, discardOutput (runProgram program [address])) | address <- [0..49] ]
      queues = M.fromList $ zip [0..49] (repeat [])

  let loop s = do
          let t1 = length (concat [ snd s  M.! address | address <- [0..49] ])
              s' = advanceAll s
              t2 = length (concat [ snd s' M.! address | address <- [0..49] ])
          if t1 == 0 && t2 == 0
          then do let [x,y] = reverse . take 2 . reverse $ snd s' M.! 255
                  putStrLn $ "idle; sending " ++ show [x,y]
                  let queues' = M.insert 0 [x,y] (snd s')
                  loop (fst s', queues')
          else loop s'
  
  loop (computers, queues)

  pure ()

discardOutput (status, outputs) =
  if null outputs
  then status
  else error "discarded some output"

-- Run one computer until it blocks waiting on input.
advance :: Integer -> S -> S
advance address (computers, queues) =
    let ExecutionWaiting computer = computers M.! address
        inbox = queues M.! address
        inputs = if null inbox then [-1] else inbox
        (status, outputs) = computer inputs
        messages = chunksOf 3 outputs
        queues' = M.insert address [] queues
        queues'' = foldl (\qs [a,x,y] -> M.insertWith (flip (++)) a [x,y] qs) queues' messages
        computers' = M.insert address status computers
    in (computers', queues'')

-- Run each computer once in turn.
advanceAll :: S -> S
advanceAll s = foldl (flip advance) s [0..49]
