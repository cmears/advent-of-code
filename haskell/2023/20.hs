import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Debug.Trace
import Text.Printf

data System = System {
      outputs :: Map String [String]
    , inputs :: Map String [String]
    , kinds :: Map String Kind
    }
  deriving (Show)

data Kind = Broadcaster | FlipFlop | Conjunction
  deriving (Show)

parseLine :: String -> (String, Kind, [String])
parseLine l =
    let (a:_:cs) = words l
        (name, kind) =
            case head a of
              '%' -> (tail a, FlipFlop)
              '&' -> (tail a, Conjunction)
              _ -> (a, Broadcaster)
        destinations = map (filter (/=',')) cs
    in (name, kind, destinations)

updateSystem :: System -> (String, Kind, [String]) -> System
updateSystem (System outs ins ks) (name,kind,dests) =
  System {
  outputs = M.insert name dests outs
, inputs = foldl (\o d -> let i = M.findWithDefault [] d ins
                          in M.insert d (name:i) o) ins dests
, kinds = M.insert name kind ks
}

type Memory = Map String Mem
data Mem = FlipFlopMem Bool | ConjMem (Map String Bool)
  deriving (Show)
type Pulse = (String, Bool, String)

execute :: System -> [Pulse] -> Memory -> (Memory, [Pulse])
execute system pulses mem = loop pulses mem []
  where
    loop :: [Pulse] -> Memory -> [Pulse] -> (Memory, [Pulse])
--    loop ps m record | trace (show (ps, m, record)) False = undefined
    loop [] m record = (m, reverse record)
    loop ((src,p,dest):ps) m record =
        let record' = ((src,p,dest):record)
        in case M.lookup dest (kinds system) of
          Just Broadcaster -> let newPulses = [(dest, p, x) | x <- outputs system M.! dest]
                              in loop (ps ++ newPulses) m record'
          Just FlipFlop -> case p of
                             True -> loop ps m record'
                             False -> let FlipFlopMem oldValue = m M.! dest
                                          newValue = not oldValue
                                          m' = M.insert dest (FlipFlopMem newValue) m
                                          newPulses = [(dest, newValue, x) | x <- outputs system M.! dest]
                                      in loop (ps ++ newPulses) m' record'
          Just Conjunction -> let ConjMem cm = m M.! dest
                                  cm' = M.insert src p cm
                                  m' = M.insert dest (ConjMem cm') m
                                  allHigh = all (\s -> M.findWithDefault False s cm') (inputs system M.! dest)
                                  newPulses = [(dest, not allHigh, x) | x <- outputs system M.! dest]
                              in loop (ps ++ newPulses) m' record'
          Nothing -> loop ps m record'
            
initialiseMem sys = M.fromList $ do
                      (name,k) <- M.toList (kinds sys)
                      v <- case k of
                             FlipFlop -> pure $ FlipFlopMem False
                             Conjunction -> pure $ ConjMem M.empty
                             Broadcaster -> []
                      pure (name, v)

main = do
  ls <- lines <$> readFile "20.txt"
  let parsed = map parseLine ls
  let sys = foldl updateSystem (System M.empty M.empty M.empty) parsed
  -- print sys
  
  -- let (m',one) = execute sys [("button", False, "broadcaster")] (initialiseMem sys)
  --     (m'',two) = execute sys [("button", False, "broadcaster")] m'
  --     (m''',three) = execute sys [("button", False, "broadcaster")] m''
  -- print one
  -- print two
  -- print three

  let pushButton :: Memory -> (Memory, [Pulse])
      pushButton m = execute sys [("button", False, "broadcaster")] m
      pushMany :: Memory -> [(Memory, [Pulse])]
      pushMany m = unfoldr (\mm -> let (m',ps) = pushButton mm in Just ((m',ps),m')) m

  let x = pushMany (initialiseMem sys)
--  print $ head x

  let firstk = take 1000 x
      ps = concatMap snd firstk
      (h,l) = partition (\(_,b,_) -> b) ps
  print (length l,length h)
  print $ length l * length h


-- --  let Just i = findIndex (\(_,ps) -> find (
--   flip mapM_ (zip [1..] x) $ \(i,(_,ps)) -> do
--     let rx = filter (\(src,b,dest) -> dest=="rx") ps
--     case length rx of
--       1 -> printf "%4d %s\n" (i::Int) (show rx)
--       _ -> pure ()

  let loop :: Integer -> Memory -> IO ()
      loop i m = do
          let (m',ps) = pushButton m
              rx = filter (\(src,b,dest) -> dest=="rx") ps
--          printf "%4d %s\n" i (show ([let FlipFlopMem b = m' M.! n in fromEnum b | n <- ["kr", "hh", "dh", "kq", "lm", "hn", "qk", "cb", "hf", "ch", "kd", "nb"]]))
--          printf "%4d %s\n" i (show ([let FlipFlopMem b = m' M.! n in fromEnum b | n <- ["kr", "hh", "dh", "kq", "lm", "hn", "qk", "cb", "hf", "ch", "kd", "nb"]]))         
--          let interesting = filter (\(src,b,dest) -> b == True && dest == "bn") ps
          let interesting = filter (\(src,b,dest) -> b == False && dest == "lz") ps
          when (not (null interesting)) $ printf "%4d %s\n" i (show interesting)
          case length rx of
            1 -> printf "%4d %s\n" (i::Integer) (show rx)
--          _ | i `mod` 10000 == 0 -> (printf "%4d %s\n" i (show m')) >> loop (i+1) m'
            _ -> loop (i+1) m'
  loop 1 (initialiseMem sys)

