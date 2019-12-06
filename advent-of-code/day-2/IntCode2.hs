import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import System.Environment
import System.IO
import Text.Printf

main = do
  c <- readFile "input"
  [a,b] <- getArgs
  let numbers = unfoldr (\s -> case span (/= ',') s of
                                 ("", _) -> Nothing
                                 (x,rest) -> Just (read x, drop 1 rest)) c
  print (numbers :: [Integer])

  let initialState = M.fromList (zip [0..] numbers)

  -- forM_ [12..12] $ \a -> do
  --   forM_ [2..2] $ \b -> do
  let (e,finalState) = execute ((M.insert 1 (read a) . M.insert 2 (read b)) initialState) 0
  putStrLn (unlines e)
  print (a,b,finalState M.! 0)

--target 19690720
-- 19690720 = 248832 * 77 + 530607 + 49
--                     ^^            ^^

--execute m i | trace (show (i,m)) False = undefined
execute m i =
    let opcode = m M.! i
    in case opcode of
         99 -> ([], m)
         _ ->
             let loc1 = m M.! (i+1)
                 loc2 = m M.! (i+2)
                 loc3 = m M.! (i+3)
                 arg1 = m M.! loc1
                 arg2 = m M.! loc2
                 op = case opcode of
                        1 -> (+)
                        2 -> (*)
                 result = op arg1 arg2
                 m' = M.insert loc3 result m
--                 explanation = printf "[%d]:  [%d] = [%d] %s [%d]" i loc3 loc1 (if opcode == 1 then "+" else "*") loc2
                 explanation = printf "[%d]:  [%d](%d) = %d %s %d" i loc3 result arg1 (if opcode == 1 then "+" else "*") arg2
                 (ef,mf) = execute m' (i+4)
             in (explanation:ef, mf)




-- (((((((x * 3 + 3) * 3 + 9) * 3 + 2) * 2 + 1) * 4 + 6) * 2 + 5) * 16 + 3) * 36 + 3 + y

-- (x * 248832 + 530607) + y


-- [0]:  [3](3) = 1 + 2
-- [4]:  [3](14) = 12 + 2
-- [8]:  [3](15) = 14 + 1
-- [12]:  [3](2) = 1 + 1
-- [16]:  [19](36) = 3 * 12
-- [20]:  [23](37) = 36 + 1
-- [24]:  [27](39) = 37 + 2
-- [28]:  [31](117) = 3 * 39
-- [32]:  [35](118) = 1 + 117
-- [36]:  [39](122) = 118 + 4
-- [40]:  [43](126) = 122 + 4
-- [44]:  [47](378) = 126 * 3
-- [48]:  [51](380) = 2 + 378
-- [52]:  [55](760) = 380 * 2
-- [56]:  [59](761) = 1 + 760
-- [60]:  [63](3044) = 761 * 4
-- [64]:  [67](3047) = 3 + 3044
-- [68]:  [71](3050) = 3 + 3047
-- [72]:  [75](6100) = 3050 * 2
-- [76]:  [79](6101) = 1 + 6100
-- [80]:  [83](6102) = 1 + 6101
-- [84]:  [87](6105) = 3 + 6102
-- [88]:  [91](24420) = 6105 * 4
-- [92]:  [95](97680) = 4 * 24420
-- [96]:  [99](97683) = 97680 + 3
-- [100]:  [103](293049) = 97683 * 3
-- [104]:  [107](1172196) = 4 * 293049
-- [108]:  [111](3516588) = 3 * 1172196
-- [112]:  [115](3516589) = 3516588 + 1
-- [116]:  [119](3516591) = 3516589 + 2
-- [120]:  [0](3516593) = 3516591 + 2
