import Data.Graph.Inductive
import qualified Data.Map as M
import Data.List
import System.Random.Shuffle

-- Our graphs have string labels on the nodes, and integer (unit) weights on the edges
type G = Gr String Integer

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

main = do
  ls <- lines <$> readFile "25.txt"
  let es = [ (init x,y) | l <- ls, let (x:ys) = words l, y <- ys ]
  
  -- Convert node names (strings) into their integer identifies.
  let nodeNames = nub (map fst es ++ map snd es)
      nodeNamePairs = zip [0..] nodeNames
      nodeNameMap = M.fromList (map swap nodeNamePairs)
  let lnodes = nodeNamePairs
      ledges = map (\(x,y) -> (nodeNameMap M.! x, nodeNameMap M.! y, 1)) es
  let g :: G
      g = undir (mkGraph lnodes ledges)

  -- First, find two nodes that have a max-flow of 3 between them.
  -- The definition of the problem implies that there should be such a
  -- pair.
  --
  -- In other words, we're assuming there are two big subgraphs A and
  -- B connected by three edges, and we're trying to find a pair of
  -- nodes such that one is in A and the other is in B.  If A and B
  -- are of roughly equal size, about half of the node-pairs in the
  -- graph have this property so it shouldn't take long.

  -- One member of the pair can be arbitrarily chosen.
  let x = 0
  ys <- shuffleM (delete 0 (nodes g))
  let Just y = find (\y -> maxFlow g x y == 3) ys

  -- Now, we know the max-flow from x -> y is 3; let's get edges that
  -- are part of that max-flow.
  -- Only the edges with flow=1 are of interest.
  let mfg2 = efilter (\(_,_,(c,m)) -> c==1) (maxFlowgraph g x y)
  let candidateEdges = edges mfg2

  -- Three of these candidate edges are the critical ones.  An edge is
  -- critical if its deletion causes the max-flow from x -> y to
  -- reduce.

  -- ("take 3" here is just a performance improvement; there should
  -- always be exactly 3).
  let criticalEdges = take 3 $ filter (\e -> maxFlow (delEdge e g) x y < 3) candidateEdges
  -- Delete the edges (and their reverse-direction counterparts) from the graph.
  let sep = delEdges criticalEdges (delEdges (map swap criticalEdges) g)
  -- Find the connected components of the new graph (there should be
  -- two) and multiply their sizes.
  print $ product $ map length (components sep)
