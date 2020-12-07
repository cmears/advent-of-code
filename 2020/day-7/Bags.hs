import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed
import qualified Data.Graph.Inductive as G
import Data.List
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- Data representation
----------------------

-- e.g. Colour "light gold"
data Colour = Colour String
  deriving (Eq, Ord, Show)

-- e.g. light gold bags contain 2 light lime bags, 1 faded green bag.
-- Contains (Colour "light gold") [(2, Colour "light lime"), (1, Colour "faded green")]
type Quantity = (Int, Colour)
data Rule = Contains Colour [Quantity]
  deriving (Show)

-- Parsing
----------

type Parser = Parsec Void String

space = L.space C.space1 empty empty
symbol = L.symbol space
lexeme = L.lexeme space
decimal = lexeme L.decimal

rule :: Parser Rule
rule = Contains <$> (colour <* bags <* contain) <*> contents <* fullstop
  where
    bags = symbol "bags"
    contain = symbol "contain"
    fullstop = symbol "."
    comma = symbol ","
    contents = (symbol "no other bags" *> pure []) <|> sepBy quantity comma

quantity :: Parser Quantity
quantity = (,) <$> decimal <*> colour <* (symbol "bags" <|> symbol "bag")

colour :: Parser Colour
colour = do
  a <- lexeme (many C.letterChar)
  b <- lexeme (many C.letterChar)
  pure (Colour (a ++ " " ++ b))

-- Graph construction
---------------------

-- Build a graph from the rules.
--   Each colour is a node.
--   Each rule has several edges, from the "containing" colour to the "contained" colours.
--   Each edge is labelled with the quantity.
-- E.g. the rule "muted beige bags contain 3 clear lime bags, 5 dark salmon bags, 1 pale olive bag."
--   becomes three labelled edges:
--     muted beige ->(3) clear lime
--     muted beige ->(5) dark salmon
--     muted beige ->(1) pale olive
constructGraph :: [Rule] -> (G.NodeMap Colour, G.Gr Colour Int)
constructGraph rules =
  let colours = nub (concat [ (c:map snd qs) | Contains c qs <- rules ])
      (lnodes, nodemap) = G.mkNodes G.new colours
      edges = [ (c, c2, n) | Contains c qs <- rules, (n,c2) <- qs ]
      Just ledges = G.mkEdges nodemap edges
  in (nodemap, G.mkGraph lnodes ledges)

main = do
  c <- readFile "input.txt"
  case parse (many rule) "" c of
    Left e -> putStrLn (errorBundlePretty e)
    Right rules -> do
      let (nm,g) = constructGraph rules

      -- Part 1
      -- Which node number is "shiny gold"?
      let shinyGoldNode = fst (G.mkNode_ nm (Colour "shiny gold"))
      -- Which nodes eventually lead into shiny gold?
      -- This is exactly the reachable nodes *from* shiny gold in the *reversed* graph.
      let ancestors = G.reachable shinyGoldNode (G.grev g)
      -- We subtract one because shiny gold is reachable from itself
      -- but we don't want to count it.
      print (length ancestors - 1)

      -- Part 2
      -- Let a[i] be the number of bags you'd need in total if you had a bag of type i.
      --   This includes the bag of type i itself.
      --   (where "i" is the node number of the bag type)
      let (minNode, maxNode) = G.nodeRange g
      let a :: Array Int Int
          a = array (minNode, maxNode) $ do
                i <- [minNode..maxNode]
                let e = case G.match i g of
                          (Just (_, _, _, out), _) -> 1 + sum [ n * (a!j) | (n, j) <- out ]
                          _ -> error "?"
                pure (i,e)
      -- How many for shiny gold?
      -- (Again we subtract one to exclude the shiny gold bag itself.)
      print (a!shinyGoldNode - 1)
