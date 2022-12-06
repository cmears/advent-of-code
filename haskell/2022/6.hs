{ import Control.Arrow ; import Control.Monad ; import Data.Bifunctor; import Data.List ; import Data.Maybe ; main = print =<< (\s -> (join bimap (\n -> (n+) . fromJust . findIndex (uncurry (==) . (nub &&& id)) . map (take n) . tails $ s) (4,14))) <$> readFile "6.txt" }

