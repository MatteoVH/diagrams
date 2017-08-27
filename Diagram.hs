{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import System.Random
import Control.Monad
import Diagrams.TwoD
import Diagrams.Prelude hiding (Point)
import Diagrams.Backend.SVG.CmdLine
import Data.List

type Point = P2 Double

generatePoint :: IO Point
generatePoint = do
  x <- randomIO :: IO Double
  y <- randomIO :: IO Double
  return (p2 (x, y))

cartesianProduct :: [a] -> [(a, a)]
cartesianProduct list =
  case list of
    [] -> []
    head : rest -> map (\ a -> (head, a)) rest ++ cartesianProduct rest

createLines :: (Point, Point) -> Diagram B
createLines (a, b) =
  fromVertices ([a, b])

main :: IO ()
main = do
  points <- replicateM 50 generatePoint
  mainWith ((mconcat (map createLines (cartesianProduct points))) # lw 0.1)
  putStrLn "use double quotes"
