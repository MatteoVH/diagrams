{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Control.Monad
import           Data.List
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (Point)
import           Diagrams.TwoD
import           System.Random

type Point = P2 Double

generatePoint :: IO Point
generatePoint = do
  x <- randomIO :: IO Double
  y <- randomIO :: IO Double
  return (p2 (x, y))

cartesianProduct :: [a] -> [(a, a)]
cartesianProduct list =
  case list of
    []          -> []
    head : rest -> map (\ a -> (head, a)) rest ++ cartesianProduct rest

createLines :: (Point, Point) -> Diagram B
createLines (a, b) =
  fromVertices [a, b]

main :: IO ()
main = do
  points <- replicateM 50 generatePoint
  let lines = (centerXY ((mconcat (map createLines (cartesianProduct points))) # lw 0.1))
  mainWith (lines `atop` centerXY (square 1 # lw none # scale 1.25))
  putStrLn "use double quotes"
