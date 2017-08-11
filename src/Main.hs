module Main (main) where

import           Forward
import           Mat

main :: IO ()
main = do
    let vertices = pointsInPerspective 1 1 [(-5) .. 0]
    print $ map (transPipe makeView makePerspective) vertices
