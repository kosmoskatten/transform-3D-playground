module Main (main) where

import           Forward
import           Linear.Matrix ((!*!))
import           Mat

main :: IO ()
main = do
    let mvp = makePerspective !*! makeView
        vertices = pointsInPerspective 1 1 [(-5) .. 0]
    print $ map (transPipe mvp) vertices
