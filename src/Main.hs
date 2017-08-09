module Main (main) where

import           Linear.Matrix     (M44, (!*), (!*!))
import qualified Linear.Projection as Proj
import           Linear.V3         (V3 (..))
import           Linear.V4         (V4 (..), normalizePoint, point)

main :: IO ()
main = do
    let mvp = makePerspective !*! makeView
        vertices = pointsInPerspective 1 1 [(-10) .. 10]
    print $ map (transPipe mvp) vertices

-- | Construct a simple perspective.
makePerspective :: M44 Float
makePerspective =
    Proj.perspective (pi / 4) (800 / 600) 0.1 100

makeView :: M44 Float
makeView =
    Proj.lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)

toClipSpace :: M44 Float -> V3 Float -> V4 Float
toClipSpace mvp pos = mvp !* point pos

pointsInPerspective :: Float -> Float -> [Int] -> [V3 Float]
pointsInPerspective x y = map (V3 x y . fromIntegral)

transPipe :: M44 Float -> V3 Float -> V3 Float
transPipe mvp = normalizePoint . toClipSpace mvp
