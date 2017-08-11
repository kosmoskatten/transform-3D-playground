module Normal
    ( transScaleMat
    , mulAsPoint
    , mulAsVector
    ) where

import           Linear.Matrix ((!*))
import           Linear.V3     (V3 (..))
import           Linear.V4     (V4 (..), point, vector)
import           Mat

transScaleMat :: M44 Float
transScaleMat =
    V4 (V4 3 0 0 1)
       (V4 0 3 0 2)
       (V4 0 0 3 3)
       (V4 0 0 0 1)

mulAsPoint :: M44 Float -> V3 Float -> V3 Float
mulAsPoint mat p = toV3 $  mat !* point p

mulAsVector :: M44 Float -> V3 Float -> V3 Float
mulAsVector mat v = toV3 $ mat !* vector v

toV3 :: V4 Float -> V3 Float
toV3 (V4 x y z _w) = V3 x y z
