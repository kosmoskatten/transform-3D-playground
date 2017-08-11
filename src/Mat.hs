module Mat
    ( M44
    , makePerspective
    , makeView
    , makeRotate
    , normalMatrix
    , identity
    ) where

import           Linear.Matrix     (M44, fromQuaternion, identity, inv44,
                                    m33_to_m44, transpose)
import qualified Linear.Projection as Proj
import           Linear.Quaternion (axisAngle)
import           Linear.V3         (V3 (..))

-- | Construct a perspective matrix.
makePerspective :: M44 Float
makePerspective =
    Proj.perspective (pi / 4) (800 / 600) 0.1 100

-- | Construct a view matrix.
makeView :: M44 Float
makeView =
    Proj.lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)

makeRotate :: V3 Float -> Float -> M44 Float
makeRotate v = m33_to_m44 . fromQuaternion . axisAngle v

-- | Make a normal matrix.
normalMatrix :: M44 Float -> M44 Float
normalMatrix = transpose . inv44
