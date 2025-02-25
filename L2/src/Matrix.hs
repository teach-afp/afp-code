-- | Simple 2D matrix algebra.

module Matrix
  ( -- * Types
    Angle, Vec, Point, Matrix
  , -- * Run functions
    theAngle, vecX, vecY, ptX, ptY
  , -- * Constructors
    angle, vec, point, matrix, diagonalMatrix
  , -- * Combinators
    inner, mul, inv, sub, scaleMatrix, determinant
  ) where

-- * Types

-- | Abstract type of angles.
newtype Angle = A
  { theAngle :: Double  -- ^ Angle in radian.
  }

-- | Abstract type of 2D vectors.
data Vec = V
  { vecX :: Double  -- ^ First component of 2D vector.
  , vecY :: Double  -- ^ Second component of 2D vector.
  }

-- | Abstract type of 2D points.
data Point = P
  { ptX  :: Double -- ^ X coordinate (cartesian) of 2D point.
  , ptY  :: Double -- ^ Y coordinate (cartesian) of 2D point.
  }

-- | Abstract type of 2x2 matrices.
data Matrix = M { m00, m01, m10, m11 :: Double }

-- * Constructors

-- | Angle creation.
angle :: Double -> Angle
angle = A

-- | Vector creation.
vec :: Double -> Double -> Vec
vec = V

-- | Point creation.
point :: Double -> Double -> Point
point = P

-- | Matrix creation.
matrix :: Double -> Double -> Double -> Double -> Matrix
matrix = M

-- | Diagonal matrix creation.
diagonalMatrix :: Vec -> Matrix
diagonalMatrix (V x y) = M
  x 0
  0 y

-- * Combinators

-- | Inner product.
inner :: Point -> Point -> Double
inner (P a b) (P c d) = a * c + b * d

-- | Transforming a point by matrix multiplication.
mul :: Matrix -> Point -> Point
mul (M x0 x1 y0 y1) p = P (inner (P x0 x1) p) (inner (P y0 y1) p)

-- | Matrix scaling.
scaleMatrix :: Double -> Matrix -> Matrix
scaleMatrix k (M a b c d) = M (k * a) (k * b) (k * c) (k * d)

-- | Determinant of a 2x2 matrix.
determinant :: Matrix -> Double
determinant (M a b c d) = a * d - b * c

-- | Matrix inversion.
inv :: Matrix -> Matrix
inv m@(M a b c d) = scaleMatrix (1 / determinant m) $ M d (-b) (-c) a

-- | Subtraction.
sub :: Point -> Vec -> Point
sub (P x y) (V dx dy) = P (x - dx) (y - dy)
