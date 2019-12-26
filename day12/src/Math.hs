module Math
  ( Vect3D(Vect3D)
  , manhattan
  ) where

newtype Vect3D =
  Vect3D (Int, Int, Int)
  deriving (Show, Eq, Ord)

instance Semigroup Vect3D where
  (Vect3D (x, y, z)) <> (Vect3D (x', y', z')) = Vect3D (x + x', y + y', z + z')

instance Monoid Vect3D where
  mempty = Vect3D (0, 0, 0)

manhattan :: Vect3D -> Int
manhattan (Vect3D (x, y, z)) = abs x + abs y + abs z
