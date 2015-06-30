module Types where

data V3 a = V3 a a a
instance (Show a) => Show (V3 a) where
  show (V3 a b c) = "(" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ ")"

data Triangle =
  Triangle {
    normal :: V3 Float,
    vertices :: V3 (V3 Float),
    attr :: Int
  }
instance Show Triangle where
  show (Triangle n vs a) = (show n) ++ "\n" ++ (show vs)

data STL =
  STL {
    header :: String,
    triCount :: Int,
    triangles :: [Triangle]
  }
instance Show STL where
  show (STL h c t) = "<STL: " ++ (show c) ++ " triangles, " ++ h ++ ">"

nullVector = V3 0 0 0
nullTriangle = Triangle nullVector (V3 nullVector nullVector nullVector) 0

