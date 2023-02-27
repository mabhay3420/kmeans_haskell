module Vector(Vector(..), vecDist, vecSum, scalarMul) where
newtype Vector a = Vec [a] deriving (Show, Eq)

cords :: Vector a -> [a]
cords (Vec x) = x

vecDist :: RealFloat a => Vector a -> Vector a -> a
vecDist (Vec xs) (Vec ys) | length xs /= length ys = error "Distance calculation can be done only on vectors of same length"
vecDist (Vec xs) (Vec ys) = sqrt (sum ([(x-y) * (x-y) | (x,y) <- zip xs ys]))

vecSum :: RealFloat a => [Vector a] -> Vector a
vecSum []  = Vec []
vecSum ((Vec xs) : vecs) | null vecs =  Vec xs
vecSum ((Vec xs) : vecs) =  Vec [x + y | (x,y) <- zip xs ys]
                            where ys = cords (vecSum vecs)


scalarMul :: RealFloat a => Vector a -> a -> Vector a
scalarMul (Vec xs) y = Vec (map (*y) xs)
