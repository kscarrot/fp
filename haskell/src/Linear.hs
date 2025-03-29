module Linear
  ( Scalar,
    Vector,
    Matrix,
    标量加,
    向量加,
    矩阵加,
    转置,
    标量乘,
    标量乘向量,
    标量乘矩阵,
    向量乘,
    向量乘矩阵,
    矩阵乘向量,
    矩阵乘,
    矩阵乘PointFree,
  )
where

type Scalar = Double

type Vector = [Scalar]

type Matrix = [Vector]

标量加 :: Scalar -> Scalar -> Scalar
标量加 x y = x + y

向量加 :: Vector -> Vector -> Vector
向量加 = zipWith (+)

矩阵加 :: Matrix -> Matrix -> Matrix
矩阵加 = zipWith $ zipWith (+)

转置 :: Matrix -> Matrix
转置 [] = []
转置 ([] : _) = []
转置 m = map head m : 转置 (map tail m)

标量乘 :: Scalar -> Scalar -> Scalar
标量乘 x y = x * y

标量乘向量 :: Scalar -> Vector -> Vector
标量乘向量 = map . 标量乘

标量乘矩阵 :: Scalar -> Matrix -> Matrix
标量乘矩阵 = map . map . (*)

向量乘 :: Vector -> Vector -> Scalar
向量乘 = (sum .) . zipWith (*)


向量乘矩阵 :: Vector -> Matrix -> Vector
向量乘矩阵 向量 矩阵 = [向量 `向量乘` 列 | 列 <- 转置 矩阵]

矩阵乘向量 :: Matrix -> Vector -> Vector
矩阵乘向量 矩阵 向量 = [行 `向量乘` 向量 | 行 <- 矩阵]

矩阵乘 :: Matrix -> Matrix -> Matrix
矩阵乘 ma mb = [[行 `向量乘` 列 | 列 <- 转置 mb] | 行 <- ma]

-- 矩阵乘 PointFree 形式比较抽象 需要要flip置换顺序
-- 矩阵乘PointFree ma mb 
--             = map (\row -> map (\col -> 向量乘 row col) (转置 mb)) ma
--             = map (\row -> map (向量乘 row) (转置 mb)) ma
--             ≡ flip (map (flip (map . 向量乘) (转置 mb))) ma
--             ≡ flip ((map . flip (map . 向量乘) . 转置) mb) ma
--             ≡ flip ( (map . flip (map . 向量乘)) . 转置 ) ma mb
矩阵乘PointFree :: Matrix -> Matrix -> Matrix
矩阵乘PointFree = flip $ map . flip (map . 向量乘) . 转置