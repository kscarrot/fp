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
    零矩阵,
    单位矩阵,
    行列式,
    余子式,
    伴随矩阵,
    逆矩阵,
  )
where

type Scalar = Double

type Vector = [Scalar]

type Matrix = [Vector]

零矩阵 :: Int -> Int -> Matrix
-- 零矩阵 = [[0 | _ <- [1..n]] | _ <- [1..m]]
零矩阵 n m = replicate n (replicate m 0)

单位矩阵 :: Int -> Matrix
单位矩阵 n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

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

移除指定行 :: Int -> Matrix -> Matrix
移除指定行 行 矩阵 = take 行 矩阵 ++ drop (行 + 1) 矩阵

移除指定列 :: Int -> Vector -> Vector
移除指定列 列 向量 = take 列 向量 ++ drop (列 + 1) 向量

余子式 :: Matrix -> Int -> Int -> Matrix
余子式 矩阵 行 列 = map (移除指定列 列) (移除指定行 行 矩阵)

行列式 :: Matrix -> Scalar
行列式 [] = 0
行列式 [[x]] = x
行列式 矩阵 = sum [((-1) ^ i) * (head 矩阵 !! i) * 行列式 (余子式 矩阵 0 i) | i <- [0 .. n - 1]]
  where
    n = length 矩阵

伴随矩阵 :: Matrix -> Matrix
伴随矩阵 矩阵 = 转置 [[行列式 (余子式 矩阵 i j) * ((-1) ^ (i + j)) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
  where
    n = length 矩阵

逆矩阵 :: Matrix -> Matrix
逆矩阵 矩阵 = (1 / 行列式 矩阵) `标量乘矩阵` 伴随矩阵 矩阵
