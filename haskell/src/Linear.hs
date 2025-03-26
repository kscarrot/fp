module Linear (
    Scalar(..),
    Vector,
    Matrix,
    标量加,
    标量乘,
    向量加,
    向量乘
) where

data Scalar = Str String
          | Num Double
          deriving (Show, Eq)

type Vector = [Scalar]
type Matrix = [Vector]

标量加 :: Scalar -> Scalar -> Scalar
标量加 (Num x) (Num y) = Num (x + y)
标量加 (Str "") (Str x) = Str x
标量加 (Str x) (Str "") = Str x
标量加 (Str x) (Str y) = Str (x ++ "+" ++ y)
标量加 _ _ = error "不支持的类型"

标量乘 :: Scalar -> Scalar -> Scalar
标量乘 (Num x) (Num y) = Num (x * y)
标量乘 (Str "") (Str _) = Str ""
标量乘 (Str _) (Str "") = Str ""
标量乘 (Str x) (Str y) = Str (x ++ y)
标量乘 _ _ = error "不支持的类型"

向量加 :: Vector -> Vector -> Vector
向量加 x y
    | length x /= length y = error "向量长度不匹配"
    | otherwise = zipWith 标量加 x y

向量乘 :: Vector -> Vector -> Scalar
向量乘 x y
    | length x /= length y = error "向量长度不匹配"
    | otherwise = foldr1 标量加 $ zipWith 标量乘 x y

