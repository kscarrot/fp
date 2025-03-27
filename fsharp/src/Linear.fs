module Linear

type 标量 =
    | 字符串 of string
    | 数字 of float

type 向量 = 标量 array
type 矩阵 = 向量 array

type 环元 =
    | 标量 of 标量
    | 向量 of 向量
    | 矩阵 of 矩阵

let 标量加 (x: 标量) (y: 标量) =
    match x, y with
    | 数字 x, 数字 y -> 数字 (x + y)
    | 字符串 "", 字符串 x
    | 字符串 x, 字符串 "" -> 字符串 x
    | 字符串 x, 字符串 y -> 字符串 $"{x}+{y}"
    | _ -> failwith "不支持的类型"


let 标量乘 (x: 标量) (y: 标量) =
    match x, y with
    | 数字 x, 数字 y -> 数字 (x * y)
    | 字符串 "", 字符串 _
    | 字符串 _, 字符串 "" -> 字符串 ""
    | 字符串 x, 字符串 y -> 字符串 $"{x}{y}"
    | _ -> failwith "不支持的类型"



let 向量加 (vx: 向量) (vy: 向量) = Array.map2 标量加 vx vy


let 向量乘 (vx: 向量) (vy: 向量) =
    Array.map2 标量乘 vx vy |> Array.reduce 标量加


let 标量乘向量 (x: 标量) (vy: 向量) =
    vy |> Array.map (fun y -> x |> 标量乘 <| y)


let 转置 (m: 矩阵) = Array.transpose m

let 矩阵乘 (mx: 矩阵) (my: 矩阵) =
    mx |> Array.map (fun 行 -> 转置 my |> Array.map (fun 列 -> 行 |> 向量乘 <| 列))


let 标量乘矩阵 (x: 标量) (m: 矩阵) = m |> Array.map (标量乘向量 x)

let 矩阵加 (ma: 矩阵) (mb: 矩阵) = Array.map2 向量加 ma mb

let 向量乘矩阵 (m: 矩阵) (vx: 向量) =
    m |> Array.map (fun 行 -> 行 |> 向量乘 <| vx)


let 乘 (x: 环元) (y: 环元) : 环元 =
    match x, y with
    | 标量 s1, 标量 s2 -> 标量 (s1 |> 标量乘 <| s2)
    | 标量 s, 向量 v -> 向量 (s |> 标量乘向量 <| v)
    | 标量 s, 矩阵 m -> 矩阵 (s |> 标量乘矩阵 <| m)
    | 向量 v, 标量 s -> 向量 (s |> 标量乘向量 <| v)
    | 向量 v1, 向量 v2 -> 标量 (v1 |> 向量乘 <| v2)
    | 向量 v, 矩阵 m -> 向量 (m |> 向量乘矩阵 <| v)
    | 矩阵 m, 标量 s -> 矩阵 (s |> 标量乘矩阵 <| m)
    | 矩阵 m, 向量 v -> 向量 (m |> 向量乘矩阵 <| v)
    | 矩阵 m1, 矩阵 m2 -> 矩阵 (m1 |> 矩阵乘 <| m2)

//TODO: 封装加法

//TODO: latex 输出

//TODO: 行列式

//TODO: 逆矩阵

//TODO: 特征值

//TODO: EVD 特征分解

//TODO: SVD 奇异值分解

//TODO: LU 分解

//TODO: QR 分解
