module Linear

type 标量 =
    | 字符串 of string
    | 数字 of float


type 向量 = 标量 array
type 矩阵 = 向量 array


let 标量加 (x: 标量) (y: 标量) =
    match (x, y) with
    | 数字 x, 数字 y -> 数字 (x + y)
    | 字符串 "", 字符串 x
    | 字符串 x, 字符串 "" -> 字符串 x
    | 字符串 x, 字符串 y -> 字符串 $"{x}+{y}"
    | _ -> failwith "不支持的类型"


let 标量乘 (x: 标量) (y: 标量) =
    match (x, y) with
    | 数字 x, 数字 y -> 数字 (x * y)
    | 字符串 "", 字符串 _
    | 字符串 _, 字符串 "" -> 字符串 ""
    | 字符串 x, 字符串 y -> 字符串 $"{x}{y}"
    | _ -> failwith "不支持的类型"



let 向量加 (x: 向量) (y: 向量) = Array.map2 标量加 x y


let 向量乘 (x: 向量) (y: 向量) = Array.map2 标量乘 x y |> Array.reduce 标量加


let 标量乘向量 (x: 标量) (y: 向量) = y |> Array.map (标量乘 x)


let 转置 (m: 矩阵) = Array.transpose m

let 矩阵乘 (ma: 矩阵) (mb: 矩阵) =
    ma |> Array.map (fun 行 -> mb |> 转置 |> Array.map (fun 列 -> 行 |> 向量乘 <| 列))
