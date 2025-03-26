module Program

type 标量 =
    | 字符串 of string
    | 数字 of float
type 向量 = 标量 array
type 矩阵 = 向量 array
let 加 (x: 标量) (y: 标量) =
    match (x, y) with
    | (数字 x, 数字 y) -> 数字 (x + y)
    | (字符串 "", 字符串 x)
    | (字符串 x, 字符串 "") -> 字符串 x
    | (字符串 x, 字符串 y) -> 字符串 $"{x}+{y}"
    | _ -> failwith "不支持的类型"


let 乘 (x: 标量) (y: 标量) =
    match (x, y) with
    | (数字 x, 数字 y) -> 数字 (x * y)
    | (字符串 "", 字符串 x)
    | (字符串 x, 字符串 "") -> 字符串 ""
    | (字符串 x, 字符串 y) -> 字符串 $"{x}{y}"
    | _ -> failwith "不支持的类型"




let 向量加 (x: 向量) (y: 向量) =
    if x.Length <> y.Length then
        failwith "向量长度不匹配"
    Array.map2 加 x y


let 向量乘 (x: 向量) (y: 向量) =
    if x.Length <> y.Length then
        failwith "向量长度不匹配"
    Array.map2 乘 x y |> Array.reduce 加

