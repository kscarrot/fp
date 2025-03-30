module Linear

type 标量 = float
type 向量 = 标量 array
type 矩阵 = 标量 array array


let 标量加 (x: 标量) (y: 标量) = x + y
let 向量加 (vx: 向量) (vy: 向量) = Array.map2 标量加 vx vy
let 矩阵加 (ma: 矩阵) (mb: 矩阵) = Array.map2 向量加 ma mb

let 转置 (m: 矩阵) = Array.transpose m

let 标量乘 (x: 标量) (y: 标量) = x * y

let 向量乘 (vx: 向量) (vy: 向量) =
    Array.map2 标量乘 vx vy |> Array.reduce 标量加

let 标量乘向量 (x: 标量) (vy: 向量) = Array.map (标量乘 x) vy
let 标量乘矩阵 (x: 标量) (m: 矩阵) = m |> Array.map (标量乘向量 x)

let 矩阵乘向量 (mx: 矩阵) (vy: 向量) =
    mx |> Array.map (fun 行 -> 行 |> 向量乘 <| vy)


let 向量乘矩阵 (vx: 向量) (my: 矩阵) =
    转置 my |> Array.map (fun 列 -> 列 |> 向量乘 <| vx)

let 矩阵乘 (mx: 矩阵) (my: 矩阵) =
    mx |> Array.map (fun 行 -> 转置 my |> Array.map (fun 列 -> 行 |> 向量乘 <| 列))

let 单位矩阵 (n: int) =
    Array.init n (fun i -> Array.init n (fun j -> if i = j then 1.0 else 0.0))

let 零矩阵 (n: int) (m: int) =
    Array.init n (fun _ -> Array.init m (fun _ -> 0.0))




let 矩阵相等 (arr1: 矩阵) (arr2: 矩阵) =
    // 检查外层数组长度
    if arr1.Length <> arr2.Length then
        false
    else
        // 逐行比较元素
        Array.forall2
            (fun row1 row2 ->
                if Array.length row1 <> Array.length row2 then
                    false
                else
                    Array.forall2 (=) row1 row2)
            arr1
            arr2

type 环元 =
    | 标量 of 标量
    | 向量 of 向量
    | 矩阵 of 矩阵
    static member 乘 (x: 标量, y: 标量) : 标量 = 标量乘 x y
    static member 乘 (x: 标量, y: 向量) : 向量 = 标量乘向量 x y
    static member 乘 (x: 标量, y: 矩阵) : 矩阵 = 标量乘矩阵 x y

    static member 乘 (x: 向量, y: 标量) : 向量 = 标量乘向量 y x
    static member 乘 (x: 向量, y: 向量) : 标量 = 向量乘 x y
    static member 乘 (x: 向量, y: 矩阵) : 向量 = 向量乘矩阵 x y

    static member 乘 (x: 矩阵, y: 标量) : 矩阵 = 标量乘矩阵 y x
    static member 乘 (x: 矩阵, y: 向量) : 向量 = 矩阵乘向量 x y
    static member 乘 (x: 矩阵, y: 矩阵) : 矩阵 = 矩阵乘 x y


// let inline (<*>) x y =
//     match (x, y) with
//     | 标量 x, 标量 y -> 标量 (x * y)
//     | 标量 x, 向量 y -> 向量 (标量乘向量 x y)
//     | 标量 x, 矩阵 y -> 矩阵 (标量乘矩阵 x y)
//     | 向量 x, 标量 y -> 向量 (标量乘向量 y x)
//     | 向量 x, 向量 y -> 标量 (向量乘 x y)
//     | 向量 x, 矩阵 y -> 向量 (向量乘矩阵 x y)
//     | 矩阵 x, 标量 y -> 矩阵 (标量乘矩阵 y x)
//     | 矩阵 x, 向量 y -> 向量 (矩阵乘向量 x y)
//     | 矩阵 x, 矩阵 y -> 矩阵 (矩阵乘 x y)

// let inline (<*>) (x: 标量) (y: 标量) = 标量乘 x y
// let inline (<*>) (x: 标量) (y: 向量) = 标量乘向量 x y
// let inline (<*>) (x: 标量) (y: 矩阵) = 标量乘矩阵 x y
// let inline (<*>) (x: 向量) (y: 矩阵) = 向量乘矩阵 x y
// let inline (<*>) (x: 向量) (y: 向量) = 向量乘 x y
// let inline (<*>) (x: 向量) (y: 标量) = 标量乘向量 y x
// let inline (<*>) (x: 矩阵) (y: 标量) = 标量乘矩阵 y x
// let inline (<*>) (x: 矩阵) (y: 向量) = 矩阵乘向量 x y
// let inline (<*>) (x: 矩阵) (y: 矩阵) = 矩阵乘 x y

// type 乘法约束<'T, 'U, 'R> =
//     static member 乘 : 'T * 'U -> 'R

// type 乘法约束<标量,标量,标量> with
//     static member 乘 (x:标量, y:标量) = 标量乘 x y

// type 乘法约束<标量,向量,向量> =
//     static member 乘 : 标量 * 向量 -> 向量



//TODO: 封装加法

//TODO: latex 输出

//TODO: 行列式

//TODO: 逆矩阵

//TODO: 特征值

//TODO: EVD 特征分解

//TODO: SVD 奇异值分解

//TODO: LU 分解

//TODO: QR 分解
