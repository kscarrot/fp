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

let 矩阵乘向量 (mx: 矩阵) (vy: 向量) : 向量 =
    mx |> Array.map (fun 行 -> 行 |> 向量乘 <| vy)

let 向量乘矩阵 (vx: 向量) (my: 矩阵) : 向量 =
    转置 my |> Array.map (fun 列 -> 列 |> 向量乘 <| vx)

let 矩阵乘 (mx: 矩阵) (my: 矩阵) =
    mx |> Array.map (fun 行 -> 转置 my |> Array.map (fun 列 -> 行 |> 向量乘 <| 列))

let 单位矩阵 (n: int) : 矩阵 =
    Array.init n (fun i -> Array.init n (fun j -> if i = j then 1.0 else 0.0))

let 零矩阵 (n: int) (m: int) : 矩阵 =
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

let 右下 行坐标 列坐标 范数 = (行坐标 + 1) % 范数, (列坐标 + 1) % 范数
let 左下 行坐标 列坐标 范数 = (行坐标 + 1) % 范数, (列坐标 - 1 + 范数) % 范数

let 方阵对角列 (m: 矩阵, 下一个) =
    let 范数 = Array.length m

    let rec 获取对角线 行坐标 列坐标 剩余步数 累积数组 =
        if 剩余步数 = 0 then
            Array.rev 累积数组
        else
            let 当前元素 = m.[行坐标].[列坐标]
            let 新行坐标, 新列坐标 = 下一个 行坐标 列坐标 范数
            获取对角线 新行坐标 新列坐标 (剩余步数 - 1) (Array.append [| 当前元素 |] 累积数组)

    let 所有对角线 = Array.init 范数 (fun 初始列坐标 -> 获取对角线 0 初始列坐标 范数 [||])

    所有对角线

let 方阵右对角 矩阵 = 方阵对角列 (矩阵, 右下)
let 方阵左对角 矩阵 = 方阵对角列 (矩阵, 左下)

let 对角乘积和 矩阵 =
    矩阵 |> Array.map (fun 行 -> Array.reduce 标量乘 行) |> Array.reduce 标量加


let 三阶行列式 矩阵 =
    let 右对角 = 方阵右对角 矩阵
    let 左对角 = 方阵左对角 矩阵
    对角乘积和 右对角 - 对角乘积和 左对角



// 获取余子式矩阵
let 余子式 矩阵 行坐标 列坐标 =
    矩阵
    |> Array.mapi (fun i row' ->
        if i <> 行坐标 then
            row'
            |> Array.mapi (fun j x -> if j <> 列坐标 then Some x else None)
            |> Array.choose id
            |> Some
        else
            None)
    |> Array.choose id


let rec 行列式 (矩阵: 矩阵) =
    let 范数 = Array.length 矩阵

    match 范数 with
    | 1 -> 矩阵.[0].[0]
    | 2 -> 矩阵.[0].[0] * 矩阵.[1].[1] - 矩阵.[0].[1] * 矩阵.[1].[0]
    | _ ->
        [ 0 .. 范数 - 1 ]
        |> List.map (fun 系数 ->
            let 符号 = if 系数 % 2 = 0 then 1.0 else -1.0
            let 余子式 = 余子式 矩阵 0 系数
            符号 * float 系数 * 行列式 余子式)
        |> List.sum

let 伴随矩阵 (矩阵: 矩阵) =
    let 范数 = Array.length 矩阵

    let 代数余子式 i j =
        let 符号 = if (i + j) % 2 = 0 then 1.0 else -1.0
        符号 * 行列式 (余子式 矩阵 i j)

    Array.init 范数 (fun i -> Array.init 范数 (fun j -> 代数余子式 i j)) |> 转置

let 逆矩阵 (矩阵: 矩阵) =
    let 行列式值 = 行列式 矩阵

    if 行列式值 = 0.0 then
        failwith "行列式为0，无法求逆矩阵"
    else
        标量乘矩阵 (1.0 / 行列式值) (伴随矩阵 矩阵)


type 环元 =
    | 标量 of 标量
    | 向量 of 向量
    | 矩阵 of 矩阵

type 环(value: 环元) =
    member this.value = value

    static member (<*>)(x: 环, y: 环) =
        match x.value, y.value with
        | 标量 x, 标量 y -> 环 (标量 (x * y))
        | 标量 x, 向量 y -> 环 (向量 (标量乘向量 x y))
        | 标量 x, 矩阵 y -> 环 (矩阵 (标量乘矩阵 x y))
        | 向量 x, 标量 y -> 环 (向量 (标量乘向量 y x))
        | 向量 x, 向量 y -> 环 (标量 (向量乘 x y))
        | 向量 x, 矩阵 y -> 环 (向量 (向量乘矩阵 x y))
        | 矩阵 x, 标量 y -> 环 (矩阵 (标量乘矩阵 y x))
        | 矩阵 x, 向量 y -> 环 (向量 (矩阵乘向量 x y))
        | 矩阵 x, 矩阵 y -> 环 (矩阵 (矩阵乘 x y))

    static member (<+>)(x: 环, y: 环) =
        match x.value, y.value with
        | 标量 x, 标量 y -> 环 (标量 (x + y))
        | 向量 x, 向量 y -> 环 (向量 (向量加 x y))
        | 矩阵 x, 矩阵 y -> 环 (矩阵 (矩阵加 x y))
        | _ -> failwith "不支持的类型"

    member this.行列式() =
        match this.value with
        | 矩阵 m -> 行列式 m
        | _ -> failwith "不是矩阵"

    member this.逆矩阵() =
        match this.value with
        | 矩阵 m -> 逆矩阵 m
        | _ -> failwith "不是矩阵"

    member this.latex() =
        match this.value with
        | 标量 x -> sprintf "%f" x
        | 向量 x -> sprintf "%A" x
        | 矩阵 x ->
            let elements =
                x
                |> Array.map (fun row -> row |> Array.map (sprintf "%g") |> String.concat " & ")
                |> String.concat "\\\\\n"
            sprintf "\\begin{vmatrix}\n%s\n\\end{vmatrix}" elements

    override this.ToString() =
        match this.value with
        | 标量 x -> sprintf "%f" x
        | 向量 x -> sprintf "%A" x
        | 矩阵 x -> sprintf "%A" x

    override this.Equals(obj) =
        match obj with
        | :? 环 as other ->
            match this.value, other.value with
            | 标量 x, 标量 y -> x = y
            | 向量 x, 向量 y -> Array.forall2 (=) x y
            | 矩阵 x, 矩阵 y -> 矩阵相等 x y
            | _ -> false
        | _ -> false

    override this.GetHashCode() =
        match this.value with
        | 标量 x -> hash x
        | 向量 x -> hash x
        | 矩阵 x -> hash x


//TODO: 特征值

//TODO: EVD 特征分解

//TODO: SVD 奇异值分解

//TODO: LU 分解

//TODO: QR 分解
