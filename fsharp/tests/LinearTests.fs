module LinearTests

open Xunit
open FsUnit.Xunit
open Linear

[<Fact>]
let ``浮点数相加应该返回正确的和`` () = 1.5 |> 标量加 <| 2.5 |> should equal 4.0


[<Fact>]
let ``浮点数相加兼容整形 返回浮点数`` () = 5.0 |> 标量加 <| 5 |> should equal 10.0

let ``整形数相加应该返回正确的浮点数结果`` () = float 1 |> 标量加 <| 2 |> should equal 3.0

[<Fact>]
let ``向量相加应该返回正确的和`` () =
    let x = [| 1.0; 2.0; 3.0 |]
    let y = [| 4.0; 5.0; 6.0 |]
    let result = x |> 向量加 <| y

    result.Length |> should equal 3
    result.[0] |> should equal 5.0
    result.[1] |> should equal 7.0
    result.[2] |> should equal 9.0




[<Fact>]
let ``矩阵转置应该返回正确的结果`` () =
    let m = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let result = m |> 转置
    result.[0] |> should equal [| 1.0; 3.0 |]
    result.[1] |> should equal [| 2.0; 4.0 |]


[<Fact>]
let ``对角矩阵转置应该等于自身`` () =
    let m = [| [| 1.0; 0.0; 0.0 |]; [| 0.0; 2.0; 0.0 |]; [| 0.0; 0.0; 3.0 |] |]

    let result = m |> 转置
    result.[0] |> should equal m.[0]
    result.[1] |> should equal m.[1]
    result.[2] |> should equal m.[2]

[<Fact>]
let ``矩阵加应该返回正确的结果`` () =
    let a = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let b = [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]
    let result = a |> 矩阵加 <| b
    result.[0] |> should equal [| 6.0; 8.0 |]
    result.[1] |> should equal [| 10.0; 12.0 |]


[<Fact>]
let ``浮点数相乘应该返回正确的积`` () = 1.5 |> 标量乘 <| 2.0 |> should equal 3.0


[<Fact>]
let ``标量乘向量应该返回正确的结果`` () =
    let x = 2.0
    let y = [| 1.0; 2.0; 3.0 |]
    let result = x |> 标量乘向量 <| y
    result.Length |> should equal 3
    result.[0] |> should equal 2.0
    result.[1] |> should equal 4.0
    result.[2] |> should equal 6.0


[<Fact>]
let ``向量相乘应该返回正确的元素积`` () =
    let x = [| 1.0; 2.0; 3.0 |]
    let y = [| 4.0; 5.0; 6.0 |]
    // 1⋅4+2⋅5+3⋅6	 = 32
    x |> 向量乘 <| y |> should equal 32.0



[<Fact>]
let ``标量乘矩阵应该返回正确的结果`` () =
    let x = 2.0
    let m = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let result = x |> 标量乘矩阵 <| m
    result.[0] |> should equal [| 2.0; 4.0 |]
    result.[1] |> should equal [| 6.0; 8.0 |]


[<Fact>]
let ``矩阵乘法应该返回正确的结果`` () =
    let a = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let b = [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]
    let result = a |> 矩阵乘 <| b


    let expected = [| [| 19.0; 22.0 |]; [| 43.0; 50.0 |] |]
    矩阵相等 result expected |> should equal true


[<Fact>]
let ``矩阵乘向量阵应该返回正确的结果`` () =
    let v = [| 1.0; 2.0 |]
    let m = [| [| 3.0; 4.0 |]; [| 5.0; 6.0 |] |]
    let result = m |> 矩阵乘向量 <| v
    result |> should equal [| 11.0; 17.0 |]

[<Fact>]
let ``向量乘矩阵应该返回正确的结果`` () =
    let v = [| 1.0; 2.0; 3.0 |]
    let m = [| [| 4.0; 5.0 |]; [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |]
    let result = v |> 向量乘矩阵 <| m
    result |> should equal [| 40.0; 46.0 |]

[<Fact>]
let ``矩阵乘向量应该返回正确的结果`` () =
    let m = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let v = [| 5.0; 6.0 |]
    let result = m |> 矩阵乘向量 <| v
    result |> should equal [| 17.0; 39.0 |]

[<Fact>]
let ``零矩阵应该正确生成`` () =
    let 零 = 零矩阵 2 3
    零.Length |> should equal 2
    零.[0].Length |> should equal 3
    零.[0].[0] |> should equal 0.0
    零.[1].[2] |> should equal 0.0

[<Fact>]
let ``单位矩阵应该正确生成`` () =
    let 单位 = 单位矩阵 3
    单位.Length |> should equal 3
    单位.[0].Length |> should equal 3
    单位.[0].[0] |> should equal 1.0
    单位.[0].[1] |> should equal 0.0
    单位.[1].[1] |> should equal 1.0
    单位.[2].[2] |> should equal 1.0
