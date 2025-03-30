module LinearWithStringTests

open Xunit
open FsUnit.Xunit
open LinearWithString

[<Fact>]
let ``浮点数相加应该返回正确的和`` () =
    数字 1.5 |> 标量加 <| 数字 2.5 |> should equal (数字 4.0)


[<Fact>]
let ``浮点数相加兼容整形 返回浮点数`` () =
    数字 5.0 |> 标量加 <| 数字 5 |> should equal (数字 10.0)


[<Fact>]
let ``整形数相加应该返回正确的浮点数结果`` () =
    数字 1 |> 标量加 <| 数字 2 |> should equal (数字 3.0)


[<Fact>]
let ``字符串相加应该返回正确的连接结果`` () =
    字符串 "Hello" |> 标量加 <| 字符串 "World" |> should equal (字符串 "Hello+World")


[<Fact>]
let ``字符串相加空串处理`` () =
    字符串 "" |> 标量加 <| 字符串 "World" |> should equal (字符串 "World")
    字符串 "" |> 标量加 <| 字符串 "" |> should equal (字符串 "")


[<Fact>]
let ``不同类型相加应该抛出异常`` () =
    (fun () -> 数字 1.5 |> 标量加 <| 字符串 "World" |> ignore)
    |> should throw typeof<System.Exception>


[<Fact>]
let ``浮点数相乘应该返回正确的积`` () =
    数字 1.5 |> 标量乘 <| 数字 2.0 |> should equal (数字 3.0)


[<Fact>]
let ``字符串相乘应该返回正确的连接结果`` () =
    字符串 "Hello" |> 标量乘 <| 字符串 "World" |> should equal (字符串 "HelloWorld")


[<Fact>]
let ``空字符串相乘应该返回空字符串`` () =
    字符串 "" |> 标量乘 <| 字符串 "World" |> should equal (字符串 "")


[<Fact>]
let ``不同类型相乘应该抛出异常`` () =
    (fun () -> 数字 1.5 |> 标量乘 <| 字符串 "World" |> ignore)
    |> should throw typeof<System.Exception>


[<Fact>]
let ``向量相加应该返回正确的和`` () =
    let x = [| 数字 1.0; 数字 2.0; 数字 3.0 |]
    let y = [| 数字 4.0; 数字 5.0; 数字 6.0 |]
    let result = x |> 向量加 <| y

    result.Length |> should equal 3
    result.[0] |> should equal (数字 5.0)
    result.[1] |> should equal (数字 7.0)
    result.[2] |> should equal (数字 9.0)



[<Fact>]
let ``字符串向量相加应该返回正确的连接结果`` () =
    let x = [| 字符串 "Hello"; 字符串 "World" |]
    let y = [| 字符串 " "; 字符串 "!" |]
    let result = x |> 向量加 <| y

    result.Length |> should equal 2
    result.[0] |> should equal (字符串 "Hello+ ")
    result.[1] |> should equal (字符串 "World+!")


[<Fact>]
let ``向量相乘应该返回正确的元素积`` () =
    let x = [| 数字 1.0; 数字 2.0; 数字 3.0 |]
    let y = [| 数字 4.0; 数字 5.0; 数字 6.0 |]
    // 1⋅4+2⋅5+3⋅6	 = 32
    x |> 向量乘 <| y |> should equal (数字 32.0)


[<Fact>]
let ``字符串向量相乘应该返回正确的连接结果`` () =
    let x = [| 字符串 "x_1"; 字符串 "y_1"; 字符串 "z_1" |]
    let y = [| 字符串 "x_2"; 字符串 "y_2"; 字符串 "z_2" |]
    x |> 向量乘 <| y |> should equal (字符串 "x_1x_2+y_1y_2+z_1z_2")




[<Fact>]
let ``标量乘向量应该返回正确的结果`` () =
    let x = 数字 2.0
    let y = [| 数字 1.0; 数字 2.0; 数字 3.0 |]
    let result = x |> 标量乘向量 <| y
    result.Length |> should equal 3
    result.[0] |> should equal (数字 2.0)
    result.[1] |> should equal (数字 4.0)
    result.[2] |> should equal (数字 6.0)

let ``字符串标量乘字符串向量应该返回正确结果`` () =
    let x = 字符串 "a"
    let y = [| 字符串 "x"; 字符串 "y"; 字符串 "z" |]
    let result = x |> 标量乘向量 <| y
    result.Length |> should equal 3
    result.[0] |> should equal (字符串 "ax")
    result.[1] |> should equal (字符串 "ay")
    result.[2] |> should equal (字符串 "az")


[<Fact>]
let ``矩阵转置应该返回正确的结果`` () =
    let m = [| [| 数字 1.0; 数字 2.0 |]; [| 数字 3.0; 数字 4.0 |] |]
    let result = m |> 转置
    result.[0] |> should equal [| 数字 1.0; 数字 3.0 |]
    result.[1] |> should equal [| 数字 2.0; 数字 4.0 |]


[<Fact>]
let ``对角矩阵转置应该等于自身`` () =
    let m =
        [| [| 数字 1.0; 数字 0.0; 数字 0.0 |]
           [| 数字 0.0; 数字 2.0; 数字 0.0 |]
           [| 数字 0.0; 数字 0.0; 数字 3.0 |] |]

    let result = m |> 转置
    result.[0] |> should equal m.[0]
    result.[1] |> should equal m.[1]
    result.[2] |> should equal m.[2]


[<Fact>]
let ``矩阵乘法应该返回正确的结果`` () =
    let a = [| [| 数字 1.0; 数字 2.0 |]; [| 数字 3.0; 数字 4.0 |] |]
    let b = [| [| 数字 5.0; 数字 6.0 |]; [| 数字 7.0; 数字 8.0 |] |]
    let result = a |> 矩阵乘 <| b
    result.[0] |> should equal [| 数字 19.0; 数字 22.0 |]
    result.[1] |> should equal [| 数字 43.0; 数字 50.0 |]


[<Fact>]
let ``字符串矩阵乘应该返回正确的结果`` () =
    let a = [| [| 字符串 "a"; 字符串 "b" |]; [| 字符串 "c"; 字符串 "d" |] |]
    let b = [| [| 字符串 "e"; 字符串 "f" |]; [| 字符串 "g"; 字符串 "h" |] |]
    let result = a |> 矩阵乘 <| b
    result.[0] |> should equal [| 字符串 "ae+bg"; 字符串 "af+bh" |]
    result.[1] |> should equal [| 字符串 "ce+dg"; 字符串 "cf+dh" |]


[<Fact>]
let ``数字标量乘矩阵应该返回正确的结果`` () =
    let x = 数字 2.0
    let m = [| [| 数字 1.0; 数字 2.0 |]; [| 数字 3.0; 数字 4.0 |] |]
    let result = x |> 标量乘矩阵 <| m
    result.[0] |> should equal [| 数字 2.0; 数字 4.0 |]
    result.[1] |> should equal [| 数字 6.0; 数字 8.0 |]

[<Fact>]
let ``字符串标量乘矩阵应该返回正确的结果`` () =
    let x = 字符串 "前缀_"
    let m = [| [| 字符串 "a"; 字符串 "b" |]; [| 字符串 "c"; 字符串 "d" |] |]
    let result = x |> 标量乘矩阵 <| m
    result.[0] |> should equal [| 字符串 "前缀_a"; 字符串 "前缀_b" |]
    result.[1] |> should equal [| 字符串 "前缀_c"; 字符串 "前缀_d" |]

[<Fact>]
let ``空字符串标量乘矩阵应该返回空字符串矩阵`` () =
    let x = 字符串 ""
    let m = [| [| 字符串 "a"; 字符串 "b" |]; [| 字符串 "c"; 字符串 "d" |] |]
    let result = x |> 标量乘矩阵 <| m
    result.[0] |> should equal [| 字符串 ""; 字符串 "" |]
    result.[1] |> should equal [| 字符串 ""; 字符串 "" |]

[<Fact>]
let ``不同类型标量乘矩阵应该抛出异常`` () =
    let x = 数字 2.0
    let m = [| [| 字符串 "a"; 字符串 "b" |]; [| 字符串 "c"; 字符串 "d" |] |]
    (fun () -> x |> 标量乘矩阵 <| m |> ignore) |> should throw typeof<System.Exception>



[<Fact>]
let ``矩阵加应该返回正确的结果`` () =
    let a = [| [| 数字 1.0; 数字 2.0 |]; [| 数字 3.0; 数字 4.0 |] |]
    let b = [| [| 数字 5.0; 数字 6.0 |]; [| 数字 7.0; 数字 8.0 |] |]
    let result = a |> 矩阵加 <| b
    result.[0] |> should equal [| 数字 6.0; 数字 8.0 |]
    result.[1] |> should equal [| 数字 10.0; 数字 12.0 |]


[<Fact>]
let ``向量乘矩阵应该返回正确的结果`` () =
    let v = [| 数字 1.0; 数字 2.0 |]
    let m = [| [| 数字 3.0; 数字 4.0 |]; [| 数字 5.0; 数字 6.0 |] |]
    let result = m |> 向量乘矩阵 <| v
    result |> should equal [| 数字 11.0; 数字 17.0 |]

