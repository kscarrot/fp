module LinearTests

open Xunit
open FsUnit.Xunit
open Linear

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
let ``不同长度向量相加应该抛出异常`` () =
    let x = [| 数字 1.0; 数字 2.0 |]
    let y = [| 数字 3.0; 数字 4.0; 数字 5.0 |]
    (fun () -> x |> 向量加 <| y |> ignore) |> should throw typeof<System.Exception>


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
let ``不同长度向量相乘应该抛出异常`` () =
    let x = [| 数字 1.0; 数字 2.0 |]
    let y = [| 数字 3.0; 数字 4.0; 数字 5.0 |]
    (fun () -> x |> 向量乘 <| y |> ignore) |> should throw typeof<System.Exception> 