module ProgramTests

open Xunit
open Program

[<Fact>]
let ``浮点数相加应该返回正确的和`` () =
    let x = 数字 1.5
    let y = 数字 2.5
    let result = 加 x y

    match result with
    | 数字 value -> Assert.Equal(4.0, value)
    | _ -> Assert.Fail("应该返回浮点类型")

[<Fact>]
let ``浮点数相加兼容整形 返回浮点数`` () =
    let x = 数字 5.0
    let y = 数字 5
    let result = 加 x y

    match result with
    | 数字 value -> Assert.Equal(10.0, value)
    | _ -> Assert.Fail("应该返回浮点类型")

let ``整形数相加应该返回正确的浮点数结果`` () =
    let x = 数字 1
    let y = 数字 2
    let result = 加 x y

    match result with
    | 数字 value -> Assert.Equal(3.0, value)
    | _ -> Assert.Fail("应该返回浮点类型")

[<Fact>]
let ``字符串相加应该返回正确的连接结果`` () =
    let x = 字符串 "Hello"
    let y = 字符串 "World"
    let result = 加 x y

    match result with
    | 字符串 value -> Assert.Equal("Hello+World", value)
    | _ -> Assert.Fail("应该返回字符串类型")



[<Fact>]
let ``字符串相加空串处理`` () =
    let x = 字符串 ""
    let y = 字符串 "World"
    let result = 加 x y

    match result with
    | 字符串 value -> Assert.Equal("World", value)
    | _ -> Assert.Fail("应该返回字符串类型")

    let x = 字符串 ""
    let y = 字符串 ""
    let result = 加 x y

    match result with
    | 字符串 value -> Assert.Equal("", value)
    | _ -> Assert.Fail("应该返回字符串类型")

[<Fact>]
let ``不同类型相加应该抛出异常`` () =
    let x = 数字 1.5
    let y = 字符串 "World"
    Assert.Throws<System.Exception>(fun () -> 加 x y |> ignore)

[<Fact>]
let ``浮点数相乘应该返回正确的积`` () =
    let x = 数字 1.5
    let y = 数字 2.0
    let result = 乘 x y

    match result with
    | 数字 value -> Assert.Equal(3.0, value)
    | _ -> Assert.Fail("应该返回数字类型")

[<Fact>]
let ``字符串相乘应该返回正确的连接结果`` () =
    let x = 字符串 "Hello"
    let y = 字符串 "World"
    let result = 乘 x y

    match result with
    | 字符串 value -> Assert.Equal("HelloWorld", value)
    | _ -> Assert.Fail("应该返回字符串类型")

[<Fact>]
let ``空字符串相乘应该返回空字符串`` () =
    let x = 字符串 ""
    let y = 字符串 "World"
    let result = 乘 x y

    match result with
    | 字符串 value -> Assert.Equal("", value)
    | _ -> Assert.Fail("应该返回字符串类型")

[<Fact>]
let ``不同类型相乘应该抛出异常`` () =
    let x = 数字 1.5
    let y = 字符串 "World"
    Assert.Throws<System.Exception>(fun () -> 乘 x y |> ignore)

[<Fact>]
let ``向量相加应该返回正确的和`` () =
    let x = [| 数字 1.0; 数字 2.0; 数字 3.0 |]
    let y = [| 数字 4.0; 数字 5.0; 数字 6.0 |]
    let result = 向量加 x y
    Assert.Equal(3, result.Length)

    match result.[0] with
    | 数字 value -> Assert.Equal(5.0, value)
    | _ -> Assert.Fail("应该返回数字类型")

    match result.[1] with
    | 数字 value -> Assert.Equal(7.0, value)
    | _ -> Assert.Fail("应该返回数字类型")

    match result.[2] with
    | 数字 value -> Assert.Equal(9.0, value)
    | _ -> Assert.Fail("应该返回数字类型")


[<Fact>]
let ``不同长度向量相加应该抛出异常`` () =
    let x = [| 数字 1.0; 数字 2.0 |]
    let y = [| 数字 3.0; 数字 4.0; 数字 5.0 |]
    Assert.Throws<System.Exception>(fun () -> 向量加 x y |> ignore)


[<Fact>]
let ``字符串向量相加应该返回正确的连接结果`` () =
    let x = [| 字符串 "Hello"; 字符串 "World" |]
    let y = [| 字符串 " "; 字符串 "!" |]
    let result = 向量加 x y
    Assert.Equal(2, result.Length)

    match result.[0] with
    | 字符串 value -> Assert.Equal("Hello+ ", value)
    | _ -> Assert.Fail("应该返回字符串类型")

    match result.[1] with
    | 字符串 value -> Assert.Equal("World+!", value)
    | _ -> Assert.Fail("应该返回字符串类型")

[<Fact>]
let ``向量相乘应该返回正确的元素积`` () =
    let x = [| 数字 1.0; 数字 2.0; 数字 3.0 |]
    let y = [| 数字 4.0; 数字 5.0; 数字 6.0 |]
    let result = 向量乘 x y
    // 1⋅4+2⋅5+3⋅6	 = 32
    match result with
    | 数字 value -> Assert.Equal(32.0, value)
    | _ -> Assert.Fail("应该返回数字类型")



[<Fact>]
let ``字符串向量相乘应该返回正确的连接结果`` () =
    let x = [| 字符串 "x_1"; 字符串 "y_1"; 字符串 "z_1" |]
    let y = [| 字符串 "x_2"; 字符串 "y_2"; 字符串 "z_2" |]
    let result = 向量乘 x y
    // x_1x_2+y_1y_2+z_1z_2
    match result with
    | 字符串 value -> Assert.Equal("x_1x_2+y_1y_2+z_1z_2", value)
    | _ -> Assert.Fail("应该返回字符串类型")


[<Fact>]
let ``不同长度向量相乘应该抛出异常`` () =
    let x = [| 数字 1.0; 数字 2.0 |]
    let y = [| 数字 3.0; 数字 4.0; 数字 5.0 |]
    Assert.Throws<System.Exception>(fun () -> 向量乘 x y |> ignore)
