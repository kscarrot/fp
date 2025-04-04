module LinearTests

open Xunit
open FsUnit.Xunit
open Linear

[<Fact>]
let ``矩阵相等验证`` () =
    let a = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let b = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let c = [| [| 1.0; 2.0 |]; [| 3.0; 5.0 |] |]
    矩阵相等 a b |> should equal true
    矩阵相等 a c |> should equal false


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


[<Fact>]
let ``环元相等应该返回正确的结果`` () =
    let s1 = 环 (标量 2.0)
    let s2 = 环 (标量 2.0)
    let s3 = 环 (标量 3.0)
    let v1 = 环 (向量 [| 1.0; 2.0 |])
    let v2 = 环 (向量 [| 1.0; 2.0 |])
    let v3 = 环 (向量 [| 1.0; 3.0 |])
    let m1 = 环 (矩阵 [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |])
    let m2 = 环 (矩阵 [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |])

    s1 = s2 |> should equal true
    s1 = s3 |> should equal false
    v1 = v2 |> should equal true
    v1 = v3 |> should equal false
    s1 = v1 |> should equal false
    s1 = m1 |> should equal false
    v1 = m1 |> should equal false
    m1 = m2 |> should equal true

[<Fact>]
let ``环元加法应该返回正确的结果`` () =
    let s1 = 环 (标量 2.0)
    let s2 = 环 (标量 3.0)
    let v1 = 环 (向量 [| 1.0; 2.0 |])
    let v2 = 环 (向量 [| 3.0; 4.0 |])
    let m1 = 环 (矩阵 [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |])
    let m2 = 环 (矩阵 [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |])

    (s1 <+> s2) = (环 (标量 5.0)) |> should equal true

    (v1 <+> v2) = (环 (向量 [| 4.0; 6.0 |])) |> should equal true

    (m1 <+> m2) = (环 (矩阵 [| [| 6.0; 8.0 |]; [| 10.0; 12.0 |] |]))
    |> should equal true

    // 不同类型相加抛出异常
    (fun () -> s1 <+> v1 |> ignore) |> should throw typeof<System.Exception>
    (fun () -> s1 <+> m1 |> ignore) |> should throw typeof<System.Exception>
    (fun () -> v1 <+> m1 |> ignore) |> should throw typeof<System.Exception>

[<Fact>]
let ``环元乘法应该返回正确的结果`` () =
    let s1 = 环 (标量 2.0)
    let s2 = 环 (标量 3.0)
    let v1 = 环 (向量 [| 1.0; 2.0 |])
    let v2 = 环 (向量 [| 3.0; 4.0 |])
    let m1 = 环 (矩阵 [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |])
    let m2 = 环 (矩阵 [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |])


    (s1 <*> s2) = (环 (标量 6.0)) |> should equal true
    (s1 <*> s2 <*> s1) = (环 (标量 12.0)) |> should equal true

    (v1 <*> v2) = (环 (标量 11.0)) |> should equal true

    (m1 <*> m2) = (环 (矩阵 [| [| 19.0; 22.0 |]; [| 43.0; 50.0 |] |]))
    |> should equal true

    (s1 <*> v1) = (环 (向量 [| 2.0; 4.0 |])) |> should equal true

    (s1 <*> m1) = (环 (矩阵 [| [| 2.0; 4.0 |]; [| 6.0; 8.0 |] |])) |> should equal true

    (v1 <*> m1) = (环 (向量 [| 7.0; 10.0 |])) |> should equal true

    (m1 <*> v1) = (环 (向量 [| 5.0; 11.0 |])) |> should equal true

    let 零矩阵环 = 环 (矩阵 (零矩阵 2 2))
    (m1 <*> 零矩阵环) = 零矩阵环 |> should equal true

    let 单位矩阵环 = 环 (矩阵 (单位矩阵 2))
    (m1 <*> 单位矩阵环) = m1 |> should equal true
    (单位矩阵环 <*> m1) = m1 |> should equal true
    (单位矩阵环 <*> m1 <*> 单位矩阵环) = m1 |> should equal true
    (m1 <*> 单位矩阵环 <*> m2) = (m1 <*> m2) |> should equal true




[<Fact>]
let ``矩阵结合律简单验证`` () =
    let a = 环 (矩阵 ([| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]))
    let b = 环 (矩阵 ([| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]))
    let c = 环 (矩阵 ([| [| 9.0; 10.0 |]; [| 11.0; 12.0 |] |]))


    (a <*> (b <*> c)) = ((a <*> b) <*> c) |> should equal true

[<Fact>]
let ``矩阵乘不满足交换律验证`` () =
    let a = 环 (矩阵 ([| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]))
    let b = 环 (矩阵 ([| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]))

    (a <*> b) = (b <*> a) |> should equal false

[<Fact>]
let ``矩阵的分配律简单验证`` () =
    let a = 环 (矩阵 ([| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]))
    let b = 环 (矩阵 ([| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]))
    let c = 环 (矩阵 ([| [| 9.0; 10.0 |]; [| 11.0; 12.0 |] |]))

    (a <*> (b <+> c)) = ((a <*> b) <+> (a <*> c)) |> should equal true
    ((a <+> b) <*> c) = ((a <*> c) <+> (b <*> c)) |> should equal true

[<Fact>]
let ``矩阵乘法转置简单验证`` () =
    let 矩阵A = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let 矩阵B = [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]
    let A = 环 (矩阵 矩阵A)
    let B = 环 (矩阵 矩阵B)
    let AT = 环 (矩阵 (转置 矩阵A))
    let BT = 环 (矩阵 (转置 矩阵B))


    let AB_T =
        match (A <*> B).value with
        | 矩阵 M -> 环 (矩阵 (转置 M))
        | _ -> failwith "结果不是矩阵"

    AB_T = (BT <*> AT) |> should equal true


[<Fact>]

let ``右下验证`` () =
    右下 0 0 1 |> should equal (0, 0)

    右下 0 0 3 |> should equal (1, 1)
    右下 1 1 3 |> should equal (2, 2)
    右下 2 2 3 |> should equal (0, 0)


    右下 0 2 3 |> should equal (1, 0)
    右下 1 0 3 |> should equal (2, 1)
    右下 2 1 3 |> should equal (0, 2)

    右下 0 1 3 |> should equal (1, 2)
    右下 1 2 3 |> should equal (2, 0)
    右下 2 0 3 |> should equal (0, 1)


let ``左下验证`` () =
    左下 0 0 1 |> should equal (0, 0)

    左下 0 0 3 |> should equal (1, 2)
    左下 1 2 3 |> should equal (2, 1)
    左下 2 1 3 |> should equal (0, 0)

    左下 0 1 3 |> should equal (1, 0)
    左下 1 0 3 |> should equal (2, 2)
    左下 2 2 3 |> should equal (0, 1)

    左下 0 2 3 |> should equal (1, 1)
    左下 1 1 3 |> should equal (2, 0)
    左下 2 0 3 |> should equal (0, 2)


let ``方阵右对角验证`` () =
    let 单元素 = [| [| 1.0 |] |]
    方阵右对角 单元素 |> should equal 单元素


    let 矩阵A = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    let 矩阵B = [| [| 5.0; 6.0 |]; [| 7.0; 8.0 |] |]
    方阵右对角 矩阵A |> should equal [| [| 1.0; 4.0 |]; [| 2.0; 3.0 |] |]
    方阵右对角 矩阵B |> should equal [| [| 5.0; 8.0 |]; [| 6.0; 7.0 |] |]

    let I = 单位矩阵 3

    方阵右对角 I
    |> should equal [| [| 1.0; 1.0; 1.0 |]; [| 0.0; 0.0; 0.0 |]; [| 0.0; 0.0; 0.0 |] |]

let ``方阵左对角验证`` () =
    let 单元素 = [| [| 1.0 |] |]
    方阵左对角 单元素 |> should equal 单元素

    let 矩阵A = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
    方阵左对角 矩阵A |> should equal [| [| 1.0; 4.0 |]; [| 2.0; 3.0 |] |]

    let I = 单位矩阵 3

    方阵左对角 I
    |> should equal [| [| 1.0; 0.0; 0.0 |]; [| 0.0; 1.0; 0.0 |]; [| 0.0; 0.0; 1.0 |] |]



let ``三阶行列式验证`` () =
    let 矩阵A = [| [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0; 6.0 |]; [| 7.0; 8.0; 9.0 |] |]
    三阶行列式 矩阵A |> should equal 0.0

    let 矩阵B = [| [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0; 6.0 |]; [| 7.0; 8.0; 10.0 |] |]
    三阶行列式 矩阵B |> should equal -3

    let 矩阵C = [| [| 1.0; 3.0 |]; [| 2.0; 1.0 |] |]
    三阶行列式 矩阵C |> should equal -5

    let 矩阵D = [| [| 2.0; 1.0 |]; [| 1.0; 3.0 |] |]
    三阶行列式 矩阵D |> should equal 5

    let I = 单位矩阵 3
    三阶行列式 I |> should equal 1


[<Fact>]
let ``余子式方法行列式验证`` () =
    let 矩阵A = [| [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0; 6.0 |]; [| 7.0; 8.0; 9.0 |] |]

    行列式 矩阵A |> should equal 0.0

    let 矩阵B = [| [| 1.0; 1.0; 1.0 |]; [| 3.0; 5.0; 6.0 |]; [| 9.0; 25.0; 36.0 |] |]
    行列式 矩阵B |> should equal 6.0

    let 矩阵C = [| [| 0.0; 0.0; 0.0; 1.0 |]; [| 0.0; 0.0; 2.0; 0.0 |]; [| 0.0; 3.0; 0.0; 0.0 |]; [| 4.0; 0.0; 0.0; 0.0 |] |]
    行列式 矩阵C |> should equal 24.0



