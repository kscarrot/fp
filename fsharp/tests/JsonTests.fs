module JsonTests

open Xunit
open FsUnit.Xunit
open Json

let private parse (input: string) =
    match lexer input with
    | Error e -> Error e
    | Ok tokens -> parser tokens

let private expectTokens (input: string) expected =
    match lexer input with
    | Ok tokens -> tokens |> List.map (fun t -> t.Token) |> should equal expected
    | Error e -> failwith e

let private expectLexError (input: string) expected =
    match lexer input with
    | Error actual -> actual |> should equal expected
    | Ok tokens -> failwithf "期望错误，实际得到 %A" tokens

let private expectValue (input: string) expected =
    match parse input with
    | Ok actual -> actual |> should equal expected
    | Error e -> failwith e

let private expectParseError (input: string) expected =
    match parse input with
    | Error actual -> actual |> should equal expected
    | Ok value -> failwithf "期望错误，实际得到 %A" value


[<Fact>]
let ``空输入只产生结束标记`` () =
    expectTokens "" [ EOF ]


[<Fact>]
let ``空白会被跳过`` () =
    expectTokens " \n\t 1 " [ NumberLit 1.0; EOF ]


[<Fact>]
let ``标点符号按出现顺序产生 token`` () =
    expectTokens "{[]},:" [ LBrace; LBracket; RBracket; RBrace; Comma; Colon; EOF ]


[<Fact>]
let ``字符串转义会被还原`` () =
    expectTokens "\"a\\nb\\t\\\"\\\\\"" [ StringLit "a\nb\t\"\\"; EOF ]


[<Fact>]
let ``数字支持负号和科学计数法`` () =
    expectTokens "-2 1.5 1e2" [ NumberLit -2.0; NumberLit 1.5; NumberLit 100.0; EOF ]


[<Fact>]
let ``布尔和 null 是独立 token`` () =
    expectTokens "true false null" [ BoolLit true; BoolLit false; NullLit; EOF ]


[<Fact>]
let ``换行后列号从 1 重新计算`` () =
    match lexer "{\n\"a\"\n}" with
    | Ok tokens ->
        tokens
        |> List.map (fun t -> t.Token, t.Line, t.Col)
        |> should equal [ LBrace, 1, 1; StringLit "a", 2, 1; RBrace, 3, 1; EOF, 3, 2 ]
    | Error e -> failwith e


[<Fact>]
let ``未闭合字符串应该报错`` () =
    expectLexError "\"abc" "(1:1) 字符串未闭合"


[<Fact>]
let ``未知转义应该报错`` () =
    expectLexError "\"\\q\"" "(1:3) 未知转义 '\\q'"


[<Fact>]
let ``转义未结束应该报错`` () =
    expectLexError "\"\\" "(1:3) 转义未结束"


[<Fact>]
let ``非法数字应该报错`` () =
    expectLexError "1.2.3" "(1:1) 非法数字 '1.2.3'"


[<Fact>]
let ``未知关键字应该报错`` () =
    expectLexError "tru" "(1:1) 未知关键字 'tru'"


[<Fact>]
let ``非法字符应该报错`` () =
    expectLexError "@" "(1:1) 非法字符 '@'"


[<Fact>]
let ``标量值可以解析`` () =
    expectValue "null" JNull
    expectValue "true" (JBool true)
    expectValue "false" (JBool false)
    expectValue "1.5" (JNumber 1.5)
    expectValue "\"hi\"" (JString "hi")


[<Fact>]
let ``空数组和空对象可以解析`` () =
    expectValue "[]" (JArray [])
    expectValue "{}" (JObject Map.empty)


[<Fact>]
let ``嵌套数组和对象可以解析`` () =
    expectValue
        """{"a": [1, true, null], "b": {"c": "x"}}"""
        (JObject (Map.ofList [
            ("a", JArray [ JNumber 1.0; JBool true; JNull ])
            ("b", JObject (Map.ofList [ ("c", JString "x") ]))
        ]))


[<Fact>]
let ``重复键保留先出现的`` () =
    expectValue """{"a": 1, "a": 2}""" (JObject (Map.ofList [ ("a", JNumber 1.0) ]))


[<Fact>]
let ``空输入不能作为值`` () =
    expectParseError "" "(1:1) 意外的 Token: EOF"


[<Fact>]
let ``多余的尾巴应该报错`` () =
    expectParseError "1 2" "(1:3) 多余的尾巴: NumberLit 2.0"


[<Fact>]
let ``未闭合的数组应该报错`` () =
    expectParseError "[1" "(1:3) 数组中意外的 Token: EOF"


[<Fact>]
let ``未闭合的对象应该报错`` () =
    expectParseError "{" "(1:2) 对象键必须是字符串，实际得到 EOF"


[<Fact>]
let ``对象键必须是字符串`` () =
    expectParseError "{1: 2}" "(1:2) 对象键必须是字符串，实际得到 NumberLit 1.0"


[<Fact>]
let ``尾逗号目前应该报错`` () =
    expectParseError "[1,]" "(1:4) 意外的 Token: RBracket"


[<Fact>]
let ``不带引号的键目前应该报错`` () =
    expectParseError "{a: 1}" "(1:2) 未知关键字 'a'"
