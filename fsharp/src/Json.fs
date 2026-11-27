module Json

open System
open System.Globalization
open System.Text

type Token =
    | LBrace                  // {
    | RBrace                  // }
    | LBracket                // [
    | RBracket                // ]
    | Colon                   // :
    | Comma                   // ,
    | StringLit of string     // "abc"
    | NumberLit of float      // 123.45
    | BoolLit   of bool       // true / false
    | NullLit                 // null
    | EOF                     // 结束标记

type TokenInfo = { Token: Token; Line: int; Col: int }

type JsonValue =
    | JNull
    | JBool   of bool
    | JNumber of float
    | JString of string
    | JArray  of JsonValue list
    | JObject of Map<string, JsonValue>

let lexer (input: string) : Result<TokenInfo list, string> =
    let len = input.Length
    let mutable pos = 0
    let mutable line = 1
    let mutable col = 1
    let tokens = ResizeArray<TokenInfo>()

    let advance () =
        // 如果是换行符，行号 +1，列号重置为 1
        if input.[pos] = '\n' then
            line <- line + 1
            col <- 1
        else
            // 否则列号 +1
            col <- col + 1
        // pos 往后推一位
        pos <- pos + 1

    let emit token line col = tokens.Add({ Token = token; Line = line; Col = col })

    try
        while pos < len do
            let ch = input.[pos]
            let l = line
            let startCol = col

            if Char.IsWhiteSpace ch then
                advance ()
            else
                match ch with
                | '{' -> emit LBrace l startCol; advance ()
                | '}' -> emit RBrace l startCol; advance ()
                | '[' -> emit LBracket l startCol; advance ()
                | ']' -> emit RBracket l startCol; advance ()
                | ':' -> emit Colon l startCol; advance ()
                | ',' -> emit Comma l startCol; advance ()
                | '"' ->
                    advance ()
                    let sb = StringBuilder()

                    while pos < len && input.[pos] <> '"' do
                        if input.[pos] = '\\' then
                            advance ()
                            if pos >= len then failwithf "(%d:%d) 转义未结束" line col

                            match input.[pos] with
                            | '"' -> sb.Append('"') |> ignore
                            | '\\' -> sb.Append('\\') |> ignore
                            | 'n' -> sb.Append('\n') |> ignore
                            | 't' -> sb.Append('\t') |> ignore
                            | x -> failwithf "(%d:%d) 未知转义 '\\%c'" line col x

                            advance ()
                        else
                            sb.Append(input.[pos]) |> ignore
                            advance ()

                    if pos >= len then failwithf "(%d:%d) 字符串未闭合" l startCol
                    advance ()
                    emit (StringLit(sb.ToString())) l startCol
                | _ when ch = '-' || Char.IsDigit ch ->
                    let start = pos

                    while pos < len && (Char.IsDigit input.[pos] || ".-+eE".Contains input.[pos]) do
                        advance ()

                    let raw = input.Substring(start, pos - start)

                    match Double.TryParse(raw, NumberStyles.Float, CultureInfo.InvariantCulture) with
                    | true, num -> emit (NumberLit num) l startCol
                    | false, _ -> failwithf "(%d:%d) 非法数字 '%s'" l startCol raw
                | _ when Char.IsLetter ch ->
                    let start = pos

                    while pos < len && Char.IsLetter input.[pos] do
                        advance ()

                    match input.Substring(start, pos - start) with
                    | "true" -> emit (BoolLit true) l startCol
                    | "false" -> emit (BoolLit false) l startCol
                    | "null" -> emit NullLit l startCol
                    | word -> failwithf "(%d:%d) 未知关键字 '%s'" l startCol word
                | _ -> failwithf "(%d:%d) 非法字符 '%c'" l startCol ch

        emit EOF line col
        Ok(List.ofSeq tokens)
    with ex ->
        Error ex.Message

let parser (tokens: TokenInfo list) : Result<JsonValue, string> =
    let expect expected = function
        | { Token = t } :: rest when t = expected -> rest
        | { Token = t; Line = l; Col = c } :: _ -> failwithf "(%d:%d) 期望 %A，实际得到 %A" l c expected t
        | [] -> failwith "意外的输入结束"

    let rec parseValue = function
        | { Token = NullLit }     :: rest -> JNull, rest
        | { Token = BoolLit b }   :: rest -> JBool b, rest
        | { Token = NumberLit n } :: rest -> JNumber n, rest
        | { Token = StringLit s } :: rest -> JString s, rest
        | { Token = LBracket }    :: rest -> parseArray rest
        | { Token = LBrace }      :: rest -> parseObject rest
        | { Token = t; Line = l; Col = c } :: _ -> failwithf "(%d:%d) 意外的 Token: %A" l c t
        | [] -> failwith "意外的输入结束"

    and parseArray = function
        | { Token = RBracket } :: rest -> JArray [], rest
        | ts ->
            let rec loop acc cur =
                let elem, afterElem = parseValue cur
                match afterElem with
                | { Token = Comma }    :: next -> loop (elem :: acc) next
                | { Token = RBracket } :: next -> JArray (List.rev (elem :: acc)), next
                | { Token = t; Line = l; Col = c } :: _ -> failwithf "(%d:%d) 数组中意外的 Token: %A" l c t
                | [] -> failwith "数组未闭合"
            loop [] ts

    and parseObject = function
        | { Token = RBrace } :: rest -> JObject Map.empty, rest
        | ts ->
            let rec loop acc = function
                | { Token = StringLit k } :: afterKey ->
                    let v, afterVal = afterKey |> expect Colon |> parseValue
                    match afterVal with
                    | { Token = Comma }  :: next -> loop ((k, v) :: acc) next
                    | { Token = RBrace } :: next -> JObject (Map.ofList ((k, v) :: acc)), next
                    | { Token = t; Line = l; Col = c } :: _ -> failwithf "(%d:%d) 对象中意外的 Token: %A" l c t
                    | [] -> failwith "对象未闭合"
                | { Token = t; Line = l; Col = c } :: _ -> failwithf "(%d:%d) 对象键必须是字符串，实际得到 %A" l c t
                | [] -> failwith "对象未闭合"
            loop [] ts

    try
        match parseValue tokens with
        | ast, [ { Token = EOF } ] -> Ok ast
        | _, { Token = t; Line = l; Col = c } :: _ -> Error (sprintf "(%d:%d) 多余的尾巴: %A" l c t)
        | _ -> Error "未正常结束"
    with ex -> Error ex.Message


    