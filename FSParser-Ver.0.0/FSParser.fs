module FSParser
    open System.Linq
    open System.Collections.Generic
    type Pattern =
    | Char of char
    | String of string
    | NChar of char
    | NString of string
    | And of Pattern list
    | Or of Pattern list
    | For0 of Pattern
    | For1 of Pattern
    | Or01 of Pattern
    | Input of string
    | NUMBER
    | STRING
    | OP
    let rec Join(join: string, array: char[]) =
        let mutable result = ""
        let mutable count = 0
        for item in array do
            result <- result + item.ToString()
            count <- count + 1
            if count < array.Length then
                result <- result + join
        result
    let Code: Dictionary<string, Pattern> = new Dictionary<string, Pattern>()
    let Op: List<string> = new List<string>()
    let rec Add(name: string, code: Pattern) =
        Code.Add($"<{name}>", code)
    let rec OpAdd(operator: string) =
        Op.Add(operator)
    let rec OpesAdd(opes: string list) =
        for item in opes do
            Op.Add(item)
    let rec IsMatch(s: string) =
        let mutable S = s
        let mutable S0 = S
        let rec For(str: string, pattern: Pattern, mode: int) =
            match pattern with
            | Char ch ->
                if S.StartsWith(ch) then
                    if mode = 0 then S <- Join("", S.Skip(1).ToArray())
                    true
                else
                    false
            | String st ->
                if S.StartsWith(st) then
                    if mode = 0 then S <- Join("", S.Skip(st.Length).ToArray())
                    true
                else false
            | NChar ch ->
                if S.StartsWith(ch) = false then
                    if mode = 0 then S <- Join("", S.Skip(1).ToArray())
                    true
                else false
            | NString st ->
                let s0 = S
                let bool: List<bool> = new List<bool>()
                for ch in st do
                    if S.StartsWith(ch) = false then
                        bool.Add(true)
                        if mode = 0 then S <- Join("", S.Skip(1).ToArray())
                    else
                        bool.Add(false)
                if s0.Length - S.Length = st.Length || bool.All(fun x -> x = true) then true
                else false
            | And hs ->
                let bool: List<bool> = new List<bool>()
                for item in hs do
                    if bool.Count = 0 || bool.All(fun x -> x = true) then bool.Add(For(S, item, mode))
                bool.All(fun x -> x = true)
            | Or hs ->
                let bool: List<bool> = new List<bool>()
                let s0 = S
                for item in hs do
                    if bool.Any(fun x -> x = true) = false then bool.Add(For(s0, item, mode))
                bool.Any(fun x -> x = true)
            | For0 hs ->
                while For(S, hs, mode) do
                    printf ""
                true
            | For1 hs ->
                let mutable count = 0
                while For(S, hs, mode) do
                    count <- count + 1
                if count <> 0 then true
                else false
            | Or01 hs ->
                let _ =  For(S, hs, mode)
                true
            | Input pat ->
                For(S, Code[$"<{pat}>"], mode)
            | NUMBER ->
                For(S, For1(Or([String "0"; String "1"; String "2"; String "3"; String "4"; String "5"; String "6"; String "7"; String "8"; String "9"])), mode)
            | STRING ->
                For(S, For1(Or([NChar '0'; NChar '1'; NChar '2'; NChar '3'; NChar '4'; NChar '5'; NChar '6'; NChar '7'; NChar '8'; NChar '9'; NChar '\"'])), mode)
            | OP ->
                let mutable bool: List<bool> = new List<bool>()
                for _ in Op do
                    bool.Add(false)
                let mutable count = 0
                let mutable op = ""
                while count < Op.Count do
                    if bool.Any(fun x -> x = true) = false then bool[count] <- For(S, String Op[count], 1)
                    if bool.Any(fun x -> x = true) && op = "" then op <- Op[count]
                    count <- count + 1
                S <- Join("", S.Skip(op.Length).ToArray())
                bool.Any(fun x -> x = true)
        let mutable bool: bool = false
        let mutable bool0: bool = true
        let mutable count = 0
        while count < Code.Count do
            if bool0 then
                bool <- For(S, Code.Values.ToArray()[count], 0)
                if bool && S.Length = 0 then
                    bool0 <- false
                else
                    bool <- false
                    S <- S0
            count <- count + 1
        bool