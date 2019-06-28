// Learn more about F# at http://fsharp.org

open System.Collections.Generic
open LParserC.LParserC
open MParser.MParser
open Precompiler.Precompiler

type Object =
| Nil
| Bool      of bool
| Char      of char
| Uint      of uint64
| Int       of int64
| Float     of float
| String    of string
| Symbol    of string
| MFunction of MFunction
| Class     of Class
| MutableObject     of MDict
| ImmutableObject   of MDict

and MDict = Dictionary<string, Object>

and Value = (Object * Class)

and Class = {
    type_name:  string
    modu_path:  string
    father:     Class
    methods:    Dictionary<string, MFunction>
}

and MClosure = {
    modu_path:  string
    father_env: MDict option
    arg_table:  string list
    body:       Expr list
}

and MNativeInterface = (string * VMContext -> Value[] -> Value)

and MFunction =
| Lambda of MClosure
| Native of MNativeInterface

and NativeLog = {
    funbody: MNativeInterface
    self:       Value
    super:      Value
}

and InterpreterLog = {
    funbody:    MClosure
    env:        MDict
    self:       Value
    super:      Value
}

and FunctionContext =
| Vm        of InterpreterLog
| Native    of NativeLog

and VMContext = {
    this_module_path:       string
    main_module_path:       string
    global_symbol_table:    MDict
    stack:                  Stack<FunctionContext>
    safe_point:             bool
}

type MaltLanguageException  (path: string, pos: Pos, value: Value) = inherit System.ApplicationException()
type ZincRuntimeException   (path: string, pos: Pos, info: string) = inherit System.ApplicationException()



let c2o = function
| RValue.Nil        -> (Nil     , null)
| RValue.Bool   x   -> (Bool   x, null)
| RValue.Char   x   -> (Char   x, null)
| RValue.Uint   x   -> (Uint   x, null)
| RValue.Int    x   -> (Int    x, null)
| RValue.Float  x   -> (Float  x, null)
| RValue.String x   -> (String x, null)
| RValue.Symbol x   -> (Symbol x, null)
| _ -> failwith "Invalid RValue"

let SetItem self s v = v // fixme
let BindLocal fc s v = v
let UnBindLocal fc s = ()
let ObjectDelete self = ()
let Call self method args = (Nil, null)

let rec eval (vmc: VMContext) ((e, p): Expr) =
    match e with
    | ExprValue.Nil -> (Nil, null)
    | ExprValue.Const x -> c2o x
    | ExprValue.Set (s, e) ->
        let valu = eval vmc e
        let self =
            match vmc.stack.Peek() with
            | Vm x -> x.self
            | Native x -> x.self
        SetItem self s (valu)
    | ExprValue.Let (s, e) ->
        let valu = eval vmc e
        let self =
            match vmc.stack.Peek() with
            | Vm x -> x.self
            | Native x -> x.self
        BindLocal self s (valu)
    | ExprValue.Use (s, e, es) ->
        let valu = eval vmc e
        BindLocal (vmc.stack.Peek()) s (valu) |> ignore
        let r = es |> List.map (fun x -> eval vmc x)
        ObjectDelete valu
        UnBindLocal (vmc.stack.Peek()) s
        List.last r
    | ExprValue.Lambda (ss, es) ->
        let fenv =
            if vmc.stack.Count = 0
            then None
            else
                match vmc.stack.Peek() with
                | Vm x -> Some x.env
                | Native x -> None
        (Object.MFunction (Lambda {
            modu_path   = vmc.this_module_path;
            father_env  = fenv;
            arg_table   = ss;
            body        = es;
        }), null)
    | ExprValue.MethodCall (e, s, es) ->
        let self = eval vmc e
        let args = es |> List.map (fun x -> eval vmc x)
        Call self s args
    | _ -> (Nil, null)



[<EntryPoint>]
let main argv =
    let a = fun x y -> x + y
    printfn "Hello World from F#!"
    0 // return an integer exit code
