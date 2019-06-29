// Learn more about F# at http://fsharp.org

open System.Collections.Generic
open LParserC.LParserC
open MParser.MParser
open Precompiler.Precompiler
open System
open System.IO

type Object =
| Nil
| Bool      of bool //fixme
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

and MDict = Dictionary<string, Value>

and Value = (Object * Class)

and Class = {
    type_name:  string
    modu_path:  string
    mutable father:     Class option
    mutable meta_class: Class option
    mutable methods:    Dictionary<string, MFunction>
}

and MClosure = {
    modu_path:  string
    father_env: FunctionContext option
    arg_table:  string list
    body:       Expr list
}

and MNativeInterface = (string * (VMContext -> Value -> Value[] -> Value))

and MFunction =
| Lambda of MClosure
| Native of MNativeInterface

and NativeLog = {
    funbody: MNativeInterface
    self:       Value
    super:      Value option
}

and InterpreterLog = {
    funbody:    MClosure
    env:        MDict
    self:       Value
    super:      Value option
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
type ZincRuntimeException   (path: string, pos: Pos option, info: string) = inherit System.ApplicationException()

type SymbolNotFoundException(sym: string) = inherit System.ApplicationException()

exception SetImmutableObjectException
exception FunctionParameterException
exception MataClassIsNoneException
exception FunctionContextException
exception TypeException

///------------------------------------------------

let EmptyLambda = {
    modu_path   = "/"
    father_env  = None
    arg_table   = []
    body        = []
}

// Base Object init
let mutable NilClass = {
    type_name=  "Nil"
    modu_path=  "/"
    father=     None
    meta_class= None
    methods=    Dictionary<string, MFunction>()
}

let mutable ObjectClass = {
    type_name=  "Object"
    modu_path=  "/"
    father=     None
    meta_class= None
    methods=    Dictionary<string, MFunction>()
}

let mutable CharClass   : Class = NilClass
let mutable BoolClass   : Class = NilClass
let mutable UintClass   : Class = NilClass
let mutable IntClass    : Class = NilClass
let mutable FloatClass  : Class = NilClass
let mutable StringClass : Class = NilClass
let mutable SymbolClass : Class = NilClass
let mutable ClosureClass: Class = NilClass

let mutable TypeClass = {
    type_name=  "Type"
    modu_path=  "/"
    father=     Some ObjectClass
    meta_class= None
    methods=    Dictionary<string, MFunction>()
}

let ObjectValue = (Class ObjectClass, ObjectClass)
let NilValue = (Nil, NilClass)

// -- tools function --

// fixme
let rec LoadSymbol (vmc: VMContext) fc s =
    let mutable rv = ref NilValue
    match fc with
    | Vm x ->
        if x.env.TryGetValue(s, rv)
        then (!rv)
        else
            match x.funbody.father_env with
            | Some x -> LoadSymbol vmc x s
            | None -> 
                let mutable rv: Value ref = ref NilValue
                if vmc.global_symbol_table.TryGetValue(s, rv)
                then (!rv)
                else raise (SymbolNotFoundException s)
    | _ -> raise FunctionContextException


let SetItem ((self, _): Value) s (v: Value) =
    match self with
    | MutableObject o -> o.Add(s, v); v
    | _ -> raise SetImmutableObjectException
    
let BindLocal (vmc: VMContext) s v =
    if vmc.stack.Count = 0
    then vmc.global_symbol_table.Add(s, v)
    else
        match (vmc.stack.Peek()) with
        | Vm x -> x.env.Add(s, v)
        | _ -> raise FunctionContextException
    v

let rec GetMethod (c: Class) (s: string) =
    let rc: MFunction ref = ref (Lambda EmptyLambda)
    if c.methods.TryGetValue(s, rc)
    then (!rc)
    else
        match c.father with
        | Some x -> GetMethod x s
        | _ -> raise (SymbolNotFoundException s)

let UnBindLocal fc s = ()
let ObjectDelete self = ()

///------------------------------------------------

// RValue to Value
let c2o = function
| RValue.Nil        -> NilValue
| RValue.Bool   x   -> (Bool   x, BoolClass)
| RValue.Char   x   -> (Char   x, CharClass)
| RValue.Uint   x   -> (Uint   x, UintClass)
| RValue.Int    x   -> (Int    x, IntClass)
| RValue.Float  x   -> (Float  x, FloatClass)
| RValue.String x   -> (String x, StringClass)
| RValue.Symbol x   -> (Symbol (String.Intern x), SymbolClass)
| _ -> failwith "Invalid RValue"

let rec eval (vmc: VMContext) ((e, p): Expr) =
    match e with
    | ExprValue.Nil         -> NilValue
    | ExprValue.Const x     -> c2o x
    | ExprValue.Symbol x    ->
        let mutable rv = ref NilValue
        if vmc.stack.Count = 0
        then
            if vmc.global_symbol_table.TryGetValue(x, rv)
            then (!rv)
            else raise (SymbolNotFoundException x)
        else LoadSymbol vmc (vmc.stack.Peek()) x
    | ExprValue.Set (s, e) ->
        let valu = eval vmc e
        let self =
            match vmc.stack.Peek() with
            | Vm x -> x.self
            | Native x -> x.self
        SetItem self s valu
    | ExprValue.Let (s, e) ->
        let valu = eval vmc e
        let self =
            match vmc.stack.Peek() with
            | Vm x -> x.self
            | Native x -> x.self
        BindLocal vmc s valu
    | ExprValue.Use (s, e, es) ->
        let valu = eval vmc e
        BindLocal vmc s (valu) |> ignore
        let r = es |> List.map (fun x -> eval vmc x)
        ObjectDelete valu
        UnBindLocal (vmc.stack.Peek()) s
        List.last r
    | ExprValue.Lambda (ss, es) ->
        (Object.MFunction (Lambda {
            modu_path   = vmc.this_module_path
            father_env  =
                if vmc.stack.Count = 0
                then None
                else Some (vmc.stack.Peek())
            arg_table   = ss
            body        = es
        }), ClosureClass)
    | ExprValue.MethodCall (e, s, es) ->
        let self = eval vmc e
        let args = es |> List.map (fun x -> eval vmc x)
        Call vmc self s args


and Call (vmc: VMContext) ((s, c): Value) (method: string) (args: Value list) =
        match GetMethod c method with
        | MFunction.Lambda x ->
            if x.arg_table.Length = args.Length
            then
                let fc = {
                    funbody= x
                    env = MDict()
                    self = (s, c)
                    super=
                        match c.father with
                        | Some x ->
                            match x.meta_class with
                            | Some y -> Some (Class x, y)
                            | None -> None
                        | None -> None
                    }
                vmc.stack.Push(FunctionContext.Vm fc)
                // load names
                List.iter2 (fun x y -> fc.env.Add(x, y)) x.arg_table args
                // run body
                let r = (List.map (fun x -> eval vmc x) x.body)
                vmc.stack.Pop() |> ignore
                if r.Length = 0
                then NilValue
                else List.last r
            else raise FunctionParameterException
            
        | MFunction.Native (n, b) ->
            vmc.stack.Push(Native {
                funbody=(n, b)
                self= (s, c)
                super=
                    match c.father with
                    | Some x ->
                        match x.meta_class with
                        | Some y -> Some (Class x, y)
                        | None -> None
                    | None -> None
            })
            let r = b vmc (s, c) (List.toArray args)
            vmc.stack.Pop() |> ignore
            r

// -- sys method registered --

let getMataclass c =
    match c.meta_class with
    | Some x -> x
    | _ -> raise MataClassIsNoneException

let subclass (_vmc: VMContext) ((_, c): Value) (x: string) =
    {
        type_name=  x
        modu_path=  c.modu_path
        father=     Some c
        meta_class= Some {
            type_name=  "mata." + x
            modu_path=  c.modu_path
            father=     Some c
            meta_class= None
            methods=    Dictionary<string, MFunction>()
        }
        methods=    Dictionary<string, MFunction>()
    }

let AddSysFunction c name f = c.methods.Add(name ,MFunction.Native("sys." + name, f))

let BaseObjectsInit (vmc: VMContext) =
    // -- base object class init --
    ObjectClass.father      <- Some ObjectClass
    ObjectClass.meta_class  <- Some ObjectClass
    // -- subclass init --
    CharClass   <- subclass vmc (Nil, ObjectClass) "Char"
    BoolClass   <- subclass vmc (Nil, ObjectClass) "Bool"
    UintClass   <- subclass vmc (Nil, ObjectClass) "Uint"
    IntClass    <- subclass vmc (Nil, ObjectClass) "Int"
    FloatClass  <- subclass vmc (Nil, ObjectClass) "Float"
    StringClass <- subclass vmc (Nil, ObjectClass) "String"
    SymbolClass <- subclass vmc (Nil, ObjectClass) "Symbol"
    ClosureClass<- subclass vmc (Nil, ObjectClass) "Closure"
    // -- add ObjectClass method --
    AddSysFunction ObjectClass "new" (
        fun _vmc self args ->
            if args.Length = 0
            then raise FunctionParameterException
            else (MutableObject (MDict()), ObjectClass))
    AddSysFunction ObjectClass "type" (
        fun _vmc (v, t) args ->
            match t.meta_class with
            | Some x -> (Class t, x)
            | None -> raise MataClassIsNoneException)
    AddSysFunction ObjectClass "subclass" (
        fun vmc (v, c) args ->
            if args.Length = 1
            then
                match args.[0] with
                | (Symbol x, _) -> 
                    let c = subclass vmc (v, c) x
                    match c.meta_class with
                    | Some x -> (Class c, x)
                    | _ -> raise MataClassIsNoneException
                | _ -> raise FunctionParameterException
            else raise FunctionParameterException)
    AddSysFunction ObjectClass "toString" (
        fun _vmc (c, v) args ->
            if args.Length = 0
            then
                let str =
                    match c with
                    | Object.Class x ->
                        "<Class " + x.modu_path + "." + x.type_name + ">"
                    | Object.MFunction x ->
                        match x with
                        | MFunction.Lambda x ->
                            "<'" + x.modu_path + "'.Lambda>"
                        | MFunction.Native (n, b) ->
                            "<Native '" + n + "' " + b.ToString() + ">"
                    | _ -> c.ToString()
                (String str, StringClass)
            else raise FunctionParameterException)
    AddSysFunction ObjectClass "print" (
        fun _vmc self args ->
            if args.Length = 0
            then
                match Call vmc self "toString" (Array.toList args) with
                | (String x, y) -> printf "%s" x; NilValue
                | _ -> raise TypeException
            else raise FunctionParameterException)
    AddSysFunction ObjectClass "debug.print" (
        fun _vmc self args ->
            if args.Length = 0
            then printfn "%A" self; NilValue
            else raise FunctionParameterException)

///------------------------------------------------



let inter (x: string) =
    use fh = new StreamReader(x)
    let input = fh.ReadToEnd()
    let mutable ms = createStrStream input
    let mutable vmc = {
        this_module_path        = x
        main_module_path        = x
        global_symbol_table     = MDict()
        stack                   = Stack<FunctionContext>()
        safe_point              = true}
    BaseObjectsInit vmc
    vmc.global_symbol_table.Add("object", (Class ObjectClass, ObjectClass))

    while ms.size < ms.valu.Length-1 do
        match Some (ms) |> MaltParser with
        | Some (e, n) ->
            e |> Prec |> (eval vmc) |> ignore
            ms <- n
        | None -> failwithf "compiler error in %s Ln:%i Cos: %i" x ms.pos.line ms.pos.col

[<EntryPoint>]
let main argv =
    printfn "Hello Malt Zinc"
    argv |> Array.iter (fun x -> inter x)
    //inter ""
    0 // return an integer exit code
