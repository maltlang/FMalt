namespace CodeGen

module CodeGen =

    open Precompiler.Precompiler
    open MParser
    open System.Collections.Generic
    open LParserC.LParserC
    open System.Text

    type MethodStruct = {
        a: int
    }

    type ModuleContext = {
        symbol_table: string []
        const_area: MParser.RValue []
        method_area: MethodStruct []
        global_method: MethodStruct
    }

    type OpValue =
    | Val1v of uint64
    | Val2v of (uint32 * uint32)

    type OpCode = (uint32 * byte * OpValue)

    // opcodes.rawMir
    let load_const          = byte 0
    let load_lambda         = byte 1
    let load_symbol         = byte 2    
    let write_local_symbol  = byte 3
    let write_object_symbol = byte 4
    let get_method          = byte 5
    let copy_to_shared_area = byte 6
    let delete              = byte 7
    let gc_new              = byte 8
    let call                = byte 9
    let _return             = byte 10

    // opcodes.optimizMir
    let load_local_of_offset    = byte 11
    let load_object_of_offset   = byte 12
    let write_local_of_offset   = byte 13
    let write_object_of_offset  = byte 14
    let get_method_of_offset    = byte 15

    //opcodes.optimizL2Mir
    let throw   = byte 16
    let add     = byte 17
    let sub     = byte 18
    let mul     = byte 19uy
    let div     = byte 20
    let _mod    = byte 21

    // append
    let call_of_offset          = byte 22

    let inline rlsConst env x (p: Pos) =
        Some ({
            symbol_table    = env.symbol_table;
            const_area      = Array.append env.const_area [| x |] ;
            method_area     = env.method_area;
            global_method   = env.global_method},
            [| (p.line, load_const, env.const_area.Length - 1) |])

    let inline rlsSymbol env x (p: Pos) =
        try
                let i = env.symbol_table |> Array.findIndex (fun a -> x = a)
                Some ({
                symbol_table    = env.symbol_table;
                const_area      = env.const_area;
                method_area     = env.method_area;
                global_method   = env.global_method}, [| (p.line, load_symbol, i) |] )
            with
            | :? KeyNotFoundException ->
                Some ({
                symbol_table    = Array.append env.symbol_table [| x |];
                const_area      = env.const_area;
                method_area     = env.method_area;
                global_method   = env.global_method}, [| (p.line, load_symbol, env.symbol_table.Length - 1) |])

    let rec CodeGenerator (env: ModuleContext) ((e, p): Expr) =
        match e with
        | ExprValue.Nil -> None
        | ExprValue.Const (x) ->
            rlsConst env x p
        | ExprValue.Symbol (x) ->
            rlsSymbol env x p
        | ExprValue.Set (name, expr) ->
            match CodeGenerator env expr with
            | Some (env, codes) ->
                let (env, codes2)   = (rlsSymbol env name p).Value
                let (_, _, offset)  = Array.get codes2 0
                Some (env, (Array.append codes [| (p.line, write_object_symbol, offset) |]))
            | None ->
                failwithf "CodeGenError: '(set <id> <expr>)' expr is None: %i,%i"
                          p.line p.col
        | ExprValue.Let (name, expr) ->
            match CodeGenerator env expr with
            | Some (env, codes) ->
                let (env, codes2)   = (rlsSymbol env name p).Value
                let (_, _, offset)  = Array.get codes2 0
                Some (env, (Array.append codes [| (p.line, write_local_symbol, offset) |]))
            | None ->
                failwithf "CodeGenError: '(let <id> <expr>)' expr is None: %i,%i"
                          p.line p.col
        | ExprValue.Use (name, expr, exprs) ->
            match CodeGenerator env expr with
            | Some (env, codes) ->
                let (env, codes2)   = (rlsSymbol env name p).Value
                let (_, _, offset)  = Array.get codes2 0
                // 处理exprs是不是Some
                let proc1 p x =
                    match x with
                        | Some (x) -> x
                        | _ -> failwithf "CodeGenError: '(Use <id> <expr> <expr>*)' expr is None: %i,%i"
                                p.line p.col

                let rec proc2 codes =
                    match codes with
                    | [(_, r)] -> r
                    | (_, codes) :: b -> Array.append codes (proc2 b)
                    | _ -> failwith "???"

                let exprsResult =
                    exprs
                    |> List.map (CodeGenerator env)
                    |> List.map (proc1 p)
                    |> proc2
                
                let lcode = [| (p.line, write_local_symbol, offset) |]
                let dcode = [| (p.line, delete, offset) |]
                // 写变量
                let readcode = Array.append codes lcode
                // 过程与delete
                let proccode = Array.append exprsResult dcode
                
                Some (env, Array.append readcode proccode)
            | None ->
                failwithf "CodeGenError: '(Use <id> <expr> <expr>*)' expr is None: %i,%i"
                          p.line p.col
        
        | _ -> None
    