namespace CodeGen

//#pragma warning disable 25

module CodeGen =

    open Precompiler.Precompiler
    open MParser
    open System.Collections.Generic
    open LParserC.LParserC
    open System.Text

    type OpValue =
    | Val1v of uint64
    | Val2v of (uint32 * uint32)

    type OpCode = (uint32 * byte * OpValue)

    type LambdaStruct = {
        local_name_table: string list
        bytecodes: OpCode []
    }

    type ModuleContext = {
        symbol_table: string []
        const_area: MParser.RValue []
        method_area: LambdaStruct []
        global_method: LambdaStruct
    }

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
    let mul     = byte 19
    let div     = byte 20
    let _mod    = byte 21

    // append
    let call_of_offset          = byte 22

    let inline rlsConst env x (p: Pos) =
        {
            symbol_table    = env.symbol_table;
            const_area      = Array.append env.const_area [| x |] ;
            method_area     = env.method_area;
            global_method   = env.global_method},
        [| (uint32(p.line), load_const, Val1v (uint64(env.const_area.Length - 1))) |]


    let inline rlsSymbol env x (p: Pos) =
        try
                let i = env.symbol_table |> Array.findIndex (fun a -> x = a)
                {
                symbol_table    = env.symbol_table;
                const_area      = env.const_area;
                method_area     = env.method_area;
                global_method   = env.global_method}, [| (uint32(p.line), load_symbol, Val1v(uint64(i))) |]
            with
            | :? KeyNotFoundException ->
                {
                symbol_table    = Array.append env.symbol_table [| x |];
                const_area      = env.const_area;
                method_area     = env.method_area;
                global_method   = env.global_method}, [| (uint32(p.line), load_symbol, Val1v(uint64(env.symbol_table.Length - 1))) |]

    let rec CodeGenerator (env: ModuleContext) ((e, p): Expr): (ModuleContext * OpCode[]) =
        match e with
        | ExprValue.Const (x) ->
            rlsConst env x p
        | ExprValue.Symbol (x) ->
            rlsSymbol env x p
        | ExprValue.Set (name, expr) ->
            let (env, codes)    = CodeGenerator env expr
            let (env, codes2)   = (rlsSymbol env name p)
            let (_, _, offset)  = Array.get codes2 0
            env,
            (Array.append codes [| (uint32(p.line), write_object_symbol, offset) |])
        | ExprValue.Let (name, expr) ->
            let (env, codes)    = CodeGenerator env expr
            let (env, codes2)   = (rlsSymbol env name p)
            let (_, _, offset)  = Array.get codes2 0
            env,
            (Array.append codes [| (uint32(p.line), write_local_symbol, offset) |])
        | ExprValue.Use (name, expr, exprs) ->
            let (env, codes)    = CodeGenerator env expr
            let (env, codes2)   = (rlsSymbol env name p)
            let (_, _, offset)  = Array.get codes2 0
            (*// 处理exprs是不是Some
            let proc1 p x =
                match x with
                    | Some (x) -> x
                    | _ -> failwithf "CodeGenError: '(use <id> <expr> <expr>*)' expr is None: %i,%i"
                            p.line p.col
                            *)

            let rec proc2 codes =
                match codes with
                | [(_, r)] -> r
                | (_, codes) :: b -> Array.append codes (proc2 b)
                | _ -> failwith "???"

            let exprsResult =
                exprs
                |> List.map (CodeGenerator env)
                //|> List.map (proc1 p)
                |> proc2
            
            let lcode = [| (uint32(p.line), write_local_symbol, offset) |]
            let dcode = [| (uint32(p.line), delete, offset) |]
            // 写变量
            let readcode = Array.append codes lcode
            // 过程与delete
            let proccode = Array.append exprsResult dcode
            
            (env, Array.append readcode proccode)
        | ExprValue.Lambda (names, exprs) ->
            let bcs = List.map (CodeGenerator env) exprs
            let rec proc2 codes =
                match codes with
                | [(_, r)] -> r
                | (_, codes) :: b -> Array.append codes (proc2 b)
                | _ -> failwith "???"

            let stru = {
                local_name_table = names
                bytecodes = proc2 bcs}

            let new_method_area = Array.append env.method_area [| stru |]

            {
            symbol_table    = env.symbol_table;
            const_area      = env.const_area;
            method_area     = new_method_area;
            global_method   = env.global_method}
            , [| (uint32 p.line, load_lambda, Val1v (uint64 (Array.length new_method_area))) |]
        | ExprValue.MethodCall (expr, name, exprs) ->
            let (env, codes)    = CodeGenerator env expr
            let (env, codes2)   = (rlsSymbol env name p)
            let (_, _, offset)  = Array.get codes2 0

            let rec proc2 codes =
                match codes with
                | [(_, r)] -> r
                | (_, codes) :: b -> Array.append codes (proc2 b)
                | _ -> failwith "???"

            let exprsResult =
                exprs
                |> List.map (CodeGenerator env)
                //|> List.map (proc1 p)
                |> proc2
            
            // 两块求值过程
            let run1 = Array.append codes2 exprsResult
            
            let (Val1v (a)) = offset

            env,
            Array.append run1 
                [| (uint32(p.line), call, Val2v (uint32(a), uint32(exprs.Length))) |]
        | _ -> failwith "error"
    