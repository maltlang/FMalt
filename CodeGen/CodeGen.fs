namespace CodeGen

//#pragma warning disable 25

module CodeGen =

    open LParserC.LParserC
    open MParser.MParser
    open Precompiler.Precompiler
    
    (*
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
    *)

    // opcodes.rawMir
    let load_const          = byte 0
    let load_lambda         = byte 1
    let load_symbol         = byte 2    
    let write_local_symbol  = byte 3
    let write_object_symbol = byte 4
    let get_method          = byte 5
    let push_env_context    = byte 6
    let _delete              = byte 7
    //let gc_new              = byte 8
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

    let call_of_offset          = byte 22

    let boxes p str =
        "("
        + p.line.ToString ()
        + ":"
        + p.col.ToString ()
        + " "
        + str + ")\n"

    type ModuleContext = {
        path:           string
        lambdas:        string array ref
        global_lambda:  string
    }

    let rec CodeGen2WordCode (m: ModuleContext) ((e, p): Expr) =
        match e with
        | Const x -> boxes p (load_const.ToString () + " " + x.ToString ())
        | Symbol x -> boxes p (load_symbol.ToString () + " " + x)
        | Set (s, e) -> (CodeGen2WordCode m e) + boxes p (write_object_symbol.ToString () + " " + s)
        | Let (s, e) -> (CodeGen2WordCode m e) + boxes p (write_local_symbol.ToString () + " " + s)
        | Use (s, e, es) ->
            (CodeGen2WordCode m e)
            + boxes p (push_env_context.ToString ())
            + boxes p (write_local_symbol.ToString () + " " + s)
            + if es.Length = 0 then "" else List.reduce (+) (List.map (CodeGen2WordCode m) es)
            + boxes p (_delete.ToString () + " " + s)
        | MethodCall (e, n, es) ->
            CodeGen2WordCode m e
            + boxes p (get_method.ToString () + " " + n.ToString ())
            + if es.Length = 0 then "" else List.reduce (+) (List.map (CodeGen2WordCode m) es)
            + boxes p (call.ToString () + " " + (es.Length + 1).ToString ())
        | Lambda (ss, es) ->
            let r = (
                if es.Length = 0
                then ""
                else List.reduce (+) (
                        List.map (
                            fun x -> boxes p (write_local_symbol.ToString () + " " + x)) ss)+
                        if es.Length = 0
                        then ""
                        else List.reduce (+) (List.map (CodeGen2WordCode m) es))
            m.lambdas := (Array.append (! m.lambdas) [| r |])
            boxes p (load_lambda.ToString () + " " + ((!m.lambdas).Length+1).ToString ())
        | _ -> ""
