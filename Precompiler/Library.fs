namespace Precompiler

module Precompiler =
    open MParser.MParser
    open LParserC.LParserC

    type Body =
    | Atom
    | Let of (RValue * BodyNode * BodyNode list)
    | Use of (FunctionConst * string * BodyNode * BodyNode list)
    | Cond of (BodyNode * BodyNode) list
    //| Match of (Body)
    | Quote of RValue
    | Lambda of FunctionConst
    | DyLoad of string
    | FunCall
    //| MacroCall

    and BodyNode = (Pos * Body)

    and FunctionConst = (string * MataTree * BodyNode)

    type MacroConst = FunctionConst

    (*
    type MacroCall = {
        callname: string
        parameter: MataTree[]
    }
    *)

    type typeLabel =
    | Any
    | Nil
    | Int
    | Uint
    | Float
    | Bool
    | Char
    | String
    | List of typeLabel
    | Tuple of typeLabel list
    //| Struct of string
    | Function of (typeLabel * typeLabel)

    type TopAst =
    | Nil
    | Import of string list
    | Load of string list
    | Export of string list
    | Defun of FunctionConst
    | DefMacro of MacroConst
    | TypeLabel of typeLabel
    | MacroCall of (string * MataTree)

    type rootNode = {
        pos: LParserC.LParserC.Pos
        ast: TopAst
    }

    type compilerContext = {
        a: int
    }

    (*
    // macro eval
    let pureEval (context: CompilerContext) (args: MataTree) =
        0
    *)

    type InvalidTypeLabel(pos: Pos) = inherit System.ApplicationException()
    type ParamerIsNotSymbol(pos: Pos) = inherit System.ApplicationException()   
    type InvalidTopLevelExpr(pos: Pos) = inherit System.ApplicationException()

    let rec getTypeLabel (args: MataTree) =
        match args.valu with
        | Symbol (s) when s = "any" -> typeLabel.Any
        | Symbol (s) when s = "nil" -> typeLabel.Nil
        | Symbol (s) when s = "int" -> typeLabel.Int
        | Symbol (s) when s = "uint" -> typeLabel.Uint
        | Symbol (s) when s = "float" -> typeLabel.Float
        | Symbol (s) when s = "bool" -> typeLabel.Bool
        | Symbol (s) when s = "char" -> typeLabel.Char
        | Symbol (s) when s = "string" -> typeLabel.String
        | RValue.List ([{valu=Symbol("list")}; t]) ->
            typeLabel.List (getTypeLabel t)
        | RValue.List ([{valu=Symbol("fun")}; pt; rt]) ->
            typeLabel.Function ((getTypeLabel pt), (getTypeLabel rt))
        | RValue.List (t) ->
            typeLabel.Tuple ((List.map getTypeLabel) t)
        | _ -> raise (InvalidTypeLabel args.pos)

    let getSymbol (args: MataTree) =
        match args.valu with
        | Symbol (s) -> s
        | _ -> raise (ParamerIsNotSymbol args.pos)

    let prec (context: compilerContext) (args: MataTree): rootNode =
        match args.valu with
        | RValue.Nil -> {
            pos= args.pos;
            ast = Nil}
        | RValue.List ({valu=Symbol("import")}::t) -> {
            pos= args.pos;
            ast= Import ((List.map getSymbol) t)}
        | RValue.List ({valu=Symbol("load")}::t) -> {
            pos= args.pos;
            ast= Load ((List.map getSymbol) t)}
        | RValue.List ({valu=Symbol("export")}::t) -> {
            pos= args.pos;
            ast= Export ((List.map getSymbol) t)}
        | RValue.List ([{valu=Symbol("type")}; tp]) -> {
            pos= args.pos;
            ast= TypeLabel (getTypeLabel tp)}
        // TODO:...
        | RValue.List ({valu=Symbol(macroName)}::t) -> {
            pos= args.pos;
            ast= MacroCall (macroName, {pos= args.pos; valu= RValue.List t})}
        | _ -> raise (InvalidTopLevelExpr args.pos)