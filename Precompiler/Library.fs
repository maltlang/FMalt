namespace Precompiler

module Precompiler =
    open MParser.MParser
    open LParserC.LParserC

    type Body =
    | Const of RValue
    | Let of (RValue * BodyNode * BodyNode list)
    | Use of (Body * string * BodyNode * BodyNode list)
    | Cond of (BodyNode * BodyNode) list
    //| Match of (Body)
    | Quote of RValue
    | Lambda of (MataTree * BodyNode)
    | DyLoad of string
    | FunCall
    //| MacroCall

    and BodyNode = (Pos * Body)

    (*
    type MacroCall = {
        callname: string
        parameter: MataTree[]
    }
    *)

    type typeLabel =
    | Any
    | Or of (typeLabel * typeLabel)
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

    let rec Equ self o2 =
        match (self, o2) with
        | (Any, _) | (_, Any) -> true
        | (Or (x1, x2), Or (y1, y2)) -> (Equ x1 y1 && Equ x2 y2) || (Equ x1 y2 && Equ x2 y1)
        | (List (x), List (y)) -> Equ x y
        | (Tuple (x), Tuple (y)) -> rtrt x y
        | (Function (x, y), Function (x2, y2)) -> Equ x y && Equ x2 y2
        | (x, y) when x = y -> true
        | _ -> false

    and rtrt t1 t2 =
        match (t1, t2) with
        | (v::t, v2::t2) ->
            if Equ v v2
            then rtrt t t2
            else false
        | ([], []) -> true
        | _ -> false

    type TopAst =
    | Nil
    | Import of string list
    | Load of string list
    | Export of string list
    | Defun of (string * MataTree * BodyNode list)
    | DefMacro of (string * MataTree * BodyNode list)
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

    let rec getBodyNode (args: MataTree) = 0

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
        | RValue.List ([{valu=Symbol("|")}; t1; t2]) ->
            typeLabel.Or ((getTypeLabel t1), (getTypeLabel t2))
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
            (*
        | RValue.List ({valu=Symbol("fun")} :: {valu=Symbol(name)} :: p ::t) -> {
            pos= args.pos;
            ast= Defun (name, p, (List.map getBodyNode) t)}
        | RValue.List ({valu=Symbol("macro")} :: {valu=Symbol(name)} :: p ::t) -> {
            pos= args.pos;
            ast= Defun (name, p, (List.map getBodyNode) t)}
            *)
        | RValue.List ({valu=Symbol(macroName)}::t) -> {
            pos= args.pos;
            ast= MacroCall (macroName, {pos= args.pos; valu= RValue.List t})}
        | _ -> raise (InvalidTopLevelExpr args.pos)