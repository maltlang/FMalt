namespace Precompiler

module Precompiler =
    open MParser.MParser
    open LParserC.LParserC
    //open Map

    type InvalidTypeLabel(pos: Pos) = inherit System.ApplicationException()
    type ParamerIsNotSymbol(pos: Pos) = inherit System.ApplicationException()   
    type InvalidTopLevelExpr(pos: Pos) = inherit System.ApplicationException()

    type Body =
    | Const of RValue
    | Symbol of string
    | Let of (RValue * BodyNode * BodyNode list)
    | Use of (Body * string * BodyNode * BodyNode list)
    | Cond of (BodyNode * BodyNode) list
    | Match of (RValue * Body) list
    | Lambda of (MataTree * BodyNode)
    | DyLoad of string
    | FunCall of (string * BodyNode list)
    | MacroCall of (string * BodyNode list)

    and BodyNode = (Pos * Body)

    let rec getBodyNode (args: MataTree) =
        match args.valu with
        | RValue.Symbol (s) -> (args.pos, Symbol(s))
        | x -> (args.pos, Const (x))

    // type check 先做个壳子，其它的暂时没精力做
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
        | (Or (x1, x2), Or (y1, y2)) ->
            (Equ x1 y1 && Equ x2 y2) || (Equ x1 y2 && Equ x2 y1) // FIXME
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
    | Const of RValue
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

    // file struct
    type mModule = {
        macroTable: Map<string, int>
        funcTable: Map<string, int>
    }

    type compilerContext = {
        moduleTable: Map<string, mModule>
    }

    let rec getTypeLabel (args: MataTree) =
        match args.valu with
        | RValue.Symbol (s) when s = "any" -> typeLabel.Any
        | RValue.Symbol (s) when s = "nil" -> typeLabel.Nil
        | RValue.Symbol (s) when s = "int" -> typeLabel.Int
        | RValue.Symbol (s) when s = "uint" -> typeLabel.Uint
        | RValue.Symbol (s) when s = "float" -> typeLabel.Float
        | RValue.Symbol (s) when s = "bool" -> typeLabel.Bool
        | RValue.Symbol (s) when s = "char" -> typeLabel.Char
        | RValue.Symbol (s) when s = "string" -> typeLabel.String
        | RValue.List ([{valu=RValue.Symbol("|")}; t1; t2]) ->
            typeLabel.Or ((getTypeLabel t1), (getTypeLabel t2))
        | RValue.List ([{valu=RValue.Symbol("list")}; t]) ->
            typeLabel.List (getTypeLabel t)
        | RValue.List ([{valu=RValue.Symbol("fun")}; pt; rt]) ->
            typeLabel.Function ((getTypeLabel pt), (getTypeLabel rt))
        | RValue.List (t) ->
            typeLabel.Tuple ((List.map getTypeLabel) t)
        | _ -> raise (InvalidTypeLabel args.pos)

    let getSymbol (args: MataTree) =
        match args.valu with
        | RValue.Symbol (s) -> s
        | _ -> raise (ParamerIsNotSymbol args.pos)

    let prec (context: compilerContext) (args: MataTree): (compilerContext * rootNode) =
        match args.valu with
        | RValue.Nil -> (context,{
            pos= args.pos;
            ast = Nil})
        | RValue.List ({valu=RValue.Symbol("import")}::t) -> (context,{
            pos= args.pos;
            ast= Import ((List.map getSymbol) t)})
        | RValue.List ({valu=RValue.Symbol("load")}::t) -> (context,{
            pos= args.pos;
            ast= Load ((List.map getSymbol) t)})
        | RValue.List ({valu=RValue.Symbol("export")}::t) -> (context,{
            pos= args.pos;
            ast= Export ((List.map getSymbol) t)})
        | RValue.List ([{valu=RValue.Symbol("type")}; tp]) -> (context,{
            pos= args.pos;
            ast= TypeLabel (getTypeLabel tp)})
        | RValue.List ({valu=RValue.Symbol("fun")} :: {valu=RValue.Symbol(name)} :: p ::t) ->
            (context,{
            pos= args.pos;
            ast= Defun (name, p, (List.map getBodyNode) t)})
        | RValue.List ({valu=RValue.Symbol("macro")} :: {valu=RValue.Symbol(name)} :: p ::t) ->
            (context,{
            pos= args.pos;
            ast= Defun (name, p, (List.map getBodyNode) t)})
        | RValue.List ({valu=RValue.Symbol(macroName)}::t) ->
            (context,{
            pos= args.pos;
            ast= MacroCall (macroName, {pos= args.pos; valu= RValue.List t})})
        | _ -> raise (InvalidTopLevelExpr args.pos)