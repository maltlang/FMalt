namespace Precompiler

module Precompiler =
    open MParser.MParser
    open LParserC.LParserC

    type Body =
    | Atom
    | Let of (RValue * BodyNode * BodyNode[])
    | Use of (FunctionConst * string * BodyNode * BodyNode[])
    | Cond of (BodyNode * BodyNode)[]
    | Match of (Body)
    | Lambda of FunctionConst
    | DyLoad of string
    | FunCall
    //| MacroCall

    and BodyNode = {
        pos: Pos
        body: Body
    }

    and FunctionConst = {
        name: string
        args: MataTree
        body: BodyNode[]
    }

    type MacroConst = {
        name: string
        args: MataTree
        body: BodyNode[]
    }

    (*
    type MacroCall = {
        callname: string
        parameter: MataTree[]
    }
    *)

    type TopAst =
    | StLoad of string
    | DyLoad of string
    | Export of string[]
    | Defun of FunctionConst
    | DefMacro of MacroConst
    | TypeLabel of (string * RValue)
    //| MacroCall of MacroCall

    type RootNode = {
        pos: LParserC.LParserC.Pos
        ast: TopAst
    }

    type CompilerContext = {
        a: int
    }

    (*
    // macro eval
    let pureEval (context: CompilerContext) (args: MataTree) =
        0
    *)

    // ����F#û��ƥ�䲻��ȫ��list���һ��������Parser����ķ�ʽ��дԤ�����������match

    let prec (context: CompilerContext) (args: MataTree)(*: RootNode*)=
        match args with
        | {valu=List([{valu=Symbol("static-load")}; sym])} -> 0
        | {valu=List([{valu=Symbol("dynamic-load")}; sym])} -> 0
        | {valu=List([{valu=Symbol("export")}; {valu=List(x)}])} as t -> 0
        | {valu=List([{valu=Symbol("type")}; argt; rett])} -> 0
        | {valu=List([{valu=Symbol("macro")}; args; body])} -> 0
        | {valu=List([{valu=Symbol("def")}; {valu=Symbol(name)}; argt])} -> 0
        | _ -> 0
