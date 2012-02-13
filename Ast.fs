module SMT2Parser.Ast

open System

let inline lst2Str (xs: _ list) = String.Join(" ", xs |> List.map string)

type Num = bigint

type Sconstant =
    | Numeral of Num
    | Decimal of decimal
    | Hexadecimal of Num
    | Binary of Num
    | String of string
with override sc.ToString() =
        match sc with
        | Numeral n | Hexadecimal n | Binary n -> sprintf "%s" (string n) // May output to appropriate formats
        | Decimal f -> sprintf "%.1f" f        
        | String s -> sprintf "\"%s\"" s
    
type Symbol = Symbol of string
with override sb.ToString() =
        match sb with
        | Symbol s -> s

type Keyword = Keyword of string
with override kw.ToString() =
        match kw with
        | Keyword s -> s

type Sexpression =
    | Const of Sconstant
    | Sb of Symbol
    | Kw of Keyword
    | List of Sexpression list
with override av.ToString() =
        match av with
        | Const sc -> string sc
        | Sb sb -> string sb
        | Kw kw -> string kw
        | List ss -> sprintf "(%s)" (lst2Str ss)

type Identifier =
    | Id of Symbol
    | IndexedId of Symbol * Num list
with override id.ToString() =
        match id with
        | Id sb -> string sb
        | IndexedId(sb, ns) -> sprintf "(_ %s %s)" (string sb) (lst2Str ns)

type AttrVal = 
    | AttrConst of Sconstant
    | AttrSym of Symbol
    | AttrSexp of Sexpression list
with override av.ToString() =
        match av with
        | AttrConst sc -> string sc
        | AttrSym sb -> string sb
        | AttrSexp ss -> sprintf "%s" (lst2Str ss)
            
type Attribute = 
    | Attr of Keyword
    | CompAttr of Keyword * AttrVal
with override a.ToString() =
        match a with
        | Attr kw -> string kw
        | CompAttr(kw, av) -> sprintf "%s %s" (string kw) (string av)

type Sort =
    | Sort of Identifier
    | CompSort of Identifier * Sort list
with override s.ToString() =
        match s with
        | Sort id -> string id
        | CompSort(id, ss) -> sprintf "(%s %s)" (string id) (lst2Str ss)

type QualIdent = 
    | QualIdent of Identifier
    | CompQualIdent of Identifier * Sort
with override qi.ToString() =
        match qi with
        | QualIdent id -> string id
        | CompQualIdent(id, s) -> sprintf "(as %s %s)" (string id) (string s)

type SortedVar = SortedVar of Symbol * Sort
with override sv.ToString() =
        match sv with
        | SortedVar(s1, s2) -> sprintf "(%s %s)" (string s1) (string s2)

type Term = 
    | ConstTerm of Sconstant
    | QualTerm of QualIdent
    | CompQualTerm of QualIdent * Term list
    | Let of VarBinding list * Term
    | Forall of SortedVar list * Term
    | Exists of SortedVar list * Term
    | AttrTerm of Term * Attribute list
with override t.ToString() =
        match t with
        | ConstTerm sc -> string sc
        | QualTerm qi -> string qi
        | CompQualTerm(qi, ts) -> sprintf "(%s %s)" (string qi) (lst2Str ts)
        | Let(vbs, t) -> sprintf "(let (%s) %s)" (lst2Str vbs) (string t)
        | Forall(svs, t) -> sprintf "(forall (%s) %s)" (lst2Str svs) (string t)
        | Exists(svs, t) -> sprintf "(exists (%s) %s)" (lst2Str svs) (string t)
        | AttrTerm(t, ats) -> sprintf "(! %s %s)" (string t) (lst2Str ats)

and VarBinding = VarBinding of Symbol * Term
    with override vb.ToString() =
            match vb with
            | VarBinding(s, t) -> sprintf "(%s %s)" (string s) (string t)

type BCT =
    | ``:print-success`` = 0
    | ``:expand-definitions`` = 1
    | ``:interactive-mode`` = 2
    | ``:produce-proofs`` = 3
    | ``:produce-unsat-cores`` = 4
    | ``:produce-models``= 5
    | ``:produce-assignments`` = 6

type SCT =
    | ``:regular-output-channel`` = 0
    | ``:diagnostic-output-channel`` = 1

type NCT =
    | ``:random-seed`` = 0
    | ``:verbosity`` = 1

type Option =
    | BoolConfig of BCT * bool
    | StringConfig of SCT * string
    | NumeralConfig of NCT * Num
    | AttrOption of Attribute
with override o.ToString() =
        match o with
        | BoolConfig(bct, b) -> sprintf "%s %b" (string bct) b
        | StringConfig(sct, s) -> sprintf "%s \"%s\"" (string sct) s
        | NumeralConfig(nct, n) -> sprintf "%s %s" (string nct) (string n)
        | AttrOption ao -> string ao

type Flag = 
    | ``:error-behaviour`` = 0
    | ``:name`` = 1
    | ``:authors`` = 2
    | ``:version`` = 3
    | ``:status`` = 4
    | ``:reason-unknown`` = 5
    | ``:all-statistics`` = 6

type InfoFlag =
    | BuiltinFlag of Flag
    | CustomFlag of Keyword
with override inf.ToString() =
        match inf with
        | BuiltinFlag f -> string f
        | CustomFlag kw -> string kw

type Command =
    | SetLogic of Symbol
    | SetOption of Option
    | SetInfo of Attribute
    | DeclareSort of Symbol * Num
    | DefineSort of Symbol * Symbol list * Sort
    | DeclareFun of Symbol * Sort list * Sort
    | DefineFun of Symbol * SortedVar list * Sort * Term
    | Push of Num
    | Pop of Num
    | Assert of Term    
    | CheckSat
    | GetAssertions
    | GetProof
    | GetUnsatCore
    | GetValue of Term list
    | GetAssignment
    | GetOption of Keyword
    | GetInfo of InfoFlag
    | Exit
with override c.ToString() =
        match c with
        | SetLogic s -> sprintf "(set-logic %s)" <| string s
        | SetOption o -> sprintf "(set-option %s)" <| string o
        | SetInfo i -> sprintf "(set-info %s)" <| string i
        | DeclareSort(s, n) -> sprintf "(declare-sort %s %s)" (string s) (string n)
        | DefineSort(s1, ss, s2) -> sprintf "(define-sort %s (%s) %s)" (string s1) (lst2Str ss) (string s2)
        | DeclareFun(s1, ss, s2) -> sprintf "(declare-fun %s (%s) %s)" (string s1) (lst2Str ss) (string s2)
        | DefineFun(s1, svs, s2, t) -> sprintf "(define-fun %s (%s) %s %s)" (string s1) (lst2Str svs) (string s2) (string t)
        | Push n -> sprintf "(push %s)" (string n)
        | Pop n -> sprintf "(pop %s)" (string n)
        | Assert t -> sprintf "(assert %s)" <| string t
        | CheckSat -> "(check-sat)"
        | GetAssertions -> "(get-assertions)"
        | GetProof -> "(get-proof)"
        | GetUnsatCore -> "(get-unsat-core)"
        | GetValue ts -> sprintf "(get-value (%s))" <| lst2Str ts
        | GetAssignment -> "(get-assignment)"
        | GetOption kw -> sprintf "(get-option %s)" <| string kw
        | GetInfo inf -> sprintf "(get-info %s)" <| string inf
        | Exit -> "(exit)"