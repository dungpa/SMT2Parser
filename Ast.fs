module SMT2.Ast

type Num = int

type Sconstant =
    | Numeral of Num
    | Decimal of float
    | Hexadecimal of Num
    | Binary of Num
    | String of string
    
type Symbol = Symbol of string

type Keyword = Keyword of string

type Sexpression =
    | Const of Sconstant
    | Sb of Symbol
    | Kw of Keyword
    | List of Sexpression list

type Identifier =
    | Id of Symbol
    | IndexedId of Symbol * Num list

type AttrVal = 
    | AttrConst of Sconstant
    | AttrSym of Symbol
    | AttrSexp of Sexpression list
    
type Attribute = 
    | Attr of Keyword
    | CompAttr of Keyword * AttrVal

type Sort =
    | Sort of Identifier
    | CompSort of Identifier * Sort list

type QualIdent = 
    | QualIdent of Identifier
    | CompQualIdent of Identifier * Sort

type SortedVar = SortedVar of Symbol * Sort

type Term = 
    | ConstTerm of Sconstant
    | QualTerm of QualIdent
    | CompQualTerm of QualIdent * Term list
    | Let of VarBinding list * Term
    | Forall of SortedVar list * Term
    | Exists of SortedVar list * Term
    | AttrTerm of Term * Attribute list

and VarBinding = VarBinding of Symbol * Term

type BoolConfigType =
    | PrintSuccess
    | ExpandDefinition
    | InteractiveMode
    | ProduceProofs
    | ProduceUnsatCore
    | ProduceModels
    | ProduceAssignments

type StringConfigType =
    | RegularOutputChannel
    | DiagnosticOutputChannel

type NumeralConfigType =
    | RandomSeed
    | Verbosity

type Option =
    | BoolConfig of BoolConfigType * bool
    | StringConfig of StringConfigType * string
    | NumeralConfig of NumeralConfigType * Num
    | AttrOption of Attribute

type Flag = 
    | ErrorBehaviour
    | Name
    | Authors
    | Version
    | Status
    | ReasonUnknown
    | AllStatistics

type InfoFlag =
    | BuiltinFlag of Flag
    | CustomFlag of Keyword

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