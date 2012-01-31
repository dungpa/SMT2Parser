module SMT2.Parser

open System.Collections.Generic
open FParsec
open FParsec.Primitives 
open FParsec.CharParsers 
 
open SMT2.Ast

// Utility functions
type SMT2Parser<'a> = Parser<'a, unit> 

let symbol: SMT2Parser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"
let spaces1: SMT2Parser<unit> = skipMany1 spaces 
let chr c = skipChar c 
let endBy p sep = many (p .>> sep)

let str s = pstring s
// Get string and skip whitespaces
let str_ws s = pstring s .>> spaces
let ws_str s = spaces >>. pstring s
let betweenStrings s1 s2 p = str s1 >>. p .>> str s2

// Lexical syntax

let pComment: SMT2Parser<_> = pstring ";" >>. skipRestOfLine true

// spaces will match common whitespaces
let pSpacing: SMT2Parser<_> = skipSepBy spaces pComment

let pEndOfFile = eof

// Numeral | Decimal | Hexadecimal | Binary
let numberFormat = NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowHexadecimal
                   ||| NumberLiteralOptions.AllowBinary

let pNumber = 
    numberLiteral numberFormat "number"
    |>> fun num -> if num.IsHexadecimal then Hexadecimal (int num.String)
                   elif num.IsBinary then Binary (int num.String)
                   elif num.IsInteger then Numeral (int num.String) // why does this match everything?
                   else Decimal (float num.String)

// Undesirable approach
let parseNumber =  
    let isNextDigit c = isDigit c || c = '.'
    let numOrDec = many1Satisfy2 isDigit isNextDigit 
    let hexOrBin = str "#" >>. manyChars (letter <|> digit) |>> fun s -> sprintf "0%s" s
    
    parse {
            let! s = numOrDec <|> hexOrBin
            match run pNumber <| s with
            | Success(result, _, _)   -> return result
            | Failure(_, _, _) -> ()
    }

let pString = 
    betweenStrings "\"" "\"" <| manyChars (noneOf "\"")

let parseString = pString |>> String

let parseConst = parseNumber <|> parseString    

let pSymbol =    
    betweenStrings "|" "|" (manyChars (noneOf "|")) // The order does matter
    <|> manyChars (noneOf "| ()")

let parseSymbol = pSymbol |>> Symbol

// TODO: process keywords
let pKeyword = 
    str ":" >>. manyChars (letter <|> symbol <|> digit)

let parseKeyword = pKeyword |>> Keyword

// You need to refer to parseSexpr from the productions below, hence the forward declaration trick 
let parseSexpr, parseSexprRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
// Just expressions separated by one or more spaces 
let parseSexpList = sepBy parseSexpr (str " ")

// A SMTParser<_> can be one of the below
// TODO: note that the definition for list need to backtrack to disambinguate the two cases 
do parseSexprRef := (parseConst |>> Const)
                    <|> (parseKeyword |>> Kw)
                    <|> (str_ws "(" >>. parseSexpList .>> ws_str ")" |>> List)
                    <|> (parseSymbol |>> Sb)
                    
// TODO: many ugly cases need to be handled.
let parseIdentifier: SMT2Parser<_> =
    (parseSymbol |>> Id)
    <|> parse {
                do! chr '(' 
                do! spaces1 // TODO: should remove it
                do! chr '_'
                do! spaces1
                let! symbol = parseSymbol
                do! spaces1
                let! xs = parseSexpList
                do! chr ')' 
                let ns = xs |> List.map (function | Const(Numeral i) -> i | _ -> failwith "parseIdentifier: Wrong format") 
                match ns with
                | []-> failwith "parseIdentifier: Should have at least one numeral"
                | _ -> return IndexedId (symbol, ns)                 
        }

let parseAttrVal =
    parseConst |>> AttrConst
    <|> (parseSymbol |>> AttrSym) 
    <|> (parseSexpList |>> AttrSexp)

let parseAttribute: SMT2Parser<_> =
    (parseKeyword |>> Attr)
    <|> parse {
                let! kw = parseKeyword
                let! attrVal = parseAttrVal
                return CompAttr(kw, attrVal)
        }

let parseSort, parseSortRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
let pSortList: SMT2Parser<_> = sepBy parseSort spaces1

do parseSortRef :=  (parseIdentifier |>> Sort)
                    <|> parse { 
                            do! chr '('                             
                            let! id = parseIdentifier                            
                            let! xs = pSortList                            
                            do! chr ')' 
                            match xs with
                            | []-> failwith "parseSortRef: should have sort declaration"
                            | _ -> return CompSort(id, xs) 
                        } 

let parseQualIdent: SMT2Parser<_> =
    (parseIdentifier |>> QualIdent)
    <|> parse {
                do! chr '('
                do! skipString "as"                
                let! id = parseIdentifier                
                let! sort = parseSort
                do! chr ')'          
                return CompQualIdent(id, sort) 
        }

let parseSortedVar = 
    parse {
            do! chr '('
            let! symbol = parseSymbol
            let! sort = parseSort
            do! chr ')'
            return SortedVar(symbol, sort)
    }

let parseTerm, parseTermRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
let pTermList: SMT2Parser<_> = sepBy parseTerm spaces1

let parseVarBinding = 
    parse {
            do! chr '('
            let! symbol = parseSymbol
            let! term = parseTerm
            do! chr ')'
            return VarBinding(symbol, term)
    }

do parseTermRef := (parseConst |>> ConstTerm)
                    <|> (parseQualIdent |>> QualTerm)
                    <|> parse { 
                            do! chr '('                             
                            let! qi = parseQualIdent
                            let! ts = pTermList                            
                            do! chr ')' 
                            match ts with
                            | []-> failwith "parseTermRef: should have at least one term"
                            | _ -> return CompQualTerm(qi, ts) 
                        } 
                    <|> parse { 
                            do! chr '('       
                            do! skipString "let"
                            do! chr '('                            
                            let! xs = sepBy parseVarBinding spaces1
                            do! chr ')' 
                            let! term = parseTerm
                            do! chr ')' 
                            match xs with
                            | []-> failwith "parseTermRef: should have at least one sorted var"
                            | _ -> return Let(xs, term) 
                        } 
                    <|> parse { // should merge these two cases (forall, exists) together
                            do! chr '('       
                            do! skipString "forall"
                            do! chr '('                            
                            let! xs = sepBy parseSortedVar spaces1
                            do! chr ')' 
                            let! term = parseTerm
                            do! chr ')' 
                            match xs with
                            | []-> failwith "parseTermRef: should have at least one sorted var"
                            | _ -> return Forall(xs, term) 
                        } 
                    <|> parse { 
                            do! chr '('       
                            do! skipString "exists"
                            do! chr '('                            
                            let! xs = sepBy parseSortedVar spaces1
                            do! chr ')' 
                            let! term = parseTerm
                            do! chr ')' 
                            match xs with
                            | []-> failwith "parseTermRef: should have at least one sorted var"
                            | _ -> return Exists(xs, term) 
                        } 
                    <|> parse {
                            do! chr '('
                            do! chr '!'
                            let! term = parseTerm
                            let! attrs = sepBy parseAttribute spaces1
                            do! chr ')'
                            match attrs with
                            | []-> failwith "parseTermRef: should have at least one attribute"
                            | _ -> return AttrTerm(term, attrs) 
                        }

// NB: currently not support Theory Declarations

// NB: currently not support Logic Declarations

let parseBoolean = (stringReturn "true"  true) <|> (stringReturn "false" false)

let parseOption = 
    (parseAttribute |>> AttrOption)
    <|> parse {
            let! s = pString
            match s with
            | ":print-success" -> let! b = parseBoolean
                                  return BoolConfig(PrintSuccess, b)
            | ":expand-definition" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(ExpandDefinition, b)
            | ":interactive-mode" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(InteractiveMode, b)
            | ":produce-proofs" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(ProduceProofs, b)
            | ":produce-models" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(ProduceModels, b)
            | ":produce-assignments" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(ProduceAssignments, b)
            | "regular-output-channel" ->
                                  let! s' = pString
                                  return StringConfig(RegularOutputChannel, s')
            | "diagnostic-output-channel" ->
                                  let! s' = pString
                                  return StringConfig(DiagnosticOutputChannel, s')
            | "random-seed" ->
                                  let! n = parseNumber
                                  match n with
                                  | Numeral i -> return NumeralConfig(RandomSeed, i)
                                  | _ -> failwith "parseOption: wrong numeral format"
            | "verbosity" ->
                                  let! n = parseNumber
                                  match n with
                                  | Numeral i -> return NumeralConfig(Verbosity, i)
                                  | _ -> failwith "parseOption: wrong numeral format"
            | _ -> failwith "parseOption: unknown config"
    }

let parseInfoFlag = 
    (parseKeyword |>> CustomFlag) // It shouldn't match builtin flags.
    <|> parse {
            let! s = pString
            match s with
            | ":error-behaviour" -> return BuiltinFlag ErrorBehaviour
            | ":name" -> return BuiltinFlag ErrorBehaviour
            | ":version" -> return BuiltinFlag Version
            | ":status" -> return BuiltinFlag Status
            | ":reason-unknown" -> return BuiltinFlag ReasonUnknown
            | ":all-statistics" -> return BuiltinFlag AllStatistics
            | _ -> failwith "parseInfoFlag: unknown flag"
    }

let parseCommand = 
    parse {
            do! chr '('
            let! symbol = parseSymbol
            do! chr ')'
            return SetLogic symbol
    }
    <|> parse {
            do! chr '('
            let! option = parseOption
            do! chr ')'
            return SetOption option
    }
    <|> parse {
            do! chr '('
            let! attr = parseAttribute
            do! chr ')'
            return SetInfo attr
    }
    <|> parse {
            do! chr '('
            do! skipString "declare-sort"
            let! symbol = parseSymbol
            let! num = parseNumber
            do! chr ')'
            match num with
            | Numeral i -> return DeclareSort(symbol, i)
            | _ -> failwith "parseCommand: wrong numeral format"
    }
    <|> parse {
            do! chr '('
            do! skipString "define-sort"
            let! symbol = parseSymbol
            do! chr '('
            let! symbols = sepBy parseSymbol spaces1
            do! chr ')'
            let! sort = parseSort
            do! chr ')'
            return DefineSort(symbol, symbols, sort)
    }
    <|> parse {
            do! chr '('
            do! skipString "define-sort"
            let! symbol = parseSymbol
            do! chr '('
            let! symbols = sepBy parseSymbol spaces1
            do! chr ')'
            let! sort = parseSort
            do! chr ')'
            return DefineSort(symbol, symbols, sort)
    }
    <|> parse {
            do! chr '('
            do! skipString "declare-fun"
            let! symbol = parseSymbol
            do! chr '('
            let! sorts = sepBy parseSort spaces1
            do! chr ')'
            let! sort = parseSort
            do! chr ')'
            return DeclareFun(symbol, sorts, sort)
    }
    <|> parse {
            do! chr '('
            do! skipString "define-fun"
            let! symbol = parseSymbol
            do! chr '('
            let! sortedVars = sepBy parseSortedVar spaces1
            do! chr ')'
            let! sort = parseSort
            let! term = parseTerm
            do! chr ')'
            return DefineFun(symbol, sortedVars, sort, term)
    }
    <|> parse {
            do! chr '('
            do! skipString "push"
            let! num = parseNumber
            do! chr ')'
            match num with
            | Numeral i -> return Push i
            | _ -> failwith "parseCommand: wrong numeral format"
    }
    <|> parse {
            do! chr '('
            do! skipString "pop"
            let! num = parseNumber
            do! chr ')'
            match num with
            | Numeral i -> return Pop i
            | _ -> failwith "parseCommand: wrong numeral format"
    }
    <|> parse {
            do! chr '('
            do! skipString "assert"
            let! term = parseTerm
            do! chr ')'
            return Assert term
    }
    <|> parse {
            do! chr '('
            do! skipString "get-value"
            let! terms = sepBy parseTerm spaces1
            do! chr ')'
            match terms with
            | [] -> failwith "parseCommand: should have at least one term"
            | _ -> return GetValue terms
    }
    <|> parse {
            do! chr '('
            do! skipString "get-option"
            let! keyword = parseKeyword
            do! chr ')'
            return GetOption keyword
    }
    <|> parse {
            do! chr '('
            do! skipString "get-info"
            let! infoFlag = parseInfoFlag
            do! chr ')'
            return GetInfo infoFlag
    }
    <|> parse {
            do! chr '('            
            let! s = pString
            do! chr ')'
            match s with
            | "check-sat" -> return CheckSat
            | "get-assertions" -> return GetAssertions
            | "get-proofs" -> return GetProof
            | "get-unsat-core" -> return GetUnsatCore
            | "get-assignment" -> return GetAssignment
            | "exit" -> return Exit
            | _ -> failwith "TODO: don't know how to backtrack to parse other cases"
    }