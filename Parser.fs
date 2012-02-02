﻿module SMT2.Parser

open System.Collections.Generic
open FParsec
open FParsec.Primitives 
open FParsec.CharParsers 
 
open SMT2.Ast

// Utility functions
type SMT2Parser<'a> = Parser<'a, unit> 

let SYMBOLS = ".!$%&|*+-/:<=>?@^_~#"
let symbol: SMT2Parser<char> = anyOf SYMBOLS

let chr c = skipChar c 
let endBy p sep = many (p .>> sep)

let isSymbol (c: char) = SYMBOLS.Contains(string c)
let reservedWords = set [
                         "par"; "NUMERAL"; "DECIMAL"; "STRING"; 
                         "_"; "!"; "as"; "let"; "forall"; "exists";
                         ]

let str s = pstring s
let str_ws s = pstring s .>> spaces
let ws_str s = spaces >>. pstring s

let betweenStrings s1 s2 p = str s1 >>. p .>> str s2

// Lexical syntax

// Currently not keep information about comments
let pComment = spaces >>. pchar ';' .>> skipRestOfLine true

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
            do! spaces
            let! s = numOrDec <|> hexOrBin
            match run pNumber <| s with
            | Success(result, _, _)   -> return result
            | Failure(_, _, _) -> ()
    }

// This method cause a lot of confusion
let pStringLiteral = 
    spaces >>. (betweenStrings "\"" "\"" <| manyChars (noneOf "\""))

let parseStringLiteral = pStringLiteral |>> String

let parseConst = parseNumber <|> parseStringLiteral    

let pSymbol =    
    attempt (parse {
            let! symbol = many1Satisfy2 (fun c -> isLetter c || isSymbol c) (fun c -> isDigit c || isLetter c || isSymbol c)
            if Set.contains symbol reservedWords then 
               failwith <| sprintf "pSymbol: reserved word of %s" symbol
            else
                return symbol
        })
    <|> (betweenStrings "|" "|" (manyChars (noneOf "|"))) // The order does matter
    

let parseSymbol = pSymbol |>> Symbol

// TODO: process keywords
let pKeyword = 
    many1Satisfy2 (fun c -> c = ':') (fun c -> isDigit c || isLetter c || isSymbol c)

let parseKeyword = pKeyword |>> Keyword

// You need to refer to parseSexpr from the productions below, hence the forward declaration trick 
let parseSexpr, parseSexprRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
// Just expressions separated by one or more spaces 
let parseSexpList = sepBy parseSexpr spaces1

// A SMTParser<_> can be one of the below
// TODO: note that the definition for list need to backtrack to disambinguate the two cases 
do parseSexprRef := choice [
                            parseConst |>> Const;
                            parseKeyword |>> Kw;
                            str_ws "(" >>. parseSexpList .>> str_ws ")" |>> List;
                            parseSymbol |>> Sb;
                            ]
                    
// TODO: many ugly cases need to be handled.
let parseIdentifier: SMT2Parser<_> =        
    attempt (parse {
                do! chr '(' 
                do! spaces // TODO: should at least one whitespace
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
        })
    <|> (parseSymbol |>> Id)

let parseAttrVal =
    parseConst |>> AttrConst
    <|> (parseSymbol |>> AttrSym) 
    <|> (parseSexpList |>> AttrSexp)

let parseAttribute: SMT2Parser<_> =    
    attempt (parse {
                let! kw = parseKeyword
                do! spaces1
                let! attrVal = parseAttrVal
                return CompAttr(kw, attrVal)
        })
    <|> (parseKeyword |>> Attr)

let parseSort, parseSortRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
let pSortList: SMT2Parser<_> = sepBy parseSort spaces1

do parseSortRef :=  attempt (parseIdentifier |>> Sort)
                    <|> parse { 
                            do! chr '('                             
                            let! id = parseIdentifier        
                            do! spaces1                    
                            let! xs = pSortList                            
                            do! chr ')' 
                            match xs with
                            | []-> failwith "parseSortRef: should have at least one sort declaration"
                            | _ -> return CompSort(id, xs) 
                        } 

let parseQualIdent: SMT2Parser<_> =    
    attempt (parse {
                do! chr '('                
                do! skipString "as"                
                let! id = parseIdentifier   
                do! spaces1             
                let! sort = parseSort
                do! chr ')'          
                return CompQualIdent(id, sort) 
        })
    <|> (parseIdentifier |>> QualIdent)

let parseSortedVar = 
    parse {
            do! chr '('
            let! symbol = parseSymbol
            do! spaces1
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
            do! spaces1
            let! term = parseTerm
            do! chr ')'
            return VarBinding(symbol, term)
    }

do parseTermRef := (parseConst |>> ConstTerm)
                    <|> (parseQualIdent |>> QualTerm)
                    <|> parse { 
                            do! chr '('                  
                            let! qi = parseQualIdent
                            do! spaces1
                            let! ts = pTermList                                                        
                            do! chr ')' 
                            match ts with
                            | []-> failwith "parseTermRef: should have at least one term"
                            | _ -> return CompQualTerm(qi, ts) 
                        } 
                    <|> parse { 
                            do! chr '('       
                            do! skipString "let"
                            do! spaces1
                            do! chr '('                            
                            let! xs = sepBy parseVarBinding spaces1
                            do! chr ')'
                            do! spaces1 
                            let! term = parseTerm
                            do! chr ')' 
                            match xs with
                            | []-> failwith "parseTermRef: should have at least one sorted var"
                            | _ -> return Let(xs, term) 
                        } 
                    <|> parse { // should merge these two cases (forall, exists) together
                            do! chr '('       
                            do! skipString "forall"
                            do! spaces1
                            do! chr '('                            
                            let! xs = sepBy parseSortedVar spaces1
                            do! chr ')' 
                            do! spaces1
                            let! term = parseTerm
                            do! chr ')' 
                            match xs with
                            | []-> failwith "parseTermRef: should have at least one sorted var"
                            | _ -> return Forall(xs, term) 
                        } 
                    <|> parse { 
                            do! chr '('       
                            do! skipString "exists"
                            do! spaces1
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
                            do! spaces1
                            let! attrs = sepBy parseAttribute spaces1
                            do! chr ')'
                            match attrs with
                            | []-> failwith "parseTermRef: should have at least one attribute"
                            | _ -> return AttrTerm(term, attrs) 
                        }

// NB: currently not support Theory Declarations

// NB: currently not support Logic Declarations

let parseBoolean = spaces >>. ((stringReturn "true" true) <|> (stringReturn "false" false))

let parseOption = 
    (parse {
            let! s = manyChars (digit <|> letter <|> symbol)
            match s with
            | ":print-success" -> let! b = parseBoolean
                                  return BoolConfig(BCT.``:print-success``, b)
            | ":expand-definitions" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(BCT.``:expand-definitions``, b)
            | ":interactive-mode" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(BCT.``:interactive-mode``, b)
            | ":produce-proofs" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(BCT.``:produce-proofs``, b)
            | ":produce-unsat-cores" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(BCT.``:produce-unsat-cores``, b)
            | ":produce-models" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(BCT.``:produce-models``, b)
            | ":produce-assignments" -> 
                                  let! b = parseBoolean
                                  return BoolConfig(BCT.``:produce-assignments``, b)
            | ":regular-output-channel" ->
                                  let! s' = pStringLiteral
                                  return StringConfig(SCT.``:regular-output-channel``, s')
            | ":diagnostic-output-channel" ->
                                  let! s' = pStringLiteral
                                  return StringConfig(SCT.``:diagnostic-output-channel``, s')
            | ":random-seed" ->
                                  let! n = parseNumber
                                  match n with
                                  | Numeral i -> return NumeralConfig(NCT.``:random-seed``, i)
                                  | _ -> failwith "parseOption: wrong numeral format"
            | ":verbosity" ->
                                  let! n = parseNumber
                                  match n with
                                  | Numeral i -> return NumeralConfig(NCT.``:verbosity``, i)
                                  | _ -> failwith "parseOption: wrong numeral format"
            | _ -> failwith <| sprintf "parseOption: unknown config of %s" s
    })
    <|> (parseAttribute |>> AttrOption)

let parseInfoFlag =     
    parse {
            let! s = pKeyword
            match s with
            | ":error-behaviour" -> return BuiltinFlag Flag.``:error-behaviour``
            | ":name" -> return BuiltinFlag Flag.``:name``
            | ":authors" -> return BuiltinFlag Flag.``:authors``
            | ":version" -> return BuiltinFlag Flag.``:version``
            | ":status" -> return BuiltinFlag Flag.``:status``
            | ":reason-unknown" -> return BuiltinFlag Flag.``:reason-unknown``
            | ":all-statistics" -> return BuiltinFlag Flag.``:all-statistics``
            | _ -> return CustomFlag (Keyword s)
    }

let parseCommand = 
    attempt (parse {
            do! chr '('            
            let! s = spaces >>. manyChars (digit <|> letter <|> symbol)
            do! chr ')'
            match s with
            | "check-sat" -> return CheckSat
            | "get-assertions" -> return GetAssertions
            | "get-proof" -> return GetProof
            | "get-unsat-core" -> return GetUnsatCore
            | "get-assignment" -> return GetAssignment
            | "exit" -> return Exit
            | _ -> failwith <| sprintf "parseCommand: not a nullary command of %s" s
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "set-logic"
            do! spaces1
            let! symbol = parseSymbol
            do! chr ')'
            return SetLogic symbol
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "set-option"
            do! spaces1
            let! option = parseOption
            do! chr ')'
            return SetOption option
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "set-info"
            do! spaces1
            let! attr = parseAttribute
            do! chr ')'
            return SetInfo attr
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "declare-sort"
            do! spaces1
            let! symbol = parseSymbol
            do! spaces1
            let! num = parseNumber
            do! chr ')'
            match num with
            | Numeral i -> return DeclareSort(symbol, i)
            | _ -> failwith "parseCommand: wrong numeral format"
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "define-sort"
            do! spaces1
            let! symbol = parseSymbol
            do! spaces1
            do! chr '('
            let! symbols = sepBy parseSymbol spaces1
            do! chr ')'
            do! spaces1
            let! sort = parseSort
            do! chr ')'
            return DefineSort(symbol, symbols, sort)
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "declare-fun"
            do! spaces1
            let! symbol = parseSymbol
            do! spaces1
            do! chr '('
            let! sorts = sepBy parseSort spaces1
            do! chr ')'
            do! spaces1
            let! sort = parseSort
            do! chr ')'
            return DeclareFun(symbol, sorts, sort)
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "define-fun"
            do! spaces1
            let! symbol = parseSymbol
            do! spaces1
            do! chr '('
            let! sortedVars = sepBy parseSortedVar spaces1
            do! chr ')'
            do! spaces1
            let! sort = parseSort
            do! spaces1
            let! term = parseTerm
            do! chr ')'
            return DefineFun(symbol, sortedVars, sort, term)
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "push"
            do! spaces1
            let! num = parseNumber
            do! chr ')'
            match num with
            | Numeral i -> return Push i
            | _ -> failwith "parseCommand: wrong numeral format"
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "pop"
            do! spaces1
            let! num = parseNumber
            do! chr ')'
            match num with
            | Numeral i -> return Pop i
            | _ -> failwith "parseCommand: wrong numeral format"
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "assert"
            do! spaces1
            let! term = parseTerm
            do! chr ')'
            return Assert term
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "get-value"
            do! spaces1
            do! chr '('
            let! terms = sepBy parseTerm spaces1
            do! chr ')'
            do! chr ')'
            match terms with
            | [] -> failwith "parseCommand: should have at least one term"
            | _ -> return GetValue terms
    })
    <|> attempt (parse {
            do! chr '('
            do! skipString "get-option"
            do! spaces1
            let! keyword = parseKeyword
            do! chr ')'
            return GetOption keyword
    })
    <|> parse {
            do! chr '('
            do! skipString "get-info"
            do! spaces1
            let! infoFlag = parseInfoFlag
            do! chr ')'
            return GetInfo infoFlag
    }

let parse = sepBy parseCommand (newline <|> pComment) .>> eof