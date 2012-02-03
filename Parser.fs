module SMT2.Parser

open System.Collections.Generic
open FParsec
open FParsec.Primitives 
open FParsec.CharParsers 
 
open SMT2.Ast

// Utility functions
type SMT2Parser<'a> = Parser<'a, unit> 

let SYMBOLS = ".!$%&|*+-/:<=>?@^_~#"
let symbol: SMT2Parser<char> = anyOf SYMBOLS

let chr_ws c = skipChar c .>> spaces
let ws_chr c = spaces >>. skipChar c

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
            let! s = spaces >>. numOrDec <|> hexOrBin
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
//            if Set.contains symbol reservedWords then 
//               failwith <| sprintf "pSymbol: reserved word of %s" symbol
//            else
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
let parseSexpList = sepEndBy parseSexpr spaces1

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
                let! symbol = chr_ws '(' >>. skipChar '_' >>. spaces1 >>. parseSymbol // skipChar is unnatural
                let! xs = spaces1 >>. sepEndBy1 parseSexpr spaces1 .>> ws_chr ')' 
                let ns = xs |> List.map (function | Const(Numeral i) -> i | _ -> failwith "parseIdentifier: Wrong format") 
                return IndexedId (symbol, ns)                 
        })
    <|> (parseSymbol |>> Id)

let parseAttrVal =
    parseConst |>> AttrConst
    <|> (parseSymbol |>> AttrSym) 
    <|> (parseSexpList |>> AttrSexp)

let parseAttribute: SMT2Parser<_> =    
    attempt (parse {
                let! kw = parseKeyword
                let! attrVal = spaces1 >>. parseAttrVal
                return CompAttr(kw, attrVal)
        })
    <|> (parseKeyword |>> Attr)

let parseSort, parseSortRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
let pSortList1: SMT2Parser<_> = sepEndBy1 parseSort spaces1

do parseSortRef :=  attempt (parseIdentifier |>> Sort)
                    <|> parse {                              
                            let! id = chr_ws '(' >>. parseIdentifier
                            let! xs = spaces1 >>. pSortList1 .>> ws_chr ')'
                            return CompSort(id, xs) 
                        } 

let parseQualIdent: SMT2Parser<_> =    
    attempt (parse {                
                let! id = chr_ws '(' >>. skipString "as" >>. spaces1 >>. parseIdentifier
                let! sort = spaces1 >>. parseSort .>> ws_chr ')'
                return CompQualIdent(id, sort) 
        })
    <|> (parseIdentifier |>> QualIdent)

let parseSortedVar = 
    parse {
            let! symbol = chr_ws '(' >>. parseSymbol
            let! sort = spaces1 >>. parseSort .>> ws_chr ')'
            return SortedVar(symbol, sort)
    }

let parseTerm, parseTermRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
let pTermList1: SMT2Parser<_> = sepEndBy1 parseTerm spaces1

let parseVarBinding = 
    parse {
            let! symbol = chr_ws '(' >>. parseSymbol
            let! term = spaces1 >>. parseTerm .>> ws_chr ')'
            return VarBinding(symbol, term)
    }

do parseTermRef := attempt (parseConst |>> ConstTerm)
                    <|> attempt (parseQualIdent |>> QualTerm)
                    <|> attempt (parse {                  
                            let! qi = chr_ws '(' >>. parseQualIdent
                            let! ts = spaces1 >>. pTermList1 .>> ws_chr ')'
                            return CompQualTerm(qi, ts) 
                        }) 
                    <|> parse { 
                            let! s = chr_ws '(' >>. manyChars (digit <|> letter <|> symbol) .>> spaces1
                            match s with
                            | "let" -> let! xs = chr_ws '(' >>. sepEndBy1 parseVarBinding spaces1 .>> ws_chr ')'
                                       let! term = spaces1 >>. parseTerm .>> ws_chr ')'
                                       return Let(xs, term) 
                            | "forall" -> 
                                       let! xs = chr_ws '(' >>. sepEndBy1 parseSortedVar spaces1 .>> ws_chr ')'
                                       let! term = spaces1 >>. parseTerm .>> ws_chr ')'
                                       return Forall(xs, term) 
                            | "exists" -> 
                                       let! xs = chr_ws '(' >>. sepEndBy1 parseSortedVar spaces1 .>> ws_chr ')'
                                       let! term = spaces1 >>. parseTerm .>> ws_chr ')'
                                       return Exists(xs, term) 
                            | "!" -> let! term = parseTerm
                                     let! attrs = spaces1 >>. sepEndBy1 parseAttribute spaces1 .>> ws_chr ')'
                                     return AttrTerm(term, attrs)
                            | _ -> failwith <| sprintf "Unknown term of %s" s
                             
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
            | _ -> failwith <| sprintf "parseOption: unknown config of %s" s // should roll back here and parse attr option
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
            let! s = chr_ws '(' >>. manyChars (digit <|> letter <|> symbol) .>> ws_chr ')'
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
            let! symbol = chr_ws '(' >>. skipString "declare-sort" >>. spaces1 >>. parseSymbol
            let! num = spaces1 >>. parseNumber .>>  ws_chr ')'
            match num with
            | Numeral i -> return DeclareSort(symbol, i)
            | _ -> failwith "parseCommand: wrong numeral format"
    })
    <|> attempt (parse {
            let! symbol = chr_ws '(' >>. skipString "define-sort" >>. spaces1 >>. parseSymbol
            let! symbols = spaces1 >>. chr_ws '(' >>. sepEndBy parseSymbol spaces1 .>>  ws_chr ')'
            let! sort = spaces1 >>. parseSort .>> ws_chr ')'
            return DefineSort(symbol, symbols, sort)
    })
    <|> attempt (parse {
            let! symbol = chr_ws '(' >>. skipString "declare-fun" >>. spaces1 >>. parseSymbol
            let! sorts = spaces1 >>. chr_ws '(' >>. sepEndBy parseSort spaces1 .>> ws_chr ')'
            let! sort = spaces1 >>. parseSort .>> ws_chr ')'
            return DeclareFun(symbol, sorts, sort)
    })
    <|> attempt (parse {
            let! symbol = chr_ws '(' >>. skipString "define-fun" >>. spaces1 >>. parseSymbol
            let! sortedVars = spaces1 >>. chr_ws '(' >>. sepEndBy parseSortedVar spaces1 .>>  ws_chr ')'
            let! sort = spaces1 >>. parseSort
            let! term = spaces1 >>. parseTerm .>> ws_chr ')'
            return DefineFun(symbol, sortedVars, sort, term)
    })
    <|> parse {
            let! s = chr_ws '(' >>. manyChars (digit <|> letter <|> symbol)
            match s with
            | "set-logic" -> let! symbol = spaces1 >>. parseSymbol .>> ws_chr ')'
                             return SetLogic symbol
            | "set-option" -> let! option = spaces1 >>. parseOption .>> ws_chr ')'
                              return SetOption option
            | "set-info" -> let! attr = spaces1 >>. parseAttribute .>> ws_chr ')'
                            return SetInfo attr
            | "push" -> let! n = spaces1 >>. parseNumber .>> ws_chr ')'
                        match n with
                        | Numeral i -> return Push i
                        | _ -> failwith "parseCommand: wrong numeral format"
            | "pop" ->  let! n = spaces1 >>. parseNumber .>> ws_chr ')'
                        match n with
                        | Numeral i -> return Pop i
                        | _ -> failwith "parseCommand: wrong numeral format"
            | "assert" ->  
                        let! term = spaces1 >>. parseTerm .>> ws_chr ')'
                        return Assert term
            | "get-value" ->  
                        let! terms = spaces1 >>. sepEndBy1 parseTerm spaces1 .>> ws_chr ')'
                        return GetValue terms
            | "get-option" ->  
                        let! keyword = spaces1 >>. parseKeyword .>> ws_chr ')'
                        return GetOption keyword
            | "get-info" ->  
                        let! infoFlag = spaces1 >>. parseInfoFlag .>> ws_chr ')'
                        return GetInfo infoFlag
            | _ -> failwith <| sprintf "parseCommand: not an unary command of %s" s
    }

let parse = sepEndBy parseCommand (newline <|> pComment) .>> eof