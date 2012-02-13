module SMT2Parser.Parser

open System
open System.Numerics
open System.Collections.Generic
open System.Globalization

open FParsec
open FParsec.Primitives 
open FParsec.CharParsers 
 
open SMT2Parser.Ast

// Utility functions
type SMT2Parser<'a> = Parser<'a, unit> 

let SYMBOLS = "~!@$%^&*_-+=<>.?/"
let isSymbol (c: char) = String.exists ((=) c) SYMBOLS
                 //SYMBOLS.Contains(string c)
let ws = spaces
let ws1 = spaces1

let chr = skipChar
let chr_ws c = chr c .>> ws
let ws_chr c = ws >>. chr c

let str s = skipString s
let str_ws s = str s .>> ws
let ws_str s = ws >>. str s

let betweenStrings s1 s2 p = str s1 >>. p .>> str s2

let lparen = pstring "(" .>> ws
let rparen = ws >>. pstring ")"

let resultSatisfies predicate msg (p: SMT2Parser<_>): SMT2Parser<_> =
    let error = messageError msg
    fun stream ->
      let state = stream.State
      let reply = p stream
      if reply.Status <> Ok || predicate reply.Result then reply
      else
          stream.BacktrackTo(state) // backtrack to beginning
          Reply(Error, error)

// Lexical syntax

// Currently not keep information about comments
let comment = ws >>. chr ';' .>> skipRestOfLine true

let pnumeral = many1SatisfyL isDigit "digit" |>> bigint.Parse

let numeralOrdecimal = 
    pipe2 (many1SatisfyL isDigit "digit") (chr '.' >>. many1SatisfyL isDigit "decimal digit" |> opt)
          (fun s1 optS2 -> match optS2 with
                           | Some s2 -> sprintf "%s.%s" s1 s2 |> Convert.ToDecimal |> Decimal
                           | None -> s1 |> bigint.Parse |> Numeral)

let hexadecimal =
    str "#x" >>. many1SatisfyL isHex "hex digit"
    |>> (fun hexStr -> bigint.Parse(hexStr, NumberStyles.AllowHexSpecifier) |> Hexadecimal)

let binary =
    str "#b" >>. many1SatisfyL (fun c -> c = '0' || c = '1') "binary digit"
    |>> (fun binStr -> let s = (new String('0', 8-(binStr.Length%8))) + binStr
                       let nBytes = s.Length/8
                       Array.init nBytes id 
                       |> Array.map (fun i -> Convert.ToByte(s.Substring(8*i, 8), 2))
                       |> fun bs -> bigint(bs)
                       |> Binary)

let number =  
    choiceL [
             hexadecimal; 
             binary;
             numeralOrdecimal; // if not decimal, could be numeral
            ]
            "number literal"

let pStringLiteral = 
    ws >>. betweenStrings "\"" "\"" (manySatisfy ((<>) '"'))

let stringLiteral = pStringLiteral |>> String

let sconst = 
    choiceL [
             number; 
             stringLiteral 
            ]
            "sconstant"   

let reservedWords = set [
                         "par"; "NUMERAL"; "DECIMAL"; "STRING"; 
                         "_"; "!"; "as"; "let"; "forall"; "exists";
                         ]

let pSymbol =
    betweenStrings "|" "|" (manySatisfy ((<>) '|'))
    <|> (many1Satisfy2L (fun c -> isLetter c || isSymbol c) (fun c -> isDigit c || isLetter c || isSymbol c) "simple symbol"
         |> resultSatisfies (not << reservedWords.Contains) "Should not be a reserved word")
    
let symbol = pSymbol |>> Symbol

let pKeyword = 
    many1Satisfy2L (fun c -> c = ':') (fun c -> isDigit c || isLetter c || isSymbol c) "keyword starting with ':'"

let keyword = pKeyword |>> Keyword

// You need to refer to sexpr from the productions below, hence the forward declaration trick 
let sexpr, sexprRef = createParserForwardedToRef() 
 
// Just expressions separated by one or more ws 
let sexpList = sepEndBy sexpr ws1

do sexprRef := choiceL [
                        sconst |>> Const;                      
                        keyword |>> Kw;
                        symbol |>> Sb;
                        lparen >>. sexpList .>> rparen |>> List;                        
                        ]
                        "sexpression"

// Changed numeral to pnumeral        
let indexedId = 
    tuple2 (chr '_' >>. ws1 >>. symbol) (ws1 >>. sepEndBy1 pnumeral ws1)
    |>> IndexedId

let identifier = 
    choiceL [
             symbol |>> Id;
             lparen >>? indexedId .>> rparen; // conflict with '(' of terms.
            ]
            "identifier"

let attrVal =
    choiceL [
            sconst |>> AttrConst;
            symbol |>> AttrSym;
            sexpList |>> AttrSexp;
            ]
            "attribute value"

let attribute =    
    pipe2 keyword (ws1 >>. attrVal |> opt)
          (fun k optAv -> match optAv with
                          | Some av -> CompAttr(k, av)  
                          | None -> Attr k)

let sort, sortRef = createParserForwardedToRef() 
 
let pSortList1 = sepEndBy1 sort ws1

let compSort = 
    tuple2 (lparen >>. identifier) (ws1 >>. pSortList1 .>> rparen)
    |>> CompSort

do sortRef :=  choiceL [
                        identifier |>> Sort; 
                        compSort;
                        ]
                       "sort" 

let compQualIdent =
    tuple2 (str "as" >>. ws1 >>. identifier) (ws1 >>. sort)
    |>> CompQualIdent

let qualIdent = 
    choiceL [   
             identifier |>> QualIdent;
             lparen >>? compQualIdent .>> rparen; // conflict with '(' of terms.
             ]
            "qualified identifier"

let sortedVar = 
    tuple2 (lparen >>. symbol) (ws1 >>. sort .>> rparen)
    |>> SortedVar

// Keep term in this form to avoid stackoverflow too soon.
let term, termRef = createParserForwardedToRef() 
 
let pTermList1 = sepEndBy1 term ws1

let varBinding = 
    tuple2 (lparen >>. symbol) (ws1 >>. term .>> rparen)
    |>> VarBinding

let ``let`` = 
    tuple2 (lparen >>? str_ws "let" >>. lparen >>. sepEndBy1 varBinding ws1 .>> rparen) (ws >>. term .>> rparen)
    |>> Let

let forall =
    tuple2 (lparen >>? str_ws "forall" >>. lparen >>. sepEndBy1 sortedVar ws1 .>> rparen) (ws >>. term .>> rparen)
    |>> Forall

let exists =
    tuple2 (lparen >>? str_ws "exists" >>. lparen >>. sepEndBy1 sortedVar ws1 .>> rparen) (ws >>. term .>> rparen)
    |>> Exists

let bang =
    tuple2 (lparen >>?  str "!" >>. ws1 >>. term) (ws1 >>. sepEndBy1 attribute ws1 .>> rparen)
    |>> AttrTerm

let compQualTerm =
    tuple2 (lparen >>? qualIdent) (ws1 >>. pTermList1 .>> rparen)
    |>> CompQualTerm
    
// Need to control backtracking smarter
do termRef :=  choiceL [
                        sconst |>> ConstTerm;
                        qualIdent |>> QualTerm; // if not QualTerm, could be compQualTerm 
                        compQualTerm;
                        ``let``;
                        bang;
                        forall;
                        exists;
                        ]
                       "term"
                    
// NB: currently not support Theory Declarations

// NB: currently not support Logic Declarations

let pboolean = (stringReturn "true" true) <|> (stringReturn "false" false)

let boolConfig =
    choice [ 
            str_ws ":print-success" >>. pboolean |>> (fun b -> BoolConfig(BCT.``:print-success``, b));
            str_ws ":expand-definitions" >>. pboolean |>> (fun b -> BoolConfig(BCT.``:expand-definitions``, b));
            str_ws ":interactive-mode" >>. pboolean |>> (fun b -> BoolConfig(BCT.``:interactive-mode``, b));
            str_ws ":produce-proofs" >>. pboolean |>> (fun b -> BoolConfig(BCT.``:produce-proofs``, b));
            str_ws ":produce-unsat-cores" >>. pboolean |>> (fun b -> BoolConfig(BCT.``:produce-unsat-cores``, b));
            str_ws ":produce-models" >>. pboolean |>> (fun b -> BoolConfig(BCT.``:produce-models``, b));
            str_ws ":produce-assignments" >>. pboolean |>> (fun b -> BoolConfig(BCT.``:produce-assignments``, b));
            ]

let stringConfig =
    choice [ 
            str_ws ":regular-output-channel" >>. pStringLiteral |>> (fun s -> StringConfig(SCT.``:regular-output-channel``, s));
            str_ws ":diagnostic-output-channel" >>. pStringLiteral |>> (fun s -> StringConfig(SCT.``:diagnostic-output-channel``, s));
            ]

let numeralConfig =
    choice [ 
            str_ws ":random-seed" >>. pnumeral |>> (fun i -> NumeralConfig(NCT.``:random-seed``, i));
            str_ws ":verbosity" >>. pnumeral |>> (fun i -> NumeralConfig(NCT.``:verbosity``, i));
            ]

let option = 
    choiceL [
             boolConfig;
             stringConfig;
             numeralConfig;
             attribute |>> AttrOption;
            ]
            "option"

let infoFlag = 
    pKeyword |>> function
                 | ":error-behaviour" -> BuiltinFlag Flag.``:error-behaviour``
                 | ":name" -> BuiltinFlag Flag.``:name``
                 | ":authors" -> BuiltinFlag Flag.``:authors``
                 | ":version" -> BuiltinFlag Flag.``:version``
                 | ":status" -> BuiltinFlag Flag.``:status``
                 | ":reason-unknown" -> BuiltinFlag Flag.``:reason-unknown``
                 | ":all-statistics" -> BuiltinFlag Flag.``:all-statistics``
                 | s -> CustomFlag (Keyword s)

let declareSort =
    tuple2 (str "declare-sort" >>. ws1 >>. symbol)
           (ws1 >>. pnumeral)
    |>> DeclareSort

let defineSort =
    tuple3 (str "define-sort" >>. ws1 >>. symbol)
           (ws >>. lparen >>. sepEndBy symbol ws1 .>> rparen)
           (ws >>. sort)
    |>> DefineSort

let declareFun =
    tuple3 (str "declare-fun" >>. ws1 >>. symbol)
           (ws >>. lparen >>. sepEndBy sort ws1 .>> rparen)
           (ws >>. sort)
    |>> DeclareFun

let defineFun =
    tuple4 (str "define-fun" >>. ws1 >>. symbol)
           (ws >>. lparen >>. sepEndBy sortedVar ws1 .>> rparen)
           (ws >>. sort)
           (ws >>. term)
    |>> DefineFun

let command =
    [
        str "check-sat" |>> (fun _ -> CheckSat);
        str "get-assertions" |>> (fun _ -> GetAssertions);
        str "get-proof" |>> (fun _ -> GetProof);
        str "get-unsat-core"  |>> (fun _ -> GetUnsatCore);
        str "get-assignment" |>> (fun _ -> GetAssignment);
        str "exit" |>> (fun _ -> Exit);

        str "set-logic" .>> ws1 >>. symbol |>> SetLogic;
        str "set-option" .>> ws1 >>. option |>> SetOption;
        str "set-info" .>> ws1 >>. attribute |>> SetInfo;
        str "push" .>> ws1 >>. pnumeral |>> Push;
        str "pop" .>> ws1 >>. pnumeral |>> Pop;
        str "assert" .>> ws1 >>. term |>> Assert;
        str "get-value" .>> ws >>. lparen >>. sepEndBy1 term ws1 .>> rparen |>> GetValue;
        str "get-option" .>> ws1 >>. keyword |>> GetOption;
        str "get-info" .>> ws1 >>. infoFlag |>> GetInfo;

        declareSort;
        defineSort;
        declareFun;
        defineFun;
        ]
    |> choice
    |> between lparen rparen

let script = sepEndBy command (skipSepBy ws comment) .>> eof

let parseCommand str = 
    match run command str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith <| sprintf "Failure: %s from \"%s\"" errorMsg str

let parse str = 
    match run script str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith <| sprintf "Failure: %s from \"%s\"" errorMsg str

let parseFile path = 
    match runParserOnFile script () path System.Text.Encoding.UTF8  with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith <| sprintf "Failure: %s from file \"%s\"" errorMsg path