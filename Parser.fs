module SMT2.Parser

open System
open System.Collections.Generic

open FParsec
open FParsec.Primitives 
open FParsec.CharParsers 
 
open SMT2.Ast

// Utility functions
type SMT2Parser<'a> = Parser<'a, unit> 

let SYMBOLS = ".!$%&|*+-/:<=>?@^_~#"
let isSymbol (c: char) = SYMBOLS.Contains(string c)

let chr = skipChar
let chr_ws c = skipChar c .>> spaces
let ws_chr c = spaces >>. skipChar c

let str s = skipString s
let str_ws s = skipString s .>> spaces
let ws_str s = spaces >>. skipString s

let betweenStrings s1 s2 p = str s1 >>. p .>> str s2

let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
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
let comment = spaces >>. chr ';' .>> skipRestOfLine true

// Doesn't check for integer overflow.
let pnumeral = many1Chars digit |>> Convert.ToInt32

let numeral = pnumeral |>> Numeral

let decimal = 
    (many1Chars digit .>> chr '.') .>>. (many1Chars digit)
    |>> (fun (s1, s2) -> sprintf "%s.%s" s1 s2 |> Convert.ToDouble |> Decimal)

let hexadecimal =
    str "#x" >>. many1Satisfy isHex
    |>> (fun hexStr -> Convert.ToInt32(hexStr, 16) |> Hexadecimal)

let binary =
    str "#b" >>. many1Satisfy (fun c -> c = '0' || c = '1')
    |>> (fun binStr -> Convert.ToInt32(binStr, 2) |> Binary)

let number =  
    choice [
            hexadecimal; binary;
            attempt decimal; // if not decimal, could be numeral
            numeral;
            ]

// This method cause a lot of confusion
// Need to consider nested strings
let pStringLiteral = 
    spaces >>. betweenStrings "\"" "\"" (manyChars (noneOf "\""))

let stringLiteral = pStringLiteral |>> String

let sconst = number <|> stringLiteral    

let reservedWords = set [
                         "par"; "NUMERAL"; "DECIMAL"; "STRING"; 
                         "_"; "!"; "as"; "let"; "forall"; "exists";
                         ]

let pSymbol =
    betweenStrings "|" "|" (manyChars (noneOf "|"))
    <|> (many1Satisfy2 (fun c -> isLetter c || isSymbol c) (fun c -> isDigit c || isLetter c || isSymbol c)
         |> resultSatisfies (not << reservedWords.Contains) "Should not be a reserved word")
    
let symbol = pSymbol |>> Symbol

let pKeyword = 
    many1Satisfy2 (fun c -> c = ':') (fun c -> isDigit c || isLetter c || isSymbol c)

let keyword = pKeyword |>> Keyword

// You need to refer to sexpr from the productions below, hence the forward declaration trick 
let sexpr, sexprRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
// Just expressions separated by one or more spaces 
let sexpList = sepEndBy sexpr spaces1

// A SMTParser<_> can be one of the below
// TODO: note that the definition for list need to backtrack to disambinguate the two cases 
do sexprRef := choice [
                        attempt sconst |>> Const; // if not sconst, could be symbol                        
                        keyword |>> Kw;
                        symbol |>> Sb;
                        chr_ws '(' >>. sexpList .>> ws_chr ')' |>> List;                        
                        ]

// Change numeral to pnumeral        
let indexedId = 
    (chr_ws '(' >>. chr '_' >>. spaces1 >>. symbol) .>>. (spaces1 >>. sepEndBy1 pnumeral spaces1 .>> ws_chr ')')
    |>> IndexedId

let identifier = 
    symbol |>> Id 
    <|> indexedId

let attrVal =
    choice [
            attempt sconst |>> AttrConst;
            symbol |>> AttrSym;
            sexpList |>> AttrSexp;
            ]

let compAttr =
    keyword .>>. (spaces1 >>. attrVal)
    |>> CompAttr

let attribute =    
    compAttr <|> (keyword |>> Attr)

let sort, sortRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
let pSortList1 = sepEndBy1 sort spaces1

let compSort = 
    (chr_ws '(' >>. identifier) .>>. (spaces1 >>. pSortList1 .>> ws_chr ')')
    |>> CompSort

do sortRef :=  attempt identifier |>> Sort 
               <|> compSort 

let compQualIdent =
    (chr_ws '(' >>? str "as" >>. spaces1 >>. identifier) .>>. (spaces1 >>. sort .>> ws_chr ')')
    |>> CompQualIdent

let qualIdent =    
    identifier |>> QualIdent 
    <|> compQualIdent

let sortedVar = 
    (chr_ws '(' >>. symbol) .>>. (spaces1 >>. sort .>> ws_chr ')')
    |>> SortedVar

let term, termRef: SMT2Parser<_> * SMT2Parser<_> ref = createParserForwardedToRef() 
 
let pTermList1 = sepEndBy1 term spaces1

let varBinding = 
    (chr_ws '(' >>. symbol)
    .>>. (spaces1 >>. term .>> ws_chr ')')
    |>> VarBinding

let ``let`` = 
    (chr_ws '(' >>? str_ws "let" >>. chr_ws '(' >>. sepEndBy1 varBinding spaces1 .>> ws_chr ')') .>>. (spaces >>. term .>> ws_chr ')')
    |>> Let

let forall =
    (chr_ws '(' >>? str_ws "forall" >>. chr_ws '(' >>. sepEndBy1 sortedVar spaces1 .>> ws_chr ')') .>>. (spaces >>. term .>> ws_chr ')')
    |>> Forall

let exists =
    (chr_ws '(' >>? str_ws "exists" >>. chr_ws '(' >>. sepEndBy1 sortedVar spaces1 .>> ws_chr ')') .>>. (spaces >>. term .>> ws_chr ')')
    |>> Exists

let bang =
    (chr_ws '(' >>?  str "!" >>. spaces1 >>. term) .>>. (spaces1 >>. sepEndBy1 attribute spaces1 .>> ws_chr ')')
    |>> AttrTerm

let compQualTerm =
    (chr_ws '(' >>? qualIdent) .>>. (spaces1 >>. pTermList1 .>> ws_chr ')')
    |>> CompQualTerm
    
// Need to control backtracking smarter
do termRef :=  choice [
                        attempt sconst |>> ConstTerm; // if not ConstTerm, could be QualTerm
                        attempt qualIdent |>> QualTerm;
                        compQualTerm;
                        ``let``;
                        bang;
                        forall;
                        exists;
                    ]
                    
// NB: currently not support Theory Declarations

// NB: currently not support Logic Declarations

let boolean = (stringReturn "true" true) <|> (stringReturn "false" false)

let boolConfig =
    choice [ 
            str_ws ":print-success" >>. boolean |>> (fun b -> BoolConfig(BCT.``:print-success``, b));
            str_ws ":expand-definitions" >>. boolean |>> (fun b -> BoolConfig(BCT.``:expand-definitions``, b));
            str_ws ":interactive-mode" >>. boolean |>> (fun b -> BoolConfig(BCT.``:interactive-mode``, b));
            str_ws ":produce-proofs" >>. boolean |>> (fun b -> BoolConfig(BCT.``:produce-proofs``, b));
            str_ws ":produce-unsat-cores" >>. boolean |>> (fun b -> BoolConfig(BCT.``:produce-unsat-cores``, b));
            str_ws ":produce-models" >>. boolean |>> (fun b -> BoolConfig(BCT.``:produce-models``, b));
            str_ws ":produce-assignments" >>. boolean |>> (fun b -> BoolConfig(BCT.``:produce-assignments``, b));
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
    choice [
            boolConfig;
            stringConfig;
            numeralConfig;
            attribute |>> AttrOption;
        ]

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
    tuple2 (chr_ws '(' >>? str "declare-sort" >>. spaces1 >>. symbol)
           (spaces1 >>. pnumeral .>>  ws_chr ')')
    |>> DeclareSort

let defineSort =
    tuple3 (chr_ws '(' >>? str "define-sort" >>. spaces1 >>. symbol)
           (spaces >>. chr_ws '(' >>. sepEndBy symbol spaces1 .>>  ws_chr ')')
           (spaces >>. sort .>> ws_chr ')')
    |>> DefineSort

let declareFun =
    tuple3 (chr_ws '(' >>? str "declare-fun" >>. spaces1 >>. symbol)
           (spaces >>. chr_ws '(' >>. sepEndBy sort spaces1 .>>  ws_chr ')')
           (spaces >>. sort .>> ws_chr ')')
    |>> DeclareFun

let defineFun =
    tuple4 (chr_ws '(' >>? str "define-fun" >>. spaces1 >>. symbol)
           (spaces >>. chr_ws '(' >>. sepEndBy sortedVar spaces1 .>>  ws_chr ')')
           (spaces >>. sort)
           (spaces >>. term .>> ws_chr ')')
    |>> DefineFun

let command = 
    choice [
            chr_ws '(' >>? str "check-sat" .>> ws_chr ')' |>> (fun _ -> CheckSat);
            chr_ws '(' >>? str "get-assertions" .>> ws_chr ')' |>> (fun _ -> GetAssertions);
            chr_ws '(' >>? str "get-proof" .>> ws_chr ')' |>> (fun _ -> GetProof);
            chr_ws '(' >>? str "get-unsat-core" .>> ws_chr ')' |>> (fun _ -> GetUnsatCore);
            chr_ws '(' >>? str "get-assignment" .>> ws_chr ')' |>> (fun _ -> GetAssignment);
            chr_ws '(' >>? str "exit" .>> ws_chr ')' |>> (fun _ -> Exit);

            chr_ws '(' >>? str "set-logic" .>> spaces1 >>. symbol .>> ws_chr ')' |>> SetLogic;
            chr_ws '(' >>? str "set-option" .>> spaces1 >>. option .>> ws_chr ')' |>> SetOption;
            chr_ws '(' >>? str "set-info" .>> spaces1 >>. attribute .>> ws_chr ')' |>> SetInfo;
            chr_ws '(' >>? str "push" .>> spaces1 >>. pnumeral .>> ws_chr ')' |>> Push;
            chr_ws '(' >>? str "pop" .>> spaces1 >>. pnumeral .>> ws_chr ')' |>> Pop;
            chr_ws '(' >>? str "assert" .>> spaces1 >>. term .>> ws_chr ')' |>> Assert;
            chr_ws '(' >>? str "get-value" .>> spaces >>. chr_ws '(' >>. sepEndBy1 term spaces1 .>> ws_chr ')' .>> ws_chr ')' |>> GetValue;
            chr_ws '(' >>? str "get-option" .>> spaces1 >>. keyword .>> ws_chr ')' |>> GetOption;
            chr_ws '(' >>? str "get-info" .>> spaces1 >>. infoFlag .>> ws_chr ')' |>> GetInfo;

            declareSort;
            defineSort;
            declareFun;
            defineFun;
    ]

let parse = sepEndBy command (skipSepBy spaces comment) .>> eof