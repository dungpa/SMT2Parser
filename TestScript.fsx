#r "FParsec"
#r "FParsecCS"

#load "Ast.fs"
#load "Parser.fs"

open System.IO
open FParsec
open SMT2Parser.Parser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A from \"%s\"" result str
    | Failure(errorMsg, _, _) -> printfn "Failure: %s from \"%s\"" errorMsg str

let testAll p xs =
    for x in xs do
        test p x

#time "on";;
["012"; "2.0"; "2.2"; 
 "#x0"; "#x01Ab"; 
 "#b0"; "#b101"; 
 ]
|> testAll number;;

// Should  \" and \\ in a differrent way.
["\"\""; "\"this is a string literal\""; 
"\"one\n two\""; "\"She said: \"Hello!\"\""; 
"\"Here is a backslash: \\\""]
|> testAll stringLiteral;;

// The last symbol should be parsable.
["+"; "<="; "x"; "plus"; "**"; "$"; "<sas"; "<adf>";
"abc77"; "*$s&6"; ".kkk"; ".8"; "+34"; "-32";
"|this is a single quoted symbol|"; "||"; "|j af klj ^(0 asfsfe2(&)&(#^$>>>?\" ']]984j|"]
|> testAll symbol;;

[":date"; ":a2"; ":foo-bar";
":<="; ":56"; ":->";
]
|> testAll keyword;;

// Don't allow "( )"
["10"; "0.2"; "#xA04"; "#b11";
"\"here is a string\""; "+++"; ":==";
"(:version)"; "( :version :author)";]
|> testAll sexpr;;

["+"; "<="; "x"; "plus"; "**"; "$"; "<sas"; "<adf>";
"|this is a single quoted symbol|"; "(_ a 1 2)"; "( _ b 2 3 0)"]
|> testAll identifier;;

[":left-assoc"; ":status unsat";
":my_attribute (humpty dumpty)"; ":authors \"Jack and Jill\"";
]
|> testAll attribute;;

["Int"; "Bool";
"(_ BitVec 3)"; "(List (Array Int Real))";
"((_ FixedSizeList 4) Real)"; "(Set (_ Bitvec 3))";
]
|> testAll sort;;

[
"(simplify (>= x (+ x x)))"
]
|> testAll term;;

[
"(set-logic QF_LIA)";
"(set-option :print-success false)";
"(push 1)";
"(assert (> z x))";
"(check-sat)";
"(get-info :all-statistics)";
"(pop 1)";
"(exit)";
]
|> testAll command;;

[
"true";
"false";
]
|> testAll boolean;;

let s = File.ReadAllText (__SOURCE_DIRECTORY__ + "\\tests\\test10.smt2")
let ast = parseCommand s;;