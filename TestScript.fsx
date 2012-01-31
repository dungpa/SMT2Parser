#r "FParsec"
#r "FParsecCS"

#load "Ast.fs"
#load "Parser.fs"

open FParsec
open SMT2.Parser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A from \"%s\"" result str
    | Failure(errorMsg, _, _) -> printfn "Failure: %s from \"%s\"" errorMsg str

let testAll p xs =
    for x in xs do
        test p x

#time "on";;
// This will match 0xA04; could remove this side-effect later.
["012"; "2.0"; "2.2"; 
 "0xA04"; "#x0"; "xA04"; 
 "#x01Ab"; "#b0"; "#b101"; 
 "should_fail"]
|> testAll parseNumber;;

// Should parse \" and \\ in a differrent way.
["\"\""; "\"this is a string literal\""; 
"\"one\n two\""; "\"She said: \"Hello!\"\""; 
"\"Here is a backslash: \\\""]
|> testAll parseString;;

// The last symbol should be parsable.
["+"; "<="; "x"; "plus"; "**"; "$"; "<sas"; "<adf>";
"abc77"; "*$s&6"; ".kkk"; ".8"; "+34"; "-32";
"|this is a single quoted symbol|"; "||"; "|j af klj ^(0 asfsfe2(&)&(#^$>>>?\" ']]984j|"]
|> testAll parseSymbol;;

[":date"; ":a2"; ":foo-bar";
":<="; ":56"; ":->";
"should fail"]
|> testAll parseKeyword;;

// Don't allow "( )"
["10"; "0.2"; "#xA04"; "#b11";
"\"here is a string\""; "+++"; ":==";
//"should fail";
"( :version  )";]
|> testAll parseSexpr;;