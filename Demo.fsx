 // Demonstrate stackoverflow by a simple example

#r "FParsec"
#r "FParsecCS"

open FParsec 
(* 
    <expression> ::= <name> | <function> | <application>  
    <name> ::= non­blank character sequence  
    <function> ::= \ <name> . <body>  
    <body> ::= <expression>  
    <application> ::= ( <function expression> <argument expression> )  
    <function expression> ::= <expression>  
    <argument expression> ::= <expression>  
*) 
 
type Name = string 
and Body = Expression 
and Function = Name * Expression 
and FunctionExpression = Expression 
and ArgumentExpression = Expression 
and Expression =  
| EName of string 
| Function of Expression * Body 
| Application of FunctionExpression * ArgumentExpression 
| EOT 

let ws = " \t\n" 
let specialChars = ".)(\\\n" 
 
let pWs = spaces 
let pName = manyChars (noneOf (ws + specialChars)) |>> EName 
 
let pExpr, pExprRef = createParserForwardedToRef<Expression, Unit>() 
 
let curry2 f a b = f(a,b) 
let pFunction = pchar '\\' >>. pipe2 pName (pchar '.' >>. pExpr) (curry2 Function) 
 
let pApplication = pchar '(' >>. pipe2 pExpr (pWs >>. pExpr) (curry2 Application)
                            .>> pWs .>> pchar ')'

do pExprRef := pFunction <|> pApplication <|> pName 
 
let pExpressions = sepBy pExpr spaces1 
 
let fparseString text = 
    match run pExpressions text with 
    | Success(result, _, _)   -> result 
    | Failure(errorMsg, _, _) -> failwith (sprintf "Failure: %s" errorMsg) 

let generateString level =
    let rec loop i =
        seq {
                if i < level then
                    yield "("
                    yield! loop level
                    yield " "
                    yield! loop (i+1)
                    yield ")"
                else 
                    yield "(x x)"
        }
    loop 0 |> String.concat ""
    
#time "on";;
let N = 5000
let s = generateString N;; 
let _ = fparseString s;;