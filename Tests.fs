module SMT2Parser.Tests

open System.IO

open Xunit

open SMT2Parser.Ast
open SMT2Parser.Parser

let testCommand tests expecteds =
    List.iter2 (fun test expected -> let result = test |> parseCommand |> string
                                     Assert.Equal<string>(expected, result)) tests expecteds

let testParse tests expecteds =
    let results = tests |> parse |> List.map string
    let expectedResults = expecteds |> parse |> List.map string
    results |> List.iter2 (fun e r -> Assert.Equal<string>(e, r)) expectedResults

[<Fact>]
let testNullaryCommand() =
    let tests = 
        [ "(check-sat)";
          "(get-assertions)";
          "(get-proof)";
          "(get-unsat-core)";
          "(get-assignment)";
          "(exit)";
        ]
    testCommand tests tests

[<Fact>]
let testSetLogic() =
    let tests = 
        [ "( set-logic QF_LIA )";
          "(set-logic UFLRA )";
        ]
    let expecteds = 
        [ "(set-logic QF_LIA)";
          "(set-logic UFLRA)";
        ]
    testCommand tests expecteds

[<Fact>]
let testSetOption() =
    let tests = 
        [ "(set-option :print-success true)";
          "(set-option :expand-definitions false)";
          "(set-option :interactive-mode true)";
          "(set-option :produce-proofs false)";
          "(set-option :produce-unsat-cores true)";
          "(set-option :produce-models false)";
          "(set-option :produce-assignments true)";
          "(set-option :regular-output-channel \"ok\")";
          "(set-option :diagnostic-output-channel \"not ok\")";
          "(set-option :random-seed 10)";
          "(set-option :verbosity 0)";
          "(set-option :my_attribute (humpty dumpty))"; // test <attribute> option
          "(set-option :status unsat)"; 
        ]
    testCommand tests tests

[<Fact>]
let testSetInfo() =
    let tests = 
        [ "(set-info :smt-lib-version 2.0)";
          "(set-info :category \"industrial\")";
          "(set-info :status unsat)";
        ]
    testCommand tests tests

[<Fact>]
let testDeclareSort() =
    let tests = 
        [ "(declare-sort Type 1)";
          "(declare-sort A 2)";
          "(declare-sort B 0)";
        ]
    testCommand tests tests

[<Fact>]
let testDefineSort() =
    let tests = 
        [ "(define-sort A() (Array Int Int Int))";
          "(define-sort Set(T) (Array T Bool))";
          "(define-sort IList () (List Int))";
          "(define-sort List-Set (T) (Array (List T) Bool))";
          "(define-sort I () Int)";
        ]
    let expecteds = 
        [ "(define-sort A () (Array Int Int Int))";
          "(define-sort Set (T) (Array T Bool))";
          "(define-sort IList () (List Int))";
          "(define-sort List-Set (T) (Array (List T) Bool))";
          "(define-sort I () Int)";
        ]
    testCommand tests expecteds

[<Fact>]
let testDeclareFun() =
    let tests = 
        [ "(declare-fun def () Real)";
          "(declare-fun pv10 () Int)";
          "(declare-fun divide (Real Real) Real)";
          "(declare-fun inv ((Array Int (Array Int Real))) (Array Int (Array Int Real)))";
        ]
    testCommand tests tests

[<Fact>]
let testDefineFun() =
    let tests = 
        [ "(define-fun conjecture () Bool (=> (and (=> p q) (=> q r)) (=> p r)))";
          "(define-fun demorgan () Bool (= (and a b) (not (or (not a) (not b)))))";
          "(define-fun mydiv ((x Real) (y Real)) Real (if (not (= y 0.0)) (/ x y) 0.0))";
        ]
    testCommand tests tests

[<Fact>]
let testPushPop() =
    let tests = 
        [ "(push 1)";
          "(pop 2)";
        ]
    testCommand tests tests

[<Fact>]
let testAssert() =
    let tests = 
        [ "(assert x)";
          "(assert (= y 0))";
        ]
    testCommand tests tests

[<Fact>]
let testGetValue() =
    let tests = 
        [ "(get-value (x))";
          "(get-value (+ y z))";
        ]
    testCommand tests tests

[<Fact>]
let testGetOption() =
    let tests = 
        [ "(get-option :print-success)";
          "(get-option :expand-definitions)";
        ]
    testCommand tests tests

[<Fact>]
let testGetInfo() =
    let tests = 
        [ "(get-info :error-behaviour)";
          "(get-info :name)";
          "(get-info :authors)"
          "(get-info :version)"
          "(get-info :status)";
          "(get-info :reason-unknown)";
          "(get-info :my_attribute)"; // test <keyword> flag
        ]
    testCommand tests tests

// Following tests are extracted from Z3's guide at http://rise4fun.com/z3/tutorial/guide
[<Fact>]
let fileTestBasic() =
    let s = File.ReadAllText "..\\..\\tests\\test01.smt2"
    testParse s s

[<Fact>]
let fileTestScope() =    
    let s = File.ReadAllText "..\\..\\tests\\test02.smt2"
    testParse s s

[<Fact>]
let fileTestConfiguration() =    
    let s = File.ReadAllText "..\\..\\tests\\test03.smt2"
    testParse s s

[<Fact>]
let fileTestDefineSort() =    
    let s = File.ReadAllText "..\\..\\tests\\test04.smt2"
    testParse s s

[<Fact>]
let fileTestArith() =    
    let s = File.ReadAllText "..\\..\\tests\\test05.smt2"
    testParse s s

[<Fact>]
let fileTestDivMinus() =    
    let s = File.ReadAllText "..\\..\\tests\\test06.smt2"
    testParse s s

[<Fact>]
let fileTestBitvec() =    
    let s = File.ReadAllText "..\\..\\tests\\test07.smt2"
    testParse s s

[<Fact>]
let fileTestQuantifier() =    
    let s = File.ReadAllText "..\\..\\tests\\test08.smt2"
    testParse s s

[<Fact>]
let fileTestPattern() =    
    let s = File.ReadAllText "..\\..\\tests\\test09.smt2"
    testParse s s