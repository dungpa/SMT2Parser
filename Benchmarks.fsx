#r "FParsec"
#r "FParsecCS"

#load "Ast.fs"
#load "Parser.fs"

open System.IO
open SMT2Parser.Parser

let rec allFiles dir =
    seq { yield! Directory.GetFiles dir
          yield! Seq.collect allFiles (Directory.GetDirectories dir) }

let allSMT2Files dir =
    dir |> allFiles |> Seq.filter (fun f -> f.EndsWith(".smt2") || f.EndsWith(".smt"))

let smt2Files = allSMT2Files (Path.Combine(__SOURCE_DIRECTORY__, "benchmarks"))
let sw = new System.Diagnostics.Stopwatch()

let time fn x =
    sw.Restart()
    fn x |> ignore
    sw.Stop()
    (float sw.ElapsedMilliseconds)/1000.0

let benchmarks() =
    smt2Files |> Seq.toArray 
              |> Array.iter (fun f ->
                                try
                                    printfn "Parsing %s" f
                                    let duration = time parseFile f
                                    printfn "Parsing succeeded in %.2f s" duration
                                with ex -> printfn "Parsing failed with the error: %s" ex.Message)

#time "on";;
//let fs = smt2Files |> Seq.toArray;;
let _ = benchmarks();;