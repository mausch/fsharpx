module FSharpx.RegexTests

open NUnit.Framework
open System.Text.RegularExpressions

[<Test>]
let ``replace with accumulator``() =
    let count, r = 
        Regex "%." 
        |> Regex.replaceWithAcc (fun s _ -> let s = s+1 in s, sprintf "@p%d" s) 0 "values (%d, %s)"
    Assert.AreEqual(2, count)
    Assert.AreEqual("values (@p1, @p2)", r)
    