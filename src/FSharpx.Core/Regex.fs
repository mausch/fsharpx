namespace FSharpx

open System.Text.RegularExpressions

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Regex =

    let replaceWithAcc folder state input (rx: Regex) =
        let acc = ref state
        let evaluator (m: Match) =
            let newState, result = folder !acc m
            acc := newState
            result
        let replacement: string = rx.Replace(input, evaluator)
        !acc, replacement
