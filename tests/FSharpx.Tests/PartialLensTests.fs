module FSharpx.Tests.PartialLensTests

open FSharpx
open NUnit.Framework

type StateCity = {
    State: string
    City: string option
} with 
    static member cityP =
        let get (a: StateCity) = a.City
        let set (a: StateCity) city = { a with City = Some city }
        PartialLens.fromGetSet get set

type Search = {
    Name: string option
    StateCity: StateCity option
} with
    static member stateCityP =
        let get (a: Search) = a.StateCity
        let set (a: Search) sc = { a with StateCity = Some sc }
        PartialLens.fromGetSet get set

let searchCity = Search.stateCityP |> PartialLens.compose StateCity.cityP

[<Test>]
let setAllSome() =
    let allSome = 
        { Name = Some "John"; StateCity = Some { State = "Florida"; City = Some "Miami" } }
    let orlando = searchCity |> PartialLens.set "Orlando" allSome
    Assert.AreEqual(Some "Orlando", orlando.StateCity.Value.City)
    
[<Test>]
let setNoCity() =
    let noCity = 
        { Name = Some "John"; StateCity = Some { State = "Florida"; City = None } }
    let orlando = searchCity |> PartialLens.set "Orlando" noCity
    Assert.AreEqual(None, orlando.StateCity.Value.City)

[<Test>]
let setNoState() =
    let noState = 
        { Name = Some "John"; StateCity = None }
    let orlando = searchCity |> PartialLens.set "Orlando" noState
    Assert.AreEqual(None, orlando.StateCity)
