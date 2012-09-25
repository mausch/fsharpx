module FSharpx.Tests.PartialLensTests

open FSharpx
open NUnit.Framework

type StateCity = {
    State: string
    City: string option
} with 
    static member cityP =
        let f (a: StateCity) =
            let setCity city = { a with City = Some city }
            a.City |> Option.map (fun city -> setCity, city)
        PartialLens f

type Search = {
    Name: string option
    StateCity: StateCity option
} with
    static member stateCityP =
        let f (a: Search) =
            let setStateCity sc = { a with StateCity = Some sc }
            a.StateCity |> Option.map (fun sc -> setStateCity, sc)
        PartialLens f

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
