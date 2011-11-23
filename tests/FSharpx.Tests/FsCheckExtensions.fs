namespace FsCheck

module Arb =
    let twoListsOfEqualLength<'a,'b> = 
        gen {
            let! listA = Gen.listOf Arb.generate<'a>
            let! listB = Gen.listOfLength listA.Length Arb.generate<'b>
            return listA, listB
        } |> Arb.fromGen

    let threeListsOfEqualLength<'a,'b,'c> = 
        gen {
            let! listA = Gen.listOf Arb.generate<'a>
            let! listB = Gen.listOfLength listA.Length Arb.generate<'b>
            let! listC = Gen.listOfLength listA.Length Arb.generate<'c>
            return listA, listB, listC
        } |> Arb.fromGen

    let listToListFunWithEqualLength<'a> =  
        Arb.generate<'a -> 'a>
        |> Gen.map List.map
        |> Arb.fromGen
