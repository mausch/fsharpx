namespace FSharpx

type Lens<'a, 'b> = {
    Get: 'a -> 'b
    Set: 'b -> 'a -> 'a
    Update: ('b -> 'b) -> 'a -> 'a
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =
    let inline get a (l: Lens<_,_>) = l.Get a
    let inline set v a (l: Lens<_,_>) = l.Set v a
    let inline update f (l: Lens<_,_>) = l.Update f

    let debug (get,set,update) (l: Lens<_,_>) =
        { Get = fun a -> get a; l.Get a
          Set = fun b a -> set b a; l.Set b a
          Update = fun f a -> update a; l.Update f a }

    let debugPrint name =
        debug (printfn "%s get %A" name, printfn "%s set %A %A" name, printfn "%s update %A" name)

    /// Creates a state-lens. Defines Update in terms of Get/Set.
    let slens(get, set) = 
        { Get = get
          Set = set
          Update = fun f a -> set (f(get a)) a }
        //|> debugPrint "slens"

    /// Creates an update-lens. Defines Set in terms of Update.
    let ulens(get, update) = 
        { Get = get
          Set = konst >> update
          Update = update }
        // |> debugPrint "ulens"

    /// Sequentially composes two lenses
    let inline compose (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        { Get = l2.Get >> l1.Get
          Set = l1.Set >> l2.Update
          Update = l1.Update >> l2.Update }
        // |> debugPrint "compose"

    /// Composes two lenses through a sum in the source
    let inline choice (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        { Get = Choice.choice l1.Get l2.Get
          Set = fun b -> Choice.bimap (l1.Set b) (l2.Set b)
          Update = fun f -> Choice.bimap (l1.Update f) (l2.Update f) }
        // |> debugPrint "choice"

    /// Pair two lenses  
    let inline pair (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        slens((fun (a,b) -> (l1.Get a, l2.Get b)),
              (fun (a,c) (b,d) -> (l1.Set a b, l2.Set c d)))

    /// <summary>
    /// <paramref name="pred"/> is applied to source. 
    /// If true, <paramref name="lensTrue"/> is selected.
    /// If false, <paramref name="lensFalse"/> is selected.
    /// </summary>
    let cond pred lensTrue lensFalse =
        let inline choose a = if pred a then lensTrue else lensFalse
        { Get = fun a -> choose a |> get a
          Set = fun b a -> choose a |> set b a
          Update = 
            fun f a ->
                let l = choose a 
                update f l a }

    /// Applies a lens in the 'get' direction within a state monad      
    let getState l = 
        fun a -> get a l, a

    /// Applies a lens in the 'set' direction within a state monad
    let setState l v = 
        fun a -> (), set v a l

    /// Update through a lens within a state monad
    let updateState l f =
        fun a -> (), update f l a

    /// Modifies the state in a state monad and returns the original value.
    let getAndModifyState l f = 
        State.state {
            let! v = State.getState
            do! updateState l f
            return v
        }

    /// Modifies the state in a state monad and returns the modified value.
    let modifyAndGetState l f = 
        State.state {
            do! updateState l f
            return! State.getState
        }

    /// Gets/sets the fst element in a pair
    let fst<'a,'b> : Lens<'a * 'b,'a> =
        let inline set v a = v, Operators.snd a
        slens(Operators.fst, set)

    /// Gets/sets the snd element in a pair
    let snd<'a,'b> : Lens<'a * 'b,'b> =
        let inline set v a = Operators.fst a, v
        slens(Operators.snd, set) 

    /// Identity lens
    let id<'a> : Lens<'a,'a> = slens(Operators.id, konst)

    let codiag<'a> = choice id id

    /// Lens for a particular value in a set
    let forSet value =
        let inline set contains = (if contains then Set.add else Set.remove) value
        slens(Set.contains value, set)

    /// Lens for a particular key in a map
    let forMap key = 
        let set = 
            function
            | Some value -> Map.add key value
            | None -> Map.remove key
        slens(Map.tryFind key, set)

    /// Lens for a particular position in an array
    let forArray i = 
        let inline set v = Array.copy >> Array.setAt i v
        slens(Array.nth i, set)

    /// Lens for a particular position in a list
    let forList i =
        let inline get l = List.nth l i
        let inline update f = List.mapi (fun j e -> if j = i then f e else e)
        ulens(get, update)
        //|> debugPrint "forList"

    /// Creates a lens that maps the given lens in a list
    let listMap (l: Lens<_,_>) =
        slens(List.map l.Get, List.map2 l.Set)
        //|> debugPrint "listMap"

    /// Creates a lens that maps the given lens in an array
    let arrayMap (l: Lens<_,_>) = 
        slens(Array.map l.Get, Array.map2 l.Set)

    /// Creates a lens that maps the given lens in a sequence
    let seqMap (l: Lens<_,_>) = 
        slens(Seq.map l.Get, Seq.map2 l.Set)

    /// Applies an isomorphism to the value viewed through a lens
    let xmap f g (l: Lens<_,_>) = 
        slens(l.Get >> f, g >> l.Set)

    /// Converts a lens that views a list into a lens that views an array
    let inline listToArray l = xmap List.toArray Array.toList l

// not side-effect free
//    let forRef =
//        { Get = (!)
//          Set = fun v a -> a := v; a }

    let ignore<'a> = slens(ignore, fun _ v -> v)

    module Operators = 
        let inline (>>|) l1 l2 = compose l2 l1
        let inline (|<<) l1 l2 = compose l1 l2
        let inline (.|.) l1 l2 = choice l2 l1
        let inline ( ***) l1 l2 = pair l2 l1
        let inline (+=) l v = update ((+) v) l
        let inline (-=) l v = update ((-) v) l
        let inline (/=) l v = update ((/) v) l
        let inline ( *=) l v = update (( *) v) l
        let inline (|||=) l v = update ((|||) v) l
        let inline (||=) l v = update ((||) v) l
        let inline (&&&=) l v = update ((&&&) v) l
        let inline (&&=) l v = update ((&&) v) l
        let inline (=!) l v = fun a -> set v a l

    module StateOperators = 
        let inline (+=) l v = updateState l ((+) v)
        let inline (-=) l v = updateState l ((-) v)
        let inline (/=) l v = updateState l ((/) v)
        let inline ( *=) l v = updateState l (( *) v)
        let inline (|||=) l v = updateState l ((|||) v)
        let inline (||=) l v = updateState l ((||) v)
        let inline (&&&=) l v = updateState l ((&&&) v)
        let inline (&&=) l v = updateState l ((&&) v)
        let inline (=!) l v = setState l v
