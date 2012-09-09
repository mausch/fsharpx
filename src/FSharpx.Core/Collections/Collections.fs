namespace FSharpx

open System
open System.Collections
open System.Collections.Generic
#if NET40
open System.Diagnostics.Contracts
#endif
open System.Runtime.CompilerServices
            
module Seq =
    /// <summary>
    /// Adds an index to a sequence
    /// </summary>
    /// <param name="a"></param>
    let inline index a = Seq.mapi tuple2 a

    /// <summary>
    /// Returns the first element (with its index) for which the given function returns true.
    /// Return None if no such element exists.
    /// </summary>
    /// <param name="pred">Predicate</param>
    /// <param name="l">Sequence</param>
    let tryFindWithIndex pred l =
        l |> index |> Seq.tryFind (fun (_,v) -> pred v)

    let inline lift2 f l1 l2 = 
        seq {
            for i in l1 do
                for j in l2 do
                    yield f i j }
        

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<Extension>]
module Array = 
    let inline nth i arr = Array.get arr i
    let inline setAt i v arr = Array.set arr i v; arr

    [<Extension>]
    [<CompiledName("AsReadOnlyList")>]
    let asReadOnlyList (l: _ []) = 
        { new IReadOnlyList<_> with
            member x.Count = l.Length
            member x.Item with get i = l.[i]
            member x.GetEnumerator() = (l :> _ seq).GetEnumerator()
            member x.GetEnumerator() = l.GetEnumerator() }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<Extension>]
module List =
    /// Curried cons
    let inline cons hd tl = hd::tl
  
    let inline singleton x = [x]

    let inline lift2 f (l1: _ list) (l2: _ list) = 
        [ for i in l1 do
            for j in l2 do
                yield f i j ]

  
    let span pred l =
        let rec loop l cont =
            match l with
            | [] -> ([],[])
            | x::[] when pred x -> (cont l, [])
            | x::xs when not (pred x) -> (cont [], l)
            | x::xs when pred x -> loop xs (fun rest -> cont (x::rest))
            | _ -> failwith "Unrecognized pattern"
        loop l id
  
    let split pred l = span (not << pred) l
  
    let skipWhile pred l = span pred l |> snd
    let skipUntil pred l = split pred l |> snd
    let takeWhile pred l = span pred l |> fst
    let takeUntil pred l = split pred l |> fst
    
    let splitAt n l =
        let pred i = i >= n
        let rec loop i l cont =
            match l with
            | [] -> ([],[])
            | x::[] when not (pred i) -> (cont l, [])
            | x::xs when pred i -> (cont [], l)
            | x::xs when not (pred i) -> loop (i+1) xs (fun rest -> cont (x::rest))
            | _ -> failwith "Unrecognized pattern"
        loop 0 l id
  
    let skip n l = splitAt n l |> snd
    let take n l = splitAt n l |> fst

    let inline mapIf pred f =
        List.map (fun x -> if pred x then f x else x)

    [<Extension>]
    [<CompiledName("AsReadOnlyList")>]
    let asReadOnlyList (l: _ list) = 
        { new IReadOnlyList<_> with
            member x.Count = l.Length
            member x.Item with get i = l.[i]
            member x.GetEnumerator() = (l :> _ seq).GetEnumerator()
            member x.GetEnumerator() = (l :> _ seq).GetEnumerator() :> IEnumerator }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<Extension>]
module ResizeArray =
    [<Extension>]
    [<CompiledName("AsReadOnlyList")>]
    let asReadOnlyList (l: IList<_>) = 
        { new IReadOnlyList<_> with
            member x.Count = l.Count
            member x.Item with get i = l.[i]
            member x.GetEnumerator() = l.GetEnumerator()
            member x.GetEnumerator() = l.GetEnumerator() :> IEnumerator }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<Extension>]
module Dictionary =
    let tryFind key (d: IDictionary<_,_>) =
        match d.TryGetValue key with
        | true,v -> Some v
        | _ -> None

    [<Extension>]
    [<CompiledName("AsReadOnlyDictionary")>]
    let asReadOnlyDictionary (d: IDictionary<_,_>) : IReadOnlyDictionary<_,_> = 
        { new IReadOnlyDictionary<_,_> with
            member x.Count = d.Count
            member x.Keys = upcast d.Keys 
            member x.Values = upcast d.Values
            member x.Item with get key = d.[key]
            member x.ContainsKey key = d.ContainsKey key
            member x.TryGetValue(key, value) = d.TryGetValue(key, ref value)
            member x.GetEnumerator() = d.GetEnumerator()
            member x.GetEnumerator() = d.GetEnumerator() :> IEnumerator }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<Extension>]
module NameValueCollection =
    open System.Collections.Specialized
    open System.Linq

    /// <summary>
    /// Returns a new <see cref="NameValueCollection"/> with the concatenation of two <see cref="NameValueCollection"/>s
    /// </summary>
    /// <param name="a"></param>
    /// <param name="b"></param>
    [<Extension>]
    [<CompiledName("Concat")>]
    let concat a b = 
        let x = NameValueCollection()
        x.Add a
        x.Add b
        x

    /// <summary>
    /// In-place add of a key-value pair to a <see cref="NameValueCollection"/>
    /// </summary>
    /// <param name="x"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    let inline addInPlace (x: NameValueCollection) (a,b) = x.Add(a,b)

    /// Adds an element to a copy of an existing NameValueCollection
    let add name value (x: NameValueCollection) =
        let r = NameValueCollection x
        r.Add(name,value)
        r

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as an array of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    [<Extension>]
    [<CompiledName("ToArray")>]
    let toArray (a: NameValueCollection) =
        a.AllKeys
        |> Array.collect (fun k -> a.GetValues k |> Array.map (fun v -> k,v))

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a sequence of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    [<Extension>]
    [<CompiledName("ToEnumerable")>]
    let toSeq (a: NameValueCollection) =
        a.AllKeys
        |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a list of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    let inline toList a = toSeq a |> Seq.toList

    /// <summary>
    /// Creates a <see cref="NameValueCollection"/> from a list of key-value pairs
    /// </summary>
    /// <param name="l"></param>
    let fromSeq l =
        let x = NameValueCollection()
        Seq.iter (addInPlace x) l
        x

    [<Extension>]
    [<CompiledName("ToLookup")>]
    let toLookup a =
        let s = toSeq a
        s.ToLookup(fst, snd)

    let private getEnumerator (x: NameValueCollection) =
        let enum = x.GetEnumerator()
        let wrapElem (o: obj) = 
            let key = o :?> string
            let values = x.GetValues key
            KeyValuePair(key,values)
        { new IEnumerator<KeyValuePair<string,string[]>> with
            member e.Current = wrapElem enum.Current
            member e.MoveNext() = enum.MoveNext()
            member e.Reset() = enum.Reset()
            member e.Dispose() = ()
            member e.Current = box (wrapElem enum.Current) }

    [<Extension>]
    [<CompiledName("AsDictionary")>]
    let asDictionary (x: NameValueCollection) =
        let notimpl() = raise <| NotImplementedException()
        { new IDictionary<string,string[]> with
            member d.Count = x.Count
            member d.IsReadOnly = false 
            member d.Item 
                with get k = 
                    let v = x.GetValues k
                    if v = null
                        then raise <| KeyNotFoundException(sprintf "Key '%s' not found" k)
                        else v
                and set k v =
                    x.Remove k
                    for i in v do
                        x.Add(k,i)
            member d.Keys = upcast ResizeArray<string>(x.Keys |> Seq.cast)
            member d.Values = 
                let values = ResizeArray<string[]>()
                for i in 0..x.Count-1 do
                    values.Add(x.GetValues i)
                upcast values
            member d.Add v = d.Add(v.Key, v.Value)
            member d.Add(key,value) = 
                if key = null
                    then raise <| ArgumentNullException("key")
                if d.ContainsKey key
                    then raise <| ArgumentException(sprintf "Duplicate key '%s'" key, "key")
                d.[key] <- value
            member d.Clear() = x.Clear()
            member d.Contains item = x.GetValues(item.Key) = item.Value
            member d.ContainsKey key = x.[key] <> null
            member d.CopyTo(array,arrayIndex) = notimpl()
            member d.GetEnumerator() = getEnumerator x
            member d.GetEnumerator() = getEnumerator x :> IEnumerator
            member d.Remove (item: KeyValuePair<string,string[]>) = 
                if d.Contains item then
                    x.Remove item.Key
                    true
                else
                    false
            member d.Remove (key: string) = 
                let exists = d.ContainsKey key
                x.Remove key
                exists
            member d.TryGetValue(key: string, value: byref<string[]>) = 
                if d.ContainsKey key then
                    value <- d.[key]
                    true
                else false
            }

    [<Extension>]
    [<CompiledName("AsReadOnlyDictionary")>]
    let asReadOnlyDictionary (x: NameValueCollection) =
        { new IReadOnlyDictionary<string,string[]> with
            member d.Count = x.Count
            member d.Item with get k = x.GetValues k
            member d.Keys = Seq.cast x.Keys
            member d.Values =
                seq {
                    for i in 0..x.Count-1 do
                        yield x.GetValues i
                }
            member d.ContainsKey key = x.[key] <> null
            member d.TryGetValue(key: string, value: byref<string[]>) = 
                if d.ContainsKey key then
                    value <- d.[key]
                    true
                else false
            member d.GetEnumerator() = getEnumerator x
            member d.GetEnumerator() = getEnumerator x :> IEnumerator }

    [<Extension>]
    [<CompiledName("AsLookup")>]
    let asLookup (this: NameValueCollection) =
        let getEnumerator() = 
            let e = this.GetEnumerator()
            let wrapElem (o: obj) = 
                let key = o :?> string
                let values = this.GetValues key :> seq<string>
                { new IGrouping<string,string> with
                    member x.Key = key
                    member x.GetEnumerator() = values.GetEnumerator()
                    member x.GetEnumerator() = values.GetEnumerator() :> IEnumerator }
  
            { new IEnumerator<IGrouping<string,string>> with
                member x.Current = wrapElem e.Current
                member x.MoveNext() = e.MoveNext()
                member x.Reset() = e.Reset()
                member x.Dispose() = ()
                member x.Current = box (wrapElem e.Current) }
                      
        { new ILookup<string,string> with
            member x.Count = this.Count
            member x.Item 
                with get key = 
                    match this.GetValues key with
                    | null -> Seq.empty
                    | a -> upcast a
            member x.Contains key = this.Get key <> null
            member x.GetEnumerator() = getEnumerator()
            member x.GetEnumerator() = getEnumerator() :> IEnumerator }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReadOnlyDictionary =
    let tryFind key (d: IReadOnlyDictionary<_,_>) =
        try
            Some (d.[key])
        with :? KeyNotFoundException -> None