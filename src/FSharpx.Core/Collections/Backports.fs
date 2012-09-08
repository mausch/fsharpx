namespace System.Collections.Generic

type IReadOnlyCollection<'T> =
    inherit IEnumerable<'T>
    abstract Count: int with get

type IReadOnlyList<'T> =
    inherit IReadOnlyCollection<'T>
    abstract Item: int -> 'T with get

type IReadOnlyDictionary<'TKey, 'TValue> =
    inherit IReadOnlyCollection<KeyValuePair<'TKey, 'TValue>>
    abstract Item: 'TKey -> 'TValue with get
    abstract Keys: IEnumerable<'TKey> with get
    abstract Values: IEnumerable<'TValue> with get
