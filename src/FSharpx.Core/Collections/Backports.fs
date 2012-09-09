namespace System.Collections.Generic

/// Represents a strongly-typed, read-only collection of elements.
type IReadOnlyCollection<'T> =
    inherit IEnumerable<'T>
    /// Gets the number of elements in the collection.
    abstract Count: int with get

/// Represents a read-only collection of elements that can be accessed by index.
type IReadOnlyList<'T> =
    inherit IReadOnlyCollection<'T>
    /// Gets the element at the specified index in the read-only list.
    abstract Item: int -> 'T with get

/// Represents a generic read-only collection of key/value pairs.
type IReadOnlyDictionary<'TKey, 'TValue> =
    inherit IReadOnlyCollection<KeyValuePair<'TKey, 'TValue>>
    /// Gets the element that has the specified key in the read-only dictionary.
    abstract Item: 'TKey -> 'TValue with get
    /// Gets an enumerable collection that contains the keys in the read-only dictionary.
    abstract Keys: IEnumerable<'TKey> with get
    /// Gets an enumerable collection that contains the values in the read-only dictionary.
    abstract Values: IEnumerable<'TValue> with get
