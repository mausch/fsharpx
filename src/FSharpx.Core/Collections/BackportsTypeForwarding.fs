module FSharpx.Core.AssemblyInfo.Backports

open System.Collections.Generic
open System.Runtime.CompilerServices

[<assembly: TypeForwardedTo(typedefof<IReadOnlyCollection<_>>)>]
[<assembly: TypeForwardedTo(typedefof<IReadOnlyList<_>>)>]
[<assembly: TypeForwardedTo(typedefof<IReadOnlyDictionary<_,_>>)>]
()