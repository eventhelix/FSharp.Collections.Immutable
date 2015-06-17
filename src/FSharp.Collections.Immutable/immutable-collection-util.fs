[<AutoOpen>]
module FSharp.Collections.Immutable.ImmutableCollectionUtil

let inline checkNotNull name arg = if Unchecked.equals arg null then nullArg name
    