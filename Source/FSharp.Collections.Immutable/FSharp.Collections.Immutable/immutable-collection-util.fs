#if INTERACTIVE
namespace global
#else
namespace FSharp.Collections.Immutable
#endif

[<AutoOpen>]
module ImmutableCollectionUtil =
    let inline checkNotNull name arg =
        match arg with
        |null -> nullArg name
        |_ -> ()
    