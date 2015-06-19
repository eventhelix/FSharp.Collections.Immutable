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
   
module ErrorStrings =
    [<Literal>]
    let InputMustBeNonNegative = "The input must be non-negative."

    [<Literal>]
    let ListsHaveDifferentLengths = "The lists have different lengths."