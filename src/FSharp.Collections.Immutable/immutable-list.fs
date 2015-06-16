[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharp.Collections.Immutable.ImmutableList
open System.Collections.Immutable 
open System.Collections.Generic
open System

let builder() = ImmutableList.CreateBuilder()

let ofSeq (seq: 'T seq) = ImmutableList.CreateRange seq
let ofList list =
    let rec convert (builder: ImmutableList<'T>.Builder) list =
        match list with
        |[] -> builder.ToImmutable()
        |head::tail -> builder.Add(head); convert builder list
    convert (builder()) list
let toArray (list: ImmutableList<_>) =
    let array = Array.zeroCreate list.Count
    list.CopyTo(array)
    array


let withItem index value (list: IImmutableList<_>) =
    list.SetItem(index, value)

let replace oldValue value (list: IImmutableList<_>) = list.Replace(oldValue, value, HashIdentity.Structural)
        

/// <summary>
/// Creates a list with all the items removed, but with the same sorting and ordering semantics as 
/// this list.
/// </summary>
/// <param name="list"></param>
let clear (list: IImmutableList<_>) = list.Clear()

/// Makes a copy of the list, and adds the specified object to the end of the copied list.
let add item (list: IImmutableList<_>) = list.Add item


/// Makes a copy of the list and adds the specified objects to the end of the copied list.
let append (list: IImmutableList<_>) items = list.AddRange items

/// Inserts the specified element at the specified index in a immutable list.
let insert index item (list: IImmutableList<_>) = list.Insert(index, item)

/// Inserts the specified elements at the specified index in the immutable list.
let insertRange index items (list: IImmutableList<_>) = list.InsertRange(index, items) // TODO: rename
    
let remove item (list: IImmutableList<_>) = list.Remove(item, HashIdentity.Structural)
let removeWith comparer item (list: IImmutableList<_>) =
    list.Remove(item, comparer)

let except items (list: IImmutableList<_>) = list.RemoveRange(items, HashIdentity.Structural)
let exceptWith (comparer: IEqualityComparer<_>) items (list: IImmutableList<_>) =
    list.RemoveRange(items, comparer)

let filter predicate (list: IImmutableList<_>) =
        Predicate(not << predicate)
        |> list.RemoveAll

let removeRange index (count: int) (list: IImmutableList<_>) = list.RemoveRange(index, count)

let removeAt index (list: IImmutableList<_>) = list.RemoveAt index

/// Searches for the specified object and returns the zero-based index of the first occurrence within the range
/// of elements in the < that starts at the specified index and
/// contains the specified number of elements.
let indexRangeWith comparer index count item (list: IImmutableList<_>) =
    list.IndexOf(item, index, count, comparer)
let indexRange index count item list =
    indexRangeWith HashIdentity.Structural index count item list
let indexFromWith comparer index item list =
    list |> indexRangeWith comparer index (list.Count - index) item 
let indexFrom index item list =
    indexFromWith HashIdentity.Structural index item list
let indexWith comparer item list =
    indexFromWith comparer 0 item list
let index item list = indexWith HashIdentity.Structural item list
    

/// Searches for the specified object and returns the zero-based index of the last occurrence within the
/// range of elements in the System.Collections.Immutable.IImmutableList`1 that contains the specified number
/// of elements and ends at the specified index.
let lastIndexRangeWith comparer index count item (list: IImmutableList<_>) =
    list.LastIndexOf(item, index, count, comparer)
let lastIndexRange index count item list =
    lastIndexRangeWith HashIdentity.Structural index count item list
let lastIndexFromWith comparer index item list =
    lastIndexRangeWith comparer index (index + 1) item list
let lastIndexFrom index item list =
    lastIndexFromWith HashIdentity.Structural index item list
let lastIndexWith comparer item list =
    list |> lastIndexFromWith comparer (list.Count - 1) item
let lastIndex item list = lastIndexWith HashIdentity.Structural item list
