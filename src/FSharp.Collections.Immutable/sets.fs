namespace FSharp.Collections.Immutable

type ISet<'T> = System.Collections.Immutable.IImmutableSet<'T>

type HashSet<'T> = System.Collections.Immutable.ImmutableHashSet<'T>

type HashSetBuilder<'T> = HashSet<'T>.Builder

type internal HashSetFactory = System.Collections.Immutable.ImmutableHashSet

module HashSet =
    let inline empty<'T> = HashSetFactory.Create<'T>()

    let inline ofBuilder (hashSetBuilder: HashSetBuilder<_>) =
           checkNotNull (nameof hashSetBuilder)  hashSetBuilder
           hashSetBuilder.ToImmutable()
    let inline ofComparer<'T> comparer = HashSetFactory.Create<'T>(equalityComparer = comparer)
    let inline ofSeq items =
        checkNotNull (nameof items) items
        HashSetFactory.CreateRange(items)
    let inline ofArray items = HashSetFactory.CreateRange(items)

    let inline check (set: HashSet<_>) = checkNotNull (nameof set) set

    let inline builder() = HashSetFactory.CreateBuilder()
    let inline builderWithComparer comparer = HashSetFactory.CreateBuilder(comparer)

    let inline toBuilder set : HashSetBuilder<_> = check set; set.ToBuilder()

    let inline singleton item = empty.Add(item)

    let inline keyComparer set = check set; set.KeyComparer

    let inline isEmpty set = check set; set.IsEmpty

    let inline length set = check set; set.Count

    let inline contains value set = check set; set.Contains value

    let inline pick chooser set = check set; set |> Seq.pick chooser
    let inline tryPick chooser set = check set; set |> Seq.tryPick chooser
    let inline vTryPick chooser set =
        check set
        match set |> Seq.tryPick chooser with
        | Some value -> ValueSome value
        | None -> ValueNone

    let inline iter action map = check map; map |> Seq.iter action

    let inline exists predicate map = check map; map |> Seq.exists predicate

    let inline add value set : HashSet<_> = check set; set.Add(value)
    let inline union set values : HashSet<_> = check set; values |> set.Union

    let inline remove value set : HashSet<_> = check set; set.Remove value
    let inline difference values set : HashSet<_> = check set; values |> set.Except

    let inline clear set: HashSet<_> = check set; set.Clear()

    let inline filter predicate set =
        set |> Seq.filter predicate |> empty.Union

    let inline where predicate set =
        set |> Seq.where predicate |> empty.Union

    let inline map mapping set' =
        set' |> Seq.map mapping

    let inline forall predicate set =
        set |> Seq.forall predicate

    let inline count (set:HashSet<_>) = check set; set.Count

type SortedSet<'T> = System.Collections.Immutable.ImmutableSortedSet<'T>

type SortedSetBuilder<'T> = SortedSet<'T>.Builder

type internal SortedSetFactory =
    System.Collections.Immutable.ImmutableSortedSet

module SortedSet =
    let inline empty<'T> = SortedSetFactory.Create<'T>()

    let inline ofBuilder (sortedSetBuilder: SortedSetBuilder<_>) =
           checkNotNull (nameof sortedSetBuilder)  sortedSetBuilder
           sortedSetBuilder.ToImmutable()
    let inline ofComparer<'T> comparer = SortedSetFactory.Create<'T>(comparer = comparer)
    let inline ofSeq items =
        checkNotNull (nameof items) items
        SortedSetFactory.CreateRange(items)
    let inline ofArray items = SortedSetFactory.CreateRange(items)

    let inline check (sortedSet: SortedSet<_>) = checkNotNull (nameof sortedSet) sortedSet

    let inline builder() = SortedSetFactory.CreateBuilder()
    let inline builderWithComparer comparer = SortedSetFactory.CreateBuilder(comparer)

    let inline toBuilder set : SortedSetBuilder<_> = check set; set.ToBuilder()

    let inline singleton item = empty.Add(item)

    let inline keyComparer set = check set; set.KeyComparer

    let inline isEmpty set = check set; set.IsEmpty

    let inline length set = check set; set.Count

    let inline contains value set = check set; set.Contains value

    let inline pick chooser set = check set; set |> Seq.pick chooser
    let inline tryPick chooser set = check set; set |> Seq.tryPick chooser
    let inline vTryPick chooser set =
        check set
        match set |> Seq.tryPick chooser with
        | Some value -> ValueSome value
        | None -> ValueNone

    let inline iter action map = check map; map |> Seq.iter action

    let inline exists predicate map = check map; map |> Seq.exists predicate

    let inline add value set : SortedSet<_> = check set; set.Add(value)
    let inline union set values : SortedSet<_> = check set; values |> set.Union

    let inline remove value set : SortedSet<_> = check set; set.Remove value
    let inline difference values set : SortedSet<_> = check set; values |> set.Except

    let inline clear set: SortedSet<_> = check set; set.Clear()

    let inline filter predicate set =
        set |> Seq.filter predicate |> empty.Union

    let inline where predicate set =
        set |> Seq.where predicate |> empty.Union

    let inline map mapping set' =
        set' |> Seq.map mapping

    let inline forall predicate set =
        set |> Seq.forall predicate

    let inline count (set:SortedSet<_>) = check set; set.Count
