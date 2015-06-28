namespace FSharp.Collections.Immutable

type IMap<'Key, 'Value> = System.Collections.Immutable.IImmutableDictionary<'Key, 'Value>

type HashMap<'Key, 'Value> =
    System.Collections.Immutable.ImmutableDictionary<'Key, 'Value>

type HashMapBuilder<'Key, 'Value> = HashMap<'Key, 'Value>.Builder

module HashMap =
    let inline check (map: HashMap<_, _>) = checkNotNull "map" map

    let inline isEmpty map = check map; map.IsEmpty
    let inline length map = check map; map.Count

    let inline keyComparer map = check map; map.KeyComparer

    let inline containsKey key map = check map; map.ContainsKey key

    let inline find key map = check map; map.[key]
    let inline tryFind key map =
        check map
        let mutable value = Unchecked.defaultof<_>
        if map.TryGetValue(key, &value) then Some value else None

    let inline add key value map : HashMap<_,_> = check map; map.Add(key, value)
    let inline append map pairs : HashMap<_,_> =
        check map
        checkNotNull "pairs" pairs
        map.AddRange pairs

    let inline remove key map : HashMap<_,_> = check map; map.Remove key
    let inline except keys map : HashMap<_,_> = check map; map.RemoveRange keys

    let inline clear map: HashMap<_,_> = check map; map.Clear()

    let inline toBuilder map = check map; map.ToBuilder()

    let inline ofBuilder (mapBuilder: HashMap<_,_>.Builder) =
        checkNotNull "mapBuilder" mapBuilder
        mapBuilder.ToImmutable()
    // consider alternate implementation using range functions
    let inline filter predicate map =
        let builder = toBuilder map
        for kvp in map do
            if predicate kvp.Key kvp.Value then
                builder.Add kvp
        builder.ToImmutable()





    

