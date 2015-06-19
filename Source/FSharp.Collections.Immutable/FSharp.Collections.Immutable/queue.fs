namespace FSharp.Collections.Immutable

type IQueue<'T> = System.Collections.Immutable.IImmutableQueue<'T>

type Queue<'T> = System.Collections.Immutable.ImmutableQueue<'T>


[<RequireQualifiedAccess; CompiledName("ImmutableStackModule")>]
module Queue =

    type internal QueueFactory = System.Collections.Immutable.ImmutableQueue

    let inline private check (queue: IQueue<_>) = checkNotNull "queue" queue

    let empty<'T> : Queue<'T> = QueueFactory.Create<'T>()

    let ofSeq(source: 'T seq): Queue<'T> = QueueFactory.CreateRange source

    let isEmpty queue = check queue; queue.IsEmpty

    let clear queue: IQueue<_> = check queue; queue.Clear()

    let enqueue item queue: IQueue<_> = check queue; queue.Enqueue item

    let peek queue = check queue; queue.Peek()
    
    let tail queue: IQueue<_> = check queue; queue.Dequeue()
    let dequeue queue = peek queue, tail queue


    //////////

    let (|Cons|Nil|) queue = // consider renaming
        if isEmpty queue then Nil else Cons(peek queue, tail queue)

    let head queue = tail queue


    let filter predicate queue =
        let rec loop queue result =
            match queue with
            |Cons(head, tail) ->
                loop tail <| if predicate head then enqueue head result else result
            |Nil -> result

        loop queue <| clear queue


