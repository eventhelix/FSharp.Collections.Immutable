namespace FSharp.Collections.Immutable

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ImmutableStack =
    let check (stack: System.Collections.Immutable.IImmutableStack<_>) =
        checkNotNull "stack" stack
    
    let push head stack = check stack; stack.Push head

    let peek stack = check stack; 

    let pop stack = check stack; stack.Peek(), stack.Pop()
    let tail stack = check stack; stack.Pop()
    
    let (|Cons|Nil|) stack =
        check stack
        if stack.IsEmpty then Nil
        else Cons(stack.Peek(), stack.Pop())
    

    let empty<'T> = System.Collections.Immutable.ImmutableStack.Create<'T>()

    /////////////

    let cons head stack = check stack; push head stack



