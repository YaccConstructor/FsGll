module FsGll.Mutable.PriorityQueue

open System

let inline private parent i = (i - 1) / 2
let inline private left  i = ((i + 1) <<< 1) - 1
let inline private right i = ((i + 1) <<< 1)

type PriorityQueue<'a> (cmp: 'a -> 'a -> int) = 
    let mutable _capacity = 4
    let mutable _count = 0
    let mutable _arr: 'a [] = Array.zeroCreate (_capacity)
    member this.Count = _count
    member this.Add (x: 'a) = 
        if _count + 1 >= _capacity then 
            _capacity <- _capacity <<< 1
            let newArr = Array.zeroCreate _capacity
            _arr.CopyTo(newArr, 0)
            _arr <- newArr
        let mutable i = _count
        _count <- _count + 1
        while i > 0 && cmp (_arr.[parent i]) x > 0 do 
            _arr.[i] <-_arr.[parent i]
            i <- parent i
        _arr.[i] <- x
        
    member this.Peek () = _arr.[0]
    member this.Pop () = 
        let x = _arr.[0]
        let y = _arr.[_count - 1]
        _arr.[0] <- y
        _count <- _count - 1
        let mutable cont = true
        let mutable i = 0
        while cont do
            let mutable minInd = i
            if left i < _count && cmp (_arr.[minInd]) (_arr.[left i]) > 0 then minInd <- left i
            if right i < _count && cmp (_arr.[minInd]) (_arr.[right i]) > 0 then minInd <- right i
            if minInd <> i then 
                _arr.[i] <- _arr.[minInd]
                i <- minInd
                _arr.[i] <- y
            else cont <- false
        _arr.[i] <- y
        x
        
        