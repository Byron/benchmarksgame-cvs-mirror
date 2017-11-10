// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Modification by Don Syme & Jomo Fisher to use null as representation
// of Empty node and to use a single Next element.
// Based on F# version by Robert Pickering
// Based on ocaml version by Troestler Christophe & Isaac Gouy
// multithreaded by Anthony Lloyd

type Next = { Left: Tree; Right: Tree }
and [<Struct>] Tree = | Tree of Next

[<EntryPoint>]
let main args =
    let minDepth = 4
    let maxDepth =if args.Length=0 then 10 else max (minDepth+2) (int args.[0])
    let stretchDepth = maxDepth + 1

    let rec make depth =
        if depth=0 then Tree Unchecked.defaultof<_>
        else Tree {Left = make (depth-1); Right = make (depth-1)}

    let rec check (Tree n) =
        if box n |> isNull then 1
        else 1 + check n.Left + check n.Right

    let stretchTree = System.Threading.Tasks.Task.Run(fun () ->
        let check = make stretchDepth |> check |> string
        "stretch tree of depth "+string stretchDepth+"\t check: "+check )

    let longLivedTree = System.Threading.Tasks.Task.Run(fun () ->
        let tree = make maxDepth
        let check = check tree |> string
        "long lived tree of depth "+string maxDepth+"\t check: "+check, tree )
    
    let loopTrees = Array.init ((maxDepth-minDepth)/2+1) (fun d ->
        let d = minDepth+d*2
        let n = 1 <<< (maxDepth - d + minDepth)
        let c = Array.Parallel.init System.Environment.ProcessorCount (fun _ ->
                    let mutable c = 0
                    for __ = 1 to n/System.Environment.ProcessorCount do
                        c <- c + (make d |> check)
                    c ) |> Array.sum
        string n+"\t trees of depth "+string d+"\t check: "+string c )

    stretchTree.Result |> stdout.WriteLine
    loopTrees |> Array.iter stdout.WriteLine
    fst longLivedTree.Result |> stdout.WriteLine
    exit 0