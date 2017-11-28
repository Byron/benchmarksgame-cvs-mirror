// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// ported from C# version adding native by Anthony Lloyd

#nowarn "9"

open Microsoft.FSharp.NativeInterop

[<EntryPoint>]
let main args =

    let inline run n fact taskSize taskId =
        use p = fixed &(Array.zeroCreate n).[0]
        use pp = fixed &(Array.zeroCreate n).[0]
        use count = fixed &(Array.zeroCreate n).[0]

        let inline firstPermutation idx =
            for i = 0 to n-1 do NativePtr.set p i i
            let mutable idx = idx
            for i = n-1 downto 1 do
                let d = idx/NativePtr.get fact i
                NativePtr.set count i d
                if d<>0 then
                    idx <-
                        for j = 0 to i do
                            NativePtr.get p j
                            |> NativePtr.set pp j
                        for j = 0 to i do
                            NativePtr.get pp ((j+d) % (i+1))
                            |> NativePtr.set p j
                        idx % NativePtr.get fact i

        let inline nextPermutation() =
            let mutable first = NativePtr.get p 1
            NativePtr.get p 0 |> NativePtr.set p 1
            NativePtr.set p 0 first
            let mutable i = 1
            while let c=NativePtr.get count i+1 in NativePtr.set count i c;c>i do
                NativePtr.set count i 0
                i <- i+1
                let next = NativePtr.get p 1
                NativePtr.set p 0 next
                for j = 1 to i-1 do NativePtr.get p (j+1) |> NativePtr.set p j
                NativePtr.set p i first
                first <- next
            first

        let inline countFlips first =
            if first=0 then 0
            elif NativePtr.get p first=0 then 1
            else
                for i = 0 to n-1 do NativePtr.get p i |> NativePtr.set pp i
                let rec loop flips first =
                    let mutable lo = 1
                    let mutable hi = first-1
                    while lo<hi do
                        let t = NativePtr.get pp lo
                        NativePtr.get pp hi |> NativePtr.set pp lo
                        NativePtr.set pp hi t
                        lo <- lo+1
                        hi <- hi-1
                    let tp = NativePtr.get pp first
                    if NativePtr.get pp tp=0 then flips
                    else
                        NativePtr.set pp first first
                        loop (flips+1) tp
                loop 2 first

        firstPermutation (taskId*taskSize)
        let mutable chksum = countFlips (NativePtr.get p 0)
        let mutable maxflips = chksum
        for i = 1 to taskSize-1 do
            let flips = nextPermutation() |> countFlips
            chksum <- chksum + (1-(i%2)*2) * flips
            if flips>maxflips then maxflips <- flips
        chksum, maxflips

    let n = if args.Length=0 then 7 else int args.[0]
    use fact = fixed &(Array.zeroCreate (n+1)).[0]
    NativePtr.set fact 0 1
    let mutable factn = 1
    for i = 1 to n do
        factn <- factn * i
        NativePtr.set fact i factn

    let chksum, maxFlips =
        let taskSize = factn / System.Environment.ProcessorCount
        Array.init System.Environment.ProcessorCount
            (fun i -> async { return run n fact taskSize i})
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.reduce (fun (c1,f1) (c2,f2) -> c1+c2,max f1 f2) 
           
    string chksum+"\nPfannkuchen("+string n+") = "+string maxFlips
    |> stdout.WriteLine

    exit 0