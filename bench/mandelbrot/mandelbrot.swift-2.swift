/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Ralph Ganszky
*/

import Glibc
import Cdispatch

struct Line {
    let row: Int
    let size: Int
    var buffer: [UInt8]
    
    init(_ row: Int, _ width: UInt) {
        self.row = row
        size = Int((width+7)/8)
        buffer = [UInt8](count: Int((width+7)/8), repeatedValue: 0)
    }
}

func writeLine(io: dispatch_io_t, _ data: dispatch_data_t, _ queue: dispatch_queue_t, _ last: Bool) {
    dispatch_io_write(io, 0, data, queue) {
	(done: Bool, dd:dispatch_data_t!, errorCode:Int32) -> Void in
	if errorCode != 0 {
	    print("FileOutputStream: Error writing data to channel")
	}
	if done {
	    if last {
		dispatch_io_close(io, 0)
	    }
	}
    }
}

let n: Int
if Process.arguments.count > 1 {
    n = Int(Process.arguments[1]) ?? 200
} else {
    n = 200
}

let w = Double(n)
let h = Double(n)

let Iter = 50
let Zero = 0.0
let Limit = 2.0

// Use a map to store the result
var lines = [Int:dispatch_data_t]()
var nextRow = 0
var firstWait = 0

let queue = dispatch_queue_create("StoreQueue", nil)
let group = dispatch_group_create()

let cqueue = dispatch_get_global_queue(Int(DISPATCH_QUEUE_PRIORITY_DEFAULT), 0)
let cgroup = dispatch_group_create()

let wqueue = dispatch_queue_create("WriteQueue", nil)
let wgroup = dispatch_group_create()

// Block the group at the beginning will be relealsed when the channel gets closed
dispatch_group_enter(group)

// Open a dispatch io chanel to stdout
let stdout = dispatch_io_create(0, STDOUT_FILENO, queue) { (errorcode: Int32) -> Void in
    if errorcode != 0 {
        print("FileOutputStream: Error creating channel")
    } else {
        // Closing the channel terminates the program
        dispatch_group_leave(group)
    }
}

let header = "P4\n\(n) \(n)\n"
if let data = header.dataUsingEncoding(NSUTF8StringEncoding) {
    let dispData = dispatch_data_create(data.bytes, data.length, nil, nil)
        dispatch_io_write(stdout, 0, dispData, queue) {
            (done: Bool, data:dispatch_data_t!, errorCode:Int32) -> Void in
            if errorCode != 0 {
                print("FileOutputStream: Error writing data to channel")
            }
        }
}

for y in Zero.stride(to: h, by: 1.0) {
    var line = Line(Int(y), UInt(n))
    dispatch_group_async(cgroup, cqueue) {
        
        var bitNum = 0
        var byteAcc:UInt8 = 0
        
        var lpos = 0
        for x in Zero.stride(to: w, by: 1.0) {
            var (Zr, Zi, Tr, Ti) = (Zero, Zero, Zero, Zero)
            var Cr = (2*x/w - 1.5)
            var Ci = (2*y/h - 1.0)
            
            var i = 0
            while i < Iter && (Tr+Ti <= Limit*Limit) {
                Zi = 2*Zr*Zi + Ci
                Zr = Tr - Ti + Cr
                Tr = Zr * Zr
                Ti = Zi * Zi
                i = i + 1
            }
            
            byteAcc <<= 1
            if Tr+Ti <= Limit*Limit {
                byteAcc |= 0x01
            }
            bitNum = bitNum + 1
            
            if bitNum == 8 {
                line.buffer[lpos] = byteAcc
                lpos = lpos + 1
                byteAcc = 0
                bitNum = 0
            } else if x == w - 1.0 {
                if n % 8 > 0 {
                    byteAcc = byteAcc << UInt8(8 - (n%8))
                }
                line.buffer[lpos] = byteAcc
                lpos = lpos + 1
                byteAcc = 0
                bitNum = 0
            }
        }
        let data = dispatch_data_create(&line.buffer, line.size, nil, nil)
        dispatch_async(queue) {

            // Check if row is next in line, if not store for later use
            if line.row == nextRow {
                nextRow = nextRow + 1
		writeLine(stdout, data, wqueue, line.row == n-1)
            } else {
                lines[line.row] = data
            }

            var written = [Int]()
            for waitingRow in lines.keys.sort() {
                if waitingRow == nextRow {
                    nextRow = nextRow + 1
		    writeLine(stdout, lines[waitingRow]!, wqueue, waitingRow == n-1)
                    written.append(waitingRow)
                } else {
                    break
                }
            }
            for l in written {
                lines.removeValueForKey(l)
            }
            written.removeAll()
        }
    }
}

// Wait till all tasks has finished
dispatch_group_wait(cgroup, DISPATCH_TIME_FOREVER)
dispatch_group_wait(group, DISPATCH_TIME_FOREVER)
