/**
 The Computer Language Benchmarks Game
 http://benchmarksgame.alioth.debian.org/
 contributed by Robert F. Dickerson
*/

import CGMP

public final class BigInt {

    private var i = mpz_t()

    public init(_ value: Int = 0) {
        __gmpz_init(&i)
        __gmpz_set_si(&i, value )
    }

    deinit {
        __gmpz_clear(&i)
    }

    public var intValue: Int {
        let ret = __gmpz_get_si(&self.i)
        return ret
    }

}

extension BigInt: Equatable {}
extension BigInt: Comparable {}

public func * (lhs: BigInt, rhs: BigInt) -> BigInt {
    let ret = BigInt()
    __gmpz_mul(&ret.i, &lhs.i, &rhs.i)
    return ret
}

public func * (lhs: Int, rhs: BigInt) -> BigInt {
    let ret = BigInt()
    let tmp = BigInt(lhs)
    __gmpz_mul(&ret.i, &tmp.i, &rhs.i)
    return ret
}

public func + (lhs: BigInt, rhs: BigInt) -> BigInt {
    let ret = BigInt()
    __gmpz_add(&ret.i, &lhs.i, &rhs.i)
    return ret
}

public func += (inout lhs: BigInt, rhs: BigInt) {
    __gmpz_add(&lhs.i, &lhs.i, &rhs.i)
}

public func += (inout lhs: BigInt, rhs: Int) {
    let tmp = BigInt(rhs)
    __gmpz_add(&lhs.i, &lhs.i, &tmp.i)
}

public func *= (inout lhs: BigInt, rhs: BigInt) {
    __gmpz_mul(&lhs.i, &lhs.i, &rhs.i)
}

public func == (lhs: BigInt, rhs: BigInt) -> Bool {
    return (0 == __gmpz_cmp(&lhs.i, &rhs.i))
}

public func != (lhs: BigInt, rhs: BigInt) -> Bool {
    return !(lhs == rhs)
}

public func / (lhs: BigInt, rhs: BigInt) -> BigInt {
    let ret = BigInt(0)
    __gmpz_fdiv_q(&ret.i,&lhs.i,&rhs.i)
    return ret
}

public func /= (lhs: BigInt, rhs: BigInt) {
    __gmpz_fdiv_q(&lhs.i, &lhs.i, &rhs.i)
}

public func <(lhs: BigInt, rhs: BigInt) -> Bool {
    return (__gmpz_cmp(&lhs.i, &rhs.i) < 0)
}

typealias Matrix = (BigInt, BigInt, BigInt, BigInt)

let unit = (BigInt(1), BigInt(0),
            BigInt(0), BigInt(1))

func * (lhs: Matrix, rhs: Matrix) -> Matrix {
    return ((lhs.0*rhs.0)+(lhs.1*rhs.2),
            (lhs.0*rhs.1)+(lhs.1*rhs.3),
            (lhs.2*rhs.0)+(lhs.3*rhs.2),
            (lhs.2*rhs.1)+(lhs.3*rhs.3))
}

func generate( k: Int) -> Matrix {
    return (BigInt(k), BigInt(4)*BigInt(k)+BigInt(2),
            BigInt(0), BigInt(2)*BigInt(k)+BigInt(1))
}

func extr(m: Matrix, x: BigInt ) -> BigInt {
    let a = (m.0 * x) + m.1
    let b = (m.2 * x) + m.3
    return a/b
}

func safe(z: Matrix, n: BigInt) -> Bool {
    return n == extr(z, x: BigInt(4))
}

func prod(z: Matrix, n: BigInt) -> Matrix {
    return (BigInt(10), BigInt(-10)*n,
            BigInt(0), BigInt(1)) * z
}

func next (z: Matrix) -> BigInt {
    return extr(z, x: BigInt(3))
}

func computePi(withDigits digits: Int) {

    var z = unit
    var n = 0
    var k = 1
    var j = 0

    var buffer = [Int](count: 10, repeatedValue: 0 )

    while n < digits {

        let y = next( z )

        if safe(z, n: y) {

            buffer[j] = y.intValue
            j += 1

            z = prod( z, n: y)
            n += 1

            if j > 9 {
                printOutput(buffer, n: n)
                j = 0
            }

        } else {

            let x = generate(k)
            k += 1
            z = z * x

        }

    }

    if n%10 != 0 {
        printOutput(buffer, n: n)
    }
}

func printOutput( buffer: [Int], n: Int) {
    var output = ""
    var count = 0

    for num in buffer {

        if n%10==0 || count < n%10 {
            output += "\(num)"
        } else {
            output += " "
        }

        count += 1
    }

    output += "\t:\(n)"

    print(output)
}

computePi(withDigits: 10000)
