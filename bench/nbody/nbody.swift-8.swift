/* The Computer Language Benchmarks Game
 contributed by Isaac Gouy
 modified by TVSori None
 converted to Swift 3 by Sergo Beruashvili
 optimized by Hannes Sverrisson
 */

import Foundation

struct Body {
    var x, y, z, vx, vy, vz, mass : Double

    @inline(__always)
    public mutating func velSub(dx: Double, dy: Double, dz: Double, mass: Double) {
        self.vx -= dx * mass
        self.vy -= dy * mass
        self.vz -= dz * mass
    }

    @inline(__always)
    public mutating func velAdd(dx: Double, dy: Double, dz: Double, mass: Double) {
        self.vx += dx * mass
        self.vy += dy * mass
        self.vz += dz * mass
    }

    @inline(__always)
    public mutating func distAdd() {
        self.x += self.vx * 0.01
        self.y += self.vy * 0.01
        self.z += self.vz * 0.01
    }
}

let PI: Double = 3.14159265358979
let SOLAR_MASS: Double = 4 * PI * PI
let DAYS_PER_YEAR: Double = 365.24

let bodies: [Body] = [
    Body (
        x: 0.0,
        y: 0.0,
        z: 0.0,
        vx: 0.0,
        vy: 0.0,
        vz: 0.0,
        mass: SOLAR_MASS
    ),
    Body (
        x: 4.84143144246472090e+00,
        y: -1.16032004402742839e+00,
        z: -1.03622044471123109e-01,
        vx: 1.66007664274403694e-03 * DAYS_PER_YEAR,
        vy: 7.69901118419740425e-03 * DAYS_PER_YEAR,
        vz: -6.90460016972063023e-05 * DAYS_PER_YEAR,
        mass: 9.54791938424326609e-04 * SOLAR_MASS
    ),
    Body (
        x: 8.34336671824457987e+00,
        y: 4.12479856412430479e+00,
        z: -4.03523417114321381e-01,
        vx: -2.76742510726862411e-03 * DAYS_PER_YEAR,
        vy: 4.99852801234917238e-03 * DAYS_PER_YEAR,
        vz: 2.30417297573763929e-05 * DAYS_PER_YEAR,
        mass: 2.85885980666130812e-04 * SOLAR_MASS
    ),
    Body (
        x: 1.28943695621391310e+01,
        y: -1.51111514016986312e+01,
        z: -2.23307578892655734e-01,
        vx: 2.96460137564761618e-03 * DAYS_PER_YEAR,
        vy: 2.37847173959480950e-03 * DAYS_PER_YEAR,
        vz: -2.96589568540237556e-05 * DAYS_PER_YEAR,
        mass: 4.36624404335156298e-05 * SOLAR_MASS
    ),
    Body (
        x: 1.53796971148509165e+01,
        y: -2.59193146099879641e+01,
        z: 1.79258772950371181e-01,
        vx: 2.68067772490389322e-03 * DAYS_PER_YEAR,
        vy: 1.62824170038242295e-03 * DAYS_PER_YEAR,
        vz: -9.51592254519715870e-05 * DAYS_PER_YEAR,
        mass: 5.15138902046611451e-05 * SOLAR_MASS
    )
]

func offsetMomentum() {
    var px: Double = 0.0
    var py: Double = 0.0
    var pz: Double = 0.0

    for body in bodies {
        px += body.vx * body.mass
        py += body.vy * body.mass
        pz += body.vz * body.mass
    }
    let bodiesPtr = UnsafeMutablePointer<Body>(mutating: bodies)
    bodiesPtr[0].vx = -px / SOLAR_MASS
    bodiesPtr[0].vy = -py / SOLAR_MASS
    bodiesPtr[0].vz = -pz / SOLAR_MASS
}

func energy() -> Double {
    var dx, dy, dz, distance: Double
    var e: Double = 0.0
    for i in 0..<5 {
        let body = bodies[i]
        e += 0.5 * body.mass *
            ( body.vx * body.vx
                + body.vy * body.vy
                + body.vz * body.vz )

        for j in i+1..<5 {
            let jbody = bodies[j]
            dx = body.x - jbody.x
            dy = body.y - jbody.y
            dz = body.z - jbody.z

            distance = sqrt(dx*dx + dy*dy + dz*dz)
            e -= (body.mass * jbody.mass) / distance
        }
    }
    return e
}

func advance(n: Int) {
    for _ in 1...n {
        var dx, dy, dz, distance, mag: Double
        let arrPtr = UnsafeMutablePointer<Body>(mutating: bodies)
        for i in 0..<5 {
            let body = bodies[i]

            for j in (i+1)..<5 {
                let jbody = bodies[j]

                dx = body.x - jbody.x
                dy = body.y - jbody.y
                dz = body.z - jbody.z

                distance = dx * dx + dy * dy + dz * dz
                mag = 0.01 / (distance * sqrt(distance))

                arrPtr[i].velSub(dx: dx, dy: dy, dz: dz, mass: jbody.mass * mag)
                arrPtr[j].velAdd(dx: dx, dy: dy, dz: dz, mass: body.mass * mag)
            }
            arrPtr[i].distAdd()
        }
    }
}

let n: Int = Int(CommandLine.arguments[1]) ?? 50_000_000

offsetMomentum()
print(String(format: "%0.9f", Double(energy())))
advance(n: n)
print(String(format: "%0.9f", Double(energy())))