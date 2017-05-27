//
// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Contributed by Alex Drozhak
//

import Foundation

let SolarMass = 4 * Double.pi * Double.pi
let DaysPerYear = 365.24
let NumberOfBodies = 5

struct Vector3 {
    var x: Double
    var y: Double
    var z: Double
}

extension Vector3 {
    init() {
        self.init(x: 0, y: 0, z: 0)
    }
    
    @_transparent
    func length() -> Double {
        return sqrt(lengthSquared())
    }
    
    @_transparent
    func lengthSquared() -> Double {
        return x * x + y * y + z * z
    }
    
    // Operators
    @_transparent
    static func + (lhs: Vector3, rhs: Vector3) -> Vector3 {
        return Vector3(x: lhs.x + rhs.x, y: lhs.y + rhs.y, z: lhs.z + rhs.z)
    }
    
    @_transparent
    static func - (lhs: Vector3, rhs: Vector3) -> Vector3 {
        return Vector3(x: lhs.x - rhs.x, y: lhs.y - rhs.y, z: lhs.z - rhs.z)
    }
    
    @_transparent
    static func * (lhs: Vector3, rhs: Vector3) -> Vector3 {
        return Vector3(x: lhs.x * rhs.x, y: lhs.y * rhs.y, z: lhs.z * rhs.z)
    }
    
    @_transparent
    static func * (lhs: Vector3, rhs: Double) -> Vector3 {
        return Vector3(x: lhs.x * rhs, y: lhs.y * rhs, z: lhs.z * rhs)
    }
}

struct Body {
    var position: Vector3
    var velocity: Vector3
    var mass: Double
}

extension Body {
    
    //Custom init for sun
    init(mass: Double) {
        self.position = Vector3()
        self.velocity = Vector3()
        self.mass = mass
    }
    
    static let sun = Body(mass: SolarMass)
    
    static let jupiter = Body(
        position: Vector3(x: 4.84143144246472090e+00,
                          y: -1.16032004402742839e+00,
                          z: -1.03622044471123109e-01),
        velocity: Vector3(x: 1.66007664274403694e-03 * DaysPerYear,
                          y: 7.69901118419740425e-03 * DaysPerYear,
                          z: -6.90460016972063023e-05 * DaysPerYear),
        mass: 9.54791938424326609e-04 * SolarMass
    )
    
    static let saturn = Body(
        position: Vector3(x: 8.34336671824457987e+00,
                          y: 4.12479856412430479e+00,
                          z: -4.03523417114321381e-01),
        velocity: Vector3(x: -2.76742510726862411e-03 * DaysPerYear,
                          y: 4.99852801234917238e-03 * DaysPerYear,
                          z: 2.30417297573763929e-05 * DaysPerYear),
        mass: 2.85885980666130812e-04 * SolarMass
    )
    
    static let uranus = Body(
        position: Vector3(x: 1.28943695621391310e+01,
                          y: -1.51111514016986312e+01,
                          z: -2.23307578892655734e-01),
        velocity: Vector3(x: 2.96460137564761618e-03 * DaysPerYear,
                          y: 2.37847173959480950e-03 * DaysPerYear,
                          z: -2.96589568540237556e-05 * DaysPerYear),
        mass: 4.36624404335156298e-05 * SolarMass
    )
    
    static let neptune = Body(
        position: Vector3(x: 1.53796971148509165e+01,
                          y: -2.59193146099879641e+01,
                          z: 1.79258772950371181e-01),
        velocity: Vector3(x: 2.68067772490389322e-03 * DaysPerYear,
                          y: 1.62824170038242295e-03 * DaysPerYear,
                          z: -9.51592254519715870e-05 * DaysPerYear),
        mass: 5.15138902046611451e-05 * SolarMass
    )
}

struct NBodySystem {
    
    static var bodies = [
        Body.sun,
        Body.jupiter,
        Body.saturn,
        Body.uranus,
        Body.neptune
    ]
    
    let ptr = UnsafeMutablePointer(mutating: &NBodySystem.bodies)
    
    @inline(__always)
    func offsetMomentum() {
        let p = NBodySystem.bodies.reduce(Vector3()) { (v, b) in v + b.velocity * b.mass }
        self.ptr[0].velocity = p * (-1.0 / SolarMass)
    }

    var energy: Double {
        var e = 0.0
        var iterator = NBodySystem.bodies.makeIterator()
        while let bi = iterator.next() {
            e += bi.velocity.lengthSquared() * bi.mass / 2.0 - bi.mass * iterator.map { $0.mass / (bi.position - $0.position).length() }.reduce(0.0, +)
        }
        return e
    }

    @inline(__always)
    func advance(_ dt: Double) {
        for i in 0..<NumberOfBodies {
            for j in (i + 1)..<NumberOfBodies {
                
                let dp = ptr[i].position - ptr[j].position
                
                let dist = dp.length()
                let mag = dt / (dist * dist * dist)
                
                ptr[i].velocity = ptr[i].velocity - dp * ptr[j].mass * mag
                ptr[j].velocity = ptr[j].velocity + dp * ptr[i].mass * mag
            }
            ptr[i].position = ptr[i].position + ptr[i].velocity * dt
        }
    }
}

let n: Int = Int(CommandLine.arguments[1]) ?? 1000

let system = NBodySystem()
system.offsetMomentum()
print(system.energy)
for _ in 1...n {
    system.advance(0.01)
}
print(system.energy)
