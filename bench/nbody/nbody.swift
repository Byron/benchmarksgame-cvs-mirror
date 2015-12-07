/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Isaac Gouy 

   Swift install check, transliterated from C#
*/

import CoreFoundation


class NBodySystem {
   let bodies: [Body] = [
         Body.Sun(),		
         Body.Jupiter(),
         Body.Saturn(),
         Body.Uranus(),
         Body.Neptune()		
      ]

   var px: Double = 0.0
   var py: Double = 0.0
   var pz: Double = 0.0

   init(){
      for body in bodies {
         px += body.vx * body.mass
         py += body.vy * body.mass		
         pz += body.vz * body.mass	
      }
      bodies[0].offsetMomentum(px,py,pz)
   }

   func advance(dt: Double) {
      var dx, dy, dz, distance, mag: Double	
	
      for var i=0; i < bodies.count; i++ {
         for var j=i+1; j < bodies.count; j++ {	
            dx = bodies[i].x - bodies[j].x
            dy = bodies[i].y - bodies[j].y
            dz = bodies[i].z - bodies[j].z
				
            distance = sqrt(dx*dx + dy*dy + dz*dz)				   
            mag = dt / (distance * distance * distance)
				
            bodies[i].vx -= dx * bodies[j].mass * mag
            bodies[i].vy -= dy * bodies[j].mass * mag
            bodies[i].vz -= dz * bodies[j].mass * mag
                                
            bodies[j].vx += dx * bodies[i].mass * mag
            bodies[j].vy += dy * bodies[i].mass * mag
            bodies[j].vz += dz * bodies[i].mass * mag
         }
      }		
		
      for body in bodies {
         body.x += dt * body.vx
         body.y += dt * body.vy
         body.z += dt * body.vz
      }		
   }	

   func energy() -> Double {		
      var dx, dy, dz, distance: Double	
      var e = 0.0		   
		
      for var i=0; i < bodies.count; i++ {
         e += 0.5 * bodies[i].mass * 
            ( bodies[i].vx * bodies[i].vx 
            + bodies[i].vy * bodies[i].vy 
            + bodies[i].vz * bodies[i].vz )
			   
         for var j=i+1; j < bodies.count; j++ {
            dx = bodies[i].x - bodies[j].x
            dy = bodies[i].y - bodies[j].y
            dz = bodies[i].z - bodies[j].z
                                
            distance = sqrt(dx*dx + dy*dy + dz*dz)
            e -= (bodies[i].mass * bodies[j].mass) / distance
         }
      }
      return e;
   }
}


class Body {
   static let PI = 3.141592653589793
   static let SOLAR_MASS = 4 * PI * PI
   static let DAYS_PER_YEAR = 365.24

   var x: Double = 0.0
   var y: Double = 0.0
   var z: Double = 0.0
   var vx: Double = 0.0
   var vy: Double = 0.0
   var vz: Double = 0.0
   var mass: Double = 0.0

   init(){}

   static func Jupiter() -> Body {
      let p = Body();
      p.x = 4.84143144246472090e+00
      p.y = -1.16032004402742839e+00
      p.z = -1.03622044471123109e-01
      p.vx = 1.66007664274403694e-03 * DAYS_PER_YEAR
      p.vy = 7.69901118419740425e-03 * DAYS_PER_YEAR
      p.vz = -6.90460016972063023e-05 * DAYS_PER_YEAR
      p.mass = 9.54791938424326609e-04 * SOLAR_MASS	   	   	   
      return p
   }

   static func Saturn() -> Body {
      let p = Body();
      p.x = 8.34336671824457987e+00
      p.y = 4.12479856412430479e+00
      p.z = -4.03523417114321381e-01
      p.vx = -2.76742510726862411e-03 * DAYS_PER_YEAR
      p.vy = 4.99852801234917238e-03 * DAYS_PER_YEAR
      p.vz = 2.30417297573763929e-05 * DAYS_PER_YEAR
      p.mass = 2.85885980666130812e-04 * SOLAR_MASS		   	  
      return p
   }

   static func Uranus() -> Body {
      let p = Body();
      p.x = 1.28943695621391310e+01
      p.y = -1.51111514016986312e+01
      p.z = -2.23307578892655734e-01
      p.vx = 2.96460137564761618e-03 * DAYS_PER_YEAR
      p.vy = 2.37847173959480950e-03 * DAYS_PER_YEAR
      p.vz = -2.96589568540237556e-05 * DAYS_PER_YEAR
      p.mass = 4.36624404335156298e-05 * SOLAR_MASS			  
      return p
   }

   static func Neptune() -> Body {
      let p = Body();
      p.x = 1.53796971148509165e+01
      p.y = -2.59193146099879641e+01
      p.z = 1.79258772950371181e-01
      p.vx = 2.68067772490389322e-03 * DAYS_PER_YEAR
      p.vy = 1.62824170038242295e-03 * DAYS_PER_YEAR
      p.vz = -9.51592254519715870e-05 * DAYS_PER_YEAR
      p.mass = 5.15138902046611451e-05 * SOLAR_MASS				  
      return p
   }

   static func Sun() -> Body {
      let p = Body();
      p.mass = SOLAR_MASS				  
      return p
   }

   func offsetMomentum(px: Double, _ py: Double, _ pz: Double) -> Body {
      vx = -px / Body.SOLAR_MASS;
      vy = -py / Body.SOLAR_MASS;
      vz = -pz / Body.SOLAR_MASS;	   
      return self;   
   }	
}


let n: Int? = Int(Process.arguments[1])
let nbodies = NBodySystem()
print( nbodies.energy() )
for var i=0; i<n; i++ {
   nbodies.advance(0.01)
}
print( nbodies.energy() )




