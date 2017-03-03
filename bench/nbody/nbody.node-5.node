/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by saito tanaka */

var sqrt = Math.sqrt ;

var PI = 3.141592653589793
var SOLAR_MASS = 4 * PI * PI
var DAYS_PER_YEAR = 365.24
var bodies = [
  [ // Sun
    0,// x
    0,// y
    0,// z
    0,// vx
    0,// vy
    0,// vz
    SOLAR_MASS
  ],
  [ // Jupiter
    4.84143144246472090e+00,
    -1.16032004402742839e+00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03 * DAYS_PER_YEAR,
    7.69901118419740425e-03 * DAYS_PER_YEAR,
    -6.90460016972063023e-05 * DAYS_PER_YEAR,
    9.54791938424326609e-04 * SOLAR_MASS
  ],
  [ // Saturn
    8.34336671824457987e+00,
    4.12479856412430479e+00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03 * DAYS_PER_YEAR,
    4.99852801234917238e-03 * DAYS_PER_YEAR,
    2.30417297573763929e-05 * DAYS_PER_YEAR,
    2.85885980666130812e-04 * SOLAR_MASS
  ],
  [// Uranus
    1.28943695621391310e+01,
    -1.51111514016986312e+01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03 * DAYS_PER_YEAR,
    2.37847173959480950e-03 * DAYS_PER_YEAR,
    -2.96589568540237556e-05 * DAYS_PER_YEAR,
    4.36624404335156298e-05 * SOLAR_MASS
  ],
  [ // Neptune
    1.53796971148509165e+01,
    -2.59193146099879641e+01,
    1.79258772950371181e-01,
    2.68067772490389322e-03 * DAYS_PER_YEAR,
    1.62824170038242295e-03 * DAYS_PER_YEAR,
    -9.51592254519715870e-05 * DAYS_PER_YEAR,
    5.15138902046611451e-05 * SOLAR_MASS
  ]
]

function advance(bodies, nbody, dt){
  for (var i=0;i<nbody;i++) {
    var bi  = bodies[i]
    var bix = bi[0],
        biy = bi[1], 
        biz = bi[2], 
        bimass = bi[6];

    var bivx = bi[3],
        bivy = bi[4], 
        bivz = bi[5];

    for (var j=i+1;j<nbody;j++) {
      var bj = bodies[j];
      var dx = bix-bj[0],
          dy = biy-bj[1], 
          dz = biz-bj[2];
      var distance = sqrt(dx*dx + dy*dy + dz*dz)
      var mag = dt / (distance * distance * distance)
      var bim = bimass*mag,
          bjm = bj[6]*mag;
      bivx = bivx - (dx * bjm)
      bivy = bivy - (dy * bjm)
      bivz = bivz - (dz * bjm)
      bj[3] = bj[3] + (dx * bim)
      bj[4] = bj[4] + (dy * bim)
      bj[5] = bj[5] + (dz * bim)
    }
    bi[3] = bivx
    bi[4] = bivy
    bi[5] = bivz
  
    bi[0] = bi[0] + (dt * bi[3])
    bi[1] = bi[1] + (dt * bi[4])
    bi[2] = bi[2] + (dt * bi[5])
  }
}

function energy(bodies, nbody){
  var e = 0;
  for (var i=0;i<nbody;i++) {
    var bi = bodies[i];
    var vx=bi[3],
        vy=bi[4], 
        vz=bi[5], 
        bim = bi[6];
    e = e + (0.5 * bim * (vx*vx + vy*vy + vz*vz));
    for (var j=i+1;j<nbody;j++) {
      var bj = bodies[j];
      var dx= bi[0]-bj[0],
          dy=bi[1]-bj[1],
          dz=bi[2]-bj[2] ;
      var distance = sqrt(dx*dx + dy*dy + dz*dz);
      e = e - ((bim * bj[6]) / distance)
    }
  }
  return e
}

function offsetMomentum(b, nbody){
  var px=0, py=0, pz = 0;
  for (var i=0;i<nbody;i++){
    var bi = b[i];
    var bim = bi[6] ;
    px = px + (bi[3] * bim)
    py = py + (bi[4] * bim)
    pz = pz + (bi[5] * bim)
  }
  b[0][3] = -px / SOLAR_MASS
  b[0][4] = -py / SOLAR_MASS
  b[0][5] = -pz / SOLAR_MASS
}

var n = +process.argv[2] ;
var nbody = bodies.length;

offsetMomentum(bodies, nbody);
console.log(energy(bodies, nbody).toFixed(9));
for (var i=0; i<n; i++){ advance(bodies, nbody, 0.01); }
console.log(energy(bodies, nbody).toFixed(9));
