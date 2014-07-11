/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Olof Kraigher
*/

extern crate sync;
use sync::Arc;

fn main () {
    match read_args() {
        Ok(num_solutions) => {
            solve(num_solutions);
        }
        Err(message) => {
            println!("{}", message);
            std::os::set_exit_status(1);
        }
    }
}

fn solve(num_solutions : uint) {
    let (min, max, num_found) = read_solutions(spawn_solvers(), num_solutions);
    println!("{} solutions found\n", num_found);
    min.pretty_print();
    max.pretty_print();
}

fn read_args() -> Result<uint, String> {
    let args = std::os::args();
    if args.len() != 2 {
        return Err(format!(
            "Usage: {} num_solutions", 
            args.get(0)));
    }

    let maybe_int : Option<uint> = from_str(args.get(1).as_slice());
    if maybe_int.is_none() {
        return Err(format!(
            "Invalid argument '{}' cannot parse as unsigned integer", 
            args.get(1)));
    }

    let num_solutions = maybe_int.unwrap();
    if num_solutions == 0 {
        return Err(format!(
            "Invalid argument '{}' must be greather than 0", 
            args.get(1)));
    }

    return Ok(num_solutions);
}

fn read_solutions(solution_receiver : Receiver<Solution>,
                  num_solutions : uint)  -> (Solution, Solution, uint) {
    let first = solution_receiver.recv();
    let mut num_found = 1;
    let mut min = first.clone();
    let mut max = first;
    for solution in solution_receiver.iter() {
        if num_found == num_solutions {
            break;
        }
        if solution < min {
            min = solution;
        } else if solution > max {
            max = solution;            
        }
        num_found += 1;
    }

    (min, max, num_found)
}

fn spawn_solvers() -> Receiver<Solution> {
    let mask_lookup = Arc::new(MaskLookup::new());

    let (solution_sender, solution_receiver) = channel();

    for first_piece in range(0, num_pieces) {
        let num_masks = mask_lookup.get(first_piece, last_position).len();

        for mask_idx in range(0, num_masks) {
            let my_solution_sender = solution_sender.clone();
            let my_mask_lookup = mask_lookup.clone();

            spawn(proc() {
                let mut solver = Solver::new(
                    my_mask_lookup.deref(), 
                    &my_solution_sender);
                solver.place_piece(first_piece, 
                                   last_position, 
                                   mask_idx, 
                                   num_masks);
            });
        }
    }
    solution_receiver
}

struct Solver<'a> {
    mask_lookup: &'a MaskLookup,
    masks: [u64, ..num_pieces],
    mask : u64,
    used_pieces: uint,
    solution_sender : &'a Sender<Solution>,
    solution: Solution,
    reversed_solution: Solution
}

impl<'a> Solver<'a> {
    fn new(mask_lookup : &'a MaskLookup, 
           solution_sender : &'a Sender<Solution>) -> Solver<'a> {
        Solver {
            mask_lookup: mask_lookup,
            masks: [0, ..num_pieces],
            mask: 0,
            used_pieces: 0,
            solution_sender: solution_sender,
            solution: Solution::default(),
            reversed_solution: Solution::default()
        }        
    }

    fn place_piece(&mut self, 
                   piece : uint,
                   position : uint,
                   start : uint,
                   step : uint) {
        self.toggle_piece(piece);
        let masks = self.mask_lookup.get(piece, position);
        let mut idx = start;
        while idx < masks.len() {
            self.evaluate(piece, masks[idx]);
            idx += step;
        }
        self.toggle_piece(piece);
    }

    fn evaluate(&mut self, piece : uint, mask : u64) {
        if self.fits(mask) {
            self.place_mask(piece, mask);
            if self.done() {
                self.send_solution();
            } else if self.still_possible() {
                self.choose_piece();
            }
            self.unplace_mask(mask);
        }
    } 

    fn choose_piece(&mut self) {
        let position = self.first_free_position();
        let mut piece = 0;
        while piece < num_pieces {
            if self.is_not_placed(piece) {
                self.place_piece(piece, position, 0, 1);
            }
            piece += 1;
        }
    }

    fn toggle_piece(&mut self, piece : uint) {
        self.used_pieces ^= 1 << piece;
    }

    fn place_mask(&mut self, piece: uint, mask : u64) {
        self.mask ^= mask;
        self.masks[piece] = mask;
    }

    fn unplace_mask(&mut self, mask : u64) {
        self.mask ^= mask;
    }

    fn done(&self) -> bool {
        self.used_pieces == (1 << num_pieces) - 1
    }
    
    fn still_possible(&self) -> bool {
        no_islands(self.mask)
    }

    fn first_free_position(&self) -> uint {
        find_first_one(!self.mask & full_mask)
    }

    fn is_not_placed(&self, piece : uint) -> bool {
        self.used_pieces & (1 << piece) == 0
    }    

    fn fits(&self, mask : u64) -> bool {
        self.mask & mask == 0
    }

    fn send_solution(&mut self) { 
        self.fill_solutions();
        let _ = self.solution_sender.send_opt(self.solution);
        let _ = self.solution_sender.send_opt(self.reversed_solution);
    }

    fn fill_solutions(&mut self) {
        for position in range(0, num_positions) {
            let piece = self.piece_at_position(position);          
            self.solution.pieces[position] = piece;
            let reversed_position = last_position - position;
            self.reversed_solution.pieces[reversed_position] = piece;
        }
    }

    fn piece_at_position(&self, position : uint) -> u8 {
        let position_mask = 1 << position;
        for piece in range(0, num_pieces) {
            let mask = self.masks[piece];
            let uses_piece = (self.used_pieces >> piece) & 1 == 1;
            let occupies_position = overlaps(mask, position_mask);
            if uses_piece && occupies_position {
                return piece as u8;
            }
        }
        return 0;
    }
}

struct Solution {
    pieces: [u8, ..num_positions]
}

impl Solution {
    fn default() -> Solution {
        Solution {
            pieces: [0, ..num_positions]
        }
    }
}

impl Clone for Solution {
    fn clone(&self) -> Solution {
        Solution {
            pieces: self.pieces
        }
    }
}

impl PartialOrd for Solution {
    fn lt(&self, other : &Solution) -> bool {
        self.pieces < other.pieces
    }
}

impl PartialEq for Solution {
    fn eq(&self, other : &Solution) -> bool {
        self.pieces == other.pieces
    }
}

impl Solution {
    fn pretty_print(&self) {
        for (idx, &piece) in self.pieces.iter().enumerate() {
            let glyph = (('0' as u8) + piece) as char;
            print!("{}", glyph);
            print!(" ");

            let x = idx % width;
            let y = idx / width;

            if x == width-1 {
                if y%2 == 0 {
                    print!("\n ");
                } else {
                    print!("\n");
                }
            }
        }
        print!("\n");
    }
}

struct MaskLookup {
    masks_by_piece_and_position: Vec<Vec<u64>>
}

impl MaskLookup {
    fn new() -> MaskLookup {
        let mut ml = MaskLookup::default();
        ml.add_piece(false, 0, [E, E, E, SE]);
        ml.add_piece(false, 1, [SE, SW, W, SW]);
        ml.add_piece(false, 2, [W, W, SW, SE]);
        ml.add_piece(true,  3, [E, E, SW, SE]);
        ml.add_piece(false, 4, [NW, W, NW, SE, SW]);
        ml.add_piece(false, 5, [E, E,  NE, W]);
        ml.add_piece(false, 6, [NW, NE, NE, W]);
        ml.add_piece(false, 7, [NE, SE, E, NE]);
        ml.add_piece(false, 8, [SE, SE, E, SE]);
        ml.add_piece(false, 9, [E, NW, NW, NW]);
        ml
    }

    fn default() -> MaskLookup {
        MaskLookup {
            masks_by_piece_and_position: 
                Vec::from_fn(num_pieces * num_positions, 
                             |_| Vec::with_capacity(2*6))
        }
    }

    fn add_piece(&mut self, 
                 fully_rotated : bool,
                 index : uint, 
                 directions : &[Direction]) {
        let mut piece : Piece = Piece::new(directions);
        let num_orientations : uint = 2;
        let num_rotations : uint = if fully_rotated {3} else {6};

        for _ in range(0, num_orientations) {
            for _ in range(0, num_rotations) {
                for x in range(0, width as int) {
                    for y in range(0, height as int) {
                        let position = Position::new(x,y);
                        self.add_piece_at_position(index,
                                                   &piece,
                                                   position);
                    }
                }
                piece = piece.rotate();
            }
            piece = piece.flip();
        }
    }

    fn add_piece_at_position(&mut self, 
                             index : uint,
                             piece : &Piece, 
                             position : Position) {

        match piece.to_mask(position) {
            Some(mask) => {
                let last = find_first_one(mask);
                let idx = index*num_positions + last;
                self.masks_by_piece_and_position.get_mut(idx).push(mask);
            }
            None => ()
        }
    }

    fn get<'a>(&'a self, index : uint, position : uint) -> &'a [u64] {
        let idx = index*num_positions + position;
        self.masks_by_piece_and_position.get(idx).as_slice()
    }
}

#[deriving(Clone)]
struct Piece {
    directions: Vec<Direction>
}

impl Piece {
    fn new(directions : &[Direction]) -> Piece {
        Piece {
            directions: Vec::from_fn(directions.len(), |i|directions[i])
        }
    }

    fn to_mask(&self, position : Position) -> Option<u64> {
        let mut mask = position.to_mask();
        let mut current_position = position;

        for direction in self.directions.iter() {
            match current_position.in_direction(direction) {
                Some(position) => {
                    current_position = position;
                    mask |= current_position.to_mask();
                },
                None => return None
            }
        }
        return Piece::prune(mask);
    }

    fn prune(mask : u64) -> Option<u64> {
        let border = 0b11111_10001_10001_10001_10001_10001_10001_10001_10001_11111;
        if mask & border == 0 || no_islands(mask) {
            Some(mask)
        } else {
            None
        }
    }

    fn flip(&self) -> Piece {
        self.as_modified(|x| x.flip())
    }

    fn rotate(&self) -> Piece {
        self.as_modified(|x| x.rotate())
    }

    fn as_modified(&self, fun : |&Direction| -> Direction) -> Piece {
        Piece {
            directions: self.directions.iter().map(fun).collect()
        }
    }
}

struct Position {
    x: int,
    y: int
}

impl Position {
    fn new(x : int, y : int) -> Position {
        Position {x:x, y:y}
    }

    fn in_direction(&self, direction : &Direction) -> Option<Position> {

        let (dx, dy) =
            match direction {
                &E => (-1, 0),
                &W => ( 1, 0),
                &NE => (self.y%2 - 1,  1),
                &NW => (self.y%2    ,  1),
                &SE => (self.y%2 - 1, -1),
                &SW => (self.y%2    , -1)
            };

        let new_position = self.in_2d_direction(dx, dy);

        if Position::is_valid(new_position) {
            Some(new_position)
        } else {
            None
        }
    }

    fn in_2d_direction(&self, dx : int, dy : int) -> Position {
        Position {
            x: self.x + dx,
            y: self.y + dy
        }                  
    }

    fn is_valid(Position{x,y} : Position) -> bool {
        0 <= x && x < width as int && 0 <= y && y < height as int
    }

    fn to_mask(&self) -> u64 {
        1u64 << (self.y * width as int + self.x) as uint
    }
}

#[deriving(Clone, FromPrimitive)]
enum Direction {
    E=0, SE=1, SW=2, W=3, NW=4, NE=5
}

impl Direction {
    fn rotate(&self) -> Direction {
        self.as_modified(|x| (x + 1)%6)
    }

    fn flip(&self) -> Direction {
        self.as_modified(|x| (9 - x)%6)
    }

    fn as_modified(&self, modifier : |int| -> int) -> Direction {
        FromPrimitive::from_int(modifier(*self as int)).unwrap()
    }
}

fn no_islands(mask : u64) -> bool {
    let allowed = !mask & full_mask;
    let seed = (1 << mask.trailing_zeros() as uint) - 1;
    let filled = flood_fill(seed, allowed);
    filled.count_ones() % 5 == 0
}

fn flood_fill(seed : u64, allowed : u64) -> u64 {
    let mut filled = seed;

    loop {
        let new_filled = grow(filled) & allowed;
        if new_filled == filled {
            return filled;
        }        
        filled = new_filled;
    }
}

fn find_first_one(mask : u64) -> uint {
    63 - mask.leading_zeros() as uint
}

fn overlaps(m1 : u64, m2 : u64) -> bool {
    return m1 & m2 != 0u64;
}

fn grow(mask : u64) -> u64 {
    let even = 0b00000_11111_00000_11111_00000_11111_00000_11111_00000_11111;
    let odd = 0b11111_00000_11111_00000_11111_00000_11111_00000_11111_00000;
    let right = 0b00001_00001_00001_00001_00001_00001_00001_00001_00001_00001;
    let left = 0b10000_10000_10000_10000_10000_10000_10000_10000_10000_10000;

    let not_right = mask & !right;
    let not_left = mask & !left;
    let east = not_right>>1;
    let west = not_left<<1;
    let body = mask | (east & (even>>1)) | (west & (odd<<1));

    mask | west | (body << width) | east | (body >> width)
}


static num_pieces : uint = 10;
static width : uint = 5;
static height : uint = 10;
static num_positions : uint = width*height;
static last_position : uint = num_positions-1;
static full_mask : u64 = (1 << num_positions) - 1;
