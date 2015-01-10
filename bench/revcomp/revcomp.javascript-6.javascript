/* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/

    contributed by Adriaan de Haan
*/

// create a mapping between the translations.
var map = [];
map['A'] = 'T';
map['a'] = 'T';
map['C'] = 'G';
map['c'] = 'G';
map['G'] = 'C';
map['g'] = 'C';
map['T'] = 'A';
map['t'] = 'A';
map['U'] = 'A';
map['u'] = 'A';
map['M'] = 'K';
map['m'] = 'K';
map['R'] = 'Y';
map['r'] = 'Y';
map['W'] = 'W';
map['w'] = 'W';
map['S'] = 'S';
map['s'] = 'S';
map['Y'] = 'R';
map['y'] = 'R';
map['K'] = 'M';
map['k'] = 'M';
map['V'] = 'B';
map['v'] = 'B';
map['H'] = 'D';
map['h'] = 'D';
map['D'] = 'H';
map['d'] = 'H';
map['B'] = 'V';
map['b'] = 'V';
map['N'] = 'N';
map['n'] = 'N';

var line = readline();
while (true) 
{
   var seq = [];
    while (line != undefined && line[0] != '>') 
   {
      seq += line;
      line = readline();         
    }
   var cnt = 0;
   for (x = seq.length; x > 0; --x)
   {
       ++cnt;
      if (cnt == 60)
      {
         cnt = 0;
         print(map[seq.charAt(x-1)]);
      }
      else
         write(map[seq.charAt(x-1)]);
   }
   if (cnt > 0)
      print("");
   if (line == undefined)
      quit();
   
   print(line);
   line = readline();
}
