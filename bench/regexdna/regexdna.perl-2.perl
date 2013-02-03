# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org/
# contributed by Danny Sauer
# completely rewritten and
# cleaned up for speed and fun by Mirco Wahab
# improved STDIN read, regex clean up by Jake Berner
# More speed and multithreading by Andrew Rodland

use strict;
use warnings;

my $l_file  = -s STDIN;
my $content; read STDIN, $content, $l_file;
# this is significantly faster than using <> in this case

$content =~ s/^>.*//mg;
$content =~ tr/\n//d;
my $l_code  =  length $content;

my @seq = ( 'agggtaaa|tttaccct',
        '[cgt]gggtaaa|tttaccc[acg]',
        'a[act]ggtaaa|tttacc[agt]t',
        'ag[act]gtaaa|tttac[agt]ct',
        'agg[act]taaa|ttta[agt]cct',
        'aggg[acg]aaa|ttt[cgt]ccct',
        'agggt[cgt]aa|tt[acg]accct',
        'agggta[cgt]a|t[acg]taccct',
        'agggtaa[cgt]|[acg]ttaccct' );

my @procs;
for my $s (@seq) {
  my $pat = qr/$s/;
  my $pid = open my $fh, '-|';
  defined $pid or die "Error creating process";
  unless ($pid) {
    my $cnt = 0;
    ++$cnt while $content =~ /$pat/gi;
    print "$s $cnt\n";
    exit 0;
  }
  push @procs, $fh;
}

for my $proc (@procs) {
  print <$proc>;
  close $proc;
}

my %iub = (         B => '(c|g|t)',  D => '(a|g|t)',
  H => '(a|c|t)',   K => '(g|t)',    M => '(a|c)',
  N => '(a|c|g|t)', R => '(a|g)',    S => '(c|g)',
  V => '(a|c|g)',   W => '(a|t)',    Y => '(c|t)' );

# We could cheat here by using $& in the subst and doing it inside a string
# eval to "hide" the fact that we're using $& from the rest of the code... but
# it's only worth 0.4 seconds on my machine.
my $findiub = '(['.(join '', keys %iub).'])';

$content =~ s/$findiub/$iub{$1}/g;

printf "\n%d\n%d\n%d\n", $l_file, $l_code, length $content;
