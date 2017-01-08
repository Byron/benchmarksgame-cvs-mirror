<?php
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed  by Thomas Flori
 */

while (ob_get_level()) ob_end_clean();
ini_set('memory_limit', '512M');

define('LINE_LENGTH', 60);
define('NUM_THREADS', 16);
define('BUFFER_LINE_COUNT', 1024);
define('BUFFER_SIZE', LINE_LENGTH * BUFFER_LINE_COUNT);

define('IA', 3877);
define('IC', 29573);
define('IM', 139968);

$workers = [];

$n = 1000;

if ($_SERVER['argc'] > 1) {
    $n = (int)$_SERVER['argv'][1];
}

echo ">ONE Homo sapiens alu\n";
$alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
     . "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
     . "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
     . "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
     . "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
     . "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
     . "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
$length = $n * 2;
$buffer = strlen($alu) * 60;
while ($length > 0) {
    $c = $length > $buffer ? $buffer : $length;
    echo chunk_split(str_pad('', $c, $alu), LINE_LENGTH, "\n"); // yes a one liner
    $length -= $c;
}


echo ">TWO IUB ambiguity codes\n";
$iub = [
    'a' => 0.27, 'c' => 0.12, 'g' => 0.12, 't' => 0.27, 'B' => 0.02, 'D' => 0.02, 'H' => 0.02, 'K' => 0.02,
    'M' => 0.02, 'N' => 0.02, 'R' => 0.02, 'S' => 0.02, 'V' => 0.02, 'W' => 0.02, 'Y' => 0.02
];
accumulatePropabilities($iub);
writeDNA(function ($randoms) use ($iub) {
    return getDNA($randoms, $iub);
}, $n * 3);

echo ">THREE Homo sapiens frequency\n";
$homosapiens = [
    'a' => 0.3029549426680, 'c' => 0.1979883004921,
    'g' => 0.1975473066391, 't' => 0.3015094502008
];
accumulatePropabilities($homosapiens);
writeDNA(function ($randoms) use ($homosapiens) {
    return getDNA($randoms, $homosapiens);
}, $n * 5);

function writeDNA($callback, $length) {
    static $seed = 42;
    $threads = [];
    $i = 0;
    $j = 0;
    // here we need a lot of memory how to avoid this?
    $dna = shmop_open(ftok(__FILE__, chr(0)), 'c', 0644, $length);
    while ($length > 0) {
        $c = BUFFER_SIZE;
        if ($c + $i > $length) {
            $c = $length - $i;
        }
        if (count($threads) === NUM_THREADS) {
            $pid = pcntl_wait($status);
            array_splice($threads, array_search($pid, $threads), 1);
        }
        $randoms = generateRandoms($seed, $c);
        if (!$pid = pcntl_fork()) {
            $offset = BUFFER_SIZE * $j;
            shmop_write($dna, $callback($randoms), $offset);
            exit();
        } else {
            $threads[] = $pid;
        }
        $length -= $c;
        $j++;
    }
    while (count($threads)) {
        $pid = pcntl_wait($status);
        array_splice($threads, array_search($pid, $threads), 1);
    }
    echo chunk_split(shmop_read($dna, 0, $length), LINE_LENGTH, "\n");
    shmop_delete($dna);
}

function generateRandoms(&$seed, $count) {
    $randoms = array_fill(0, $count, 0);
    foreach ($randoms as &$r) {
        $r = $seed = ($seed * IA + IC) % IM;
    }
    return $randoms;
}

function getDNA($randoms, $genelist) {
    $dna = '';
    foreach ($randoms as $r) {
        foreach ($genelist as $nucleoid => $v) {
            if ($r < $v) {
                $dna .= $nucleoid;
                break;
            }
        }
    }
    return $dna;
}

function accumulatePropabilities(&$genelist) {
    $cumul = 0;
    foreach($genelist as $k=>&$v) {
        $cumul = $v = $v * IM + $cumul;
    }
    $genelist = array_map('intval', array_map('ceil', $genelist));
}
