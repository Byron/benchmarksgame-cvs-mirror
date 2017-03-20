<?php
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   regex-dna program contributed by Danny Sauer
   modified by Josh Goldfoot
   modified by Sergey Khripunov
   modified by Craig Russell
   converted from regex-dna program
*/

$tok = ftok(__FILE__, chr(time() & 255));
$queue = msg_get_queue($tok);

$variants = array(
    'agggtaaa|tttaccct',
    '[cgt]gggtaaa|tttaccc[acg]',
    'a[act]ggtaaa|tttacc[agt]t',
    'ag[act]gtaaa|tttac[agt]ct',
    'agg[act]taaa|ttta[agt]cct',
    'aggg[acg]aaa|ttt[cgt]ccct',
    'agggt[cgt]aa|tt[acg]accct',
    'agggta[cgt]a|t[acg]taccct',
    'agggtaa[cgt]|[acg]ttaccct',
);

// IUB replacement parallel arrays
$IUB = array(
   '/tHa[Nt]/S',
   '/aND|caN|Ha[DS]|WaS/S',
   '/a[NSt]|BY/S',
   '/<[^>]*>/S',
   '/\\|[^|][^|]*\\|/S'
);
$IUBnew = array(
   '<4>',
   '<3>',
   '<2>',
   '|',
   '-'
);

// read in file
$contents = file_get_contents('php://stdin');
$initialLength = strlen($contents);

// remove things
$contents = preg_replace('/^>.*$|\n/mS', '', $contents);
$codeLength = strlen($contents);

// do regexp counts
$messages = array_flip($variants);
$chunks = str_split($contents, ceil(strlen($contents) / 4));
$workers = $results = array();
foreach ($variants as $key => $regex){
   if($key == 0 || $key == 2 || $key == 4 || $key == 6) {
      if($pid = pcntl_fork()) $workers[] = $pid;
  }
   if($pid && $key > 7) {
      $messages[$regex] =
         preg_match_all('/' . $regex . '/iS', $contents, $discard);
   }
   else if(!$pid) {
      $results[] = $regex . ',' . 
         preg_match_all('/' . $regex . '/iS', $contents, $discard);
      if($key == 1 || $key == 3 || $key == 5 || $key == 7) {
         $results[] = strlen(preg_replace($IUB, $IUBnew, $chunks[(int)($key / 2)]));
         msg_send($queue, 2, implode(';', $results), false, false, $errno);
         exit;
	  }
   }
}

// receive and output the counts
$contentLength = 0;
foreach($workers as $worker) {
   pcntl_waitpid($worker, $status);
   msg_receive($queue, 2, $msgtype, 4096, $message, false);
   $message = explode(';', $message);
   foreach($message as $key => $line) {
      if($key == 2)
         $contentLength += $line;
      else {
         $tmp = explode(',', $line, 2);
         $messages[$tmp[0]] = $tmp[1];
      }
   }
}
foreach($messages as $regex => $count) {
   echo $regex, ' ', $count, "\n";
}

echo "\n",
      $initialLength, "\n",
      $codeLength, "\n",
      $contentLength, "\n";
