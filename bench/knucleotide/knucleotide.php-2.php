<?
/* 
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Damien Bonvillain
   fixed by Isaac Gouy
   modified by Levi Cameron
   modified by Ludovic Urbain
*/


function read_sequence($id) {
   $id = '>' . $id;
   $ln_id = strlen($id);
   $fd = STDIN;

   // reach sequence three
   while (strpos($line, $id) ===false){
      $line = stream_get_line($fd, 64, "\n"); // returns faster when the length is too large.
      if(feof($fd)){
         exit(-1);
      }
   }
   // next, read the content of the sequence
   $r='';
   while (!feof($fd)) {
      $line = stream_get_line($fd, 64, "\n");
      if (!isset($line[0])) continue;
      $c = $line[0];
      if ($c === ';') continue;
      if ($c === '>') break;
      $r.=$line;
   }
   return strtoupper($r);
}

function nucl_count(&$sequence,&$seqlen, &$keys) {
   $map = array();
   if(strlen($keys[0])!=2){
      foreach($keys as $p){
         $map[$p]=substr_count($sequence,$p);
      }
   }else{
      foreach($keys as $p){
         //$map[$p]=substr_count($sequence,$p); 
               // doesn't count overlapping substrings
         $map[$p]=preg_match_all('/'.$p[0].'(?='.$p[1].')/',$sequence); 
               // lookbehind is slower. 
               // I couldn't find another way to do it using regex
               // and no options for other string matchers in php
         /*
         $p1=$p[0];
         $p2=$p[1];
         $map[$p]=0;
         for($i=0;$i<$seqlen-1;$i++){
            if($sequence[$i]===$p1 && $sequence[$i+1]===$p2){
               $map[$p]++;
            }
         }
         */
      }
   }
   return $map;
}

function percentages(&$a){
   $t=0;
   foreach($a as $k=>$v){
      $t+=$v;
   }
   foreach($a as $k=>$v){
      $a[$k]=$v/$t * 100;
   }
}

function freq_name_comparator($a, $b) {
   if ($a == $b) return 0;
   return  ($a < $b) ? 1 : -1;
}
$sequence = read_sequence('THREE');
$seqlen=strlen($sequence);
fclose(STDIN);

$jobs = array(
   array(array('A'),array('AA','AT','TA','TT'),array('GGT','GGTA')),
   array(array('T'),array('CA','GA','AG','AC'),array('GGTATT')),
   array(array('G'),array('TG','GT','TC','CT'),array('GGTATTTTAATT')),
   array(array('C'),array('GG','GC','CG','CC'),array('GGTATTTTAATTTATAGT'))
);

$tok = ftok(__FILE__, chr(time() & 255));
$queue = msg_get_queue($tok);

$parent = TRUE;
$count = count($jobs);
for ($i = 0; $i < $count-1; ++$i) {
   $pid = pcntl_fork();
   if ($pid === -1) {
      die('could not fork');
   } else if ($pid) {
      continue;
   }
   $parent = FALSE;
   break;
}
$result=array(
   $i,
   nucl_count($sequence, $seqlen, $jobs[$i][0]),
   nucl_count($sequence, $seqlen, $jobs[$i][1]),
   nucl_count($sequence, $seqlen, $jobs[$i][2])
);
if (!msg_send($queue, 2, $result, TRUE, FALSE, $errno)) {
   var_dump("$errno");
   var_dump(msg_stat_queue($queue));
}

if (!$parent) {
   exit(0);
}

$results = array();
foreach($jobs as $job) {
   msg_receive($queue, 2, $msgtype, 4096, $result, TRUE);
   $results[0][$result[0]] = $result[1];
   $results[1][$result[0]] = $result[2];
   $results[2][$result[0]] = $result[3];
   pcntl_wait($s);
}

for($i=0;$i<count($results);$i++){
   $tmp=array();
   for($j=0;$j<count($results[$i]);$j++){
      foreach($results[$i][$j] as $nucl=>$count){
         $tmp[$nucl]=$count;
      }
   }
   $results[$i]=$tmp;
}
percentages($results[0]);
percentages($results[1]);
uasort($results[0], 'freq_name_comparator');
uasort($results[1], 'freq_name_comparator');
$r='';
foreach($results[0] as $k=>$v){
   $r.=sprintf ("%s %.3f\n", $k, $v);
}
$r.="\n";
foreach($results[1] as $k=>$v){
   $r.=sprintf ("%s %.3f\n", $k, $v);
}
$r.="\n";
foreach($results[2] as $k=>$v){
   $r.=sprintf ("%d\t%s\n", $v, $k);
}
echo $r;
msg_remove_queue($queue);
