<?php
/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/ 

   contributed by Peter Baltruschat
   modified by Levi Cameron
   modified (parallelized) by Sam Scarano
*/

function bottomUpTree($item, $depth)
{
    if (!$depth) return array(null,null,$item);
    $item2 = $item + $item;
    $depth--;
    return array(
        bottomUpTree($item2-1,$depth),
        bottomUpTree($item2,$depth),
        $item);
}

function itemCheck($treeNode) {
    return $treeNode[2]
    + ($treeNode[0][0] === null ? itemCheck($treeNode[0]) : $treeNode[0][2])
    - ($treeNode[1][0] === null ? itemCheck($treeNode[1]) : $treeNode[1][2]);
}

function makeAndCheck($i, $depth) {
    $result = 0;

    $t = bottomUpTree($i, $depth);
    $result += itemCheck($t);
    unset($t);

    $t = bottomUpTree(-$i, $depth);
    $result += itemCheck($t);
    unset($t);

    return $result;
}

function makeAndCheckAll($depth, $iterations) {
    $result = 0;
    for($i = 1; $i <= $iterations; ++$i)
        $result += makeAndCheck($i, $depth);

    return $result;
}

/**
 * While $socket is open, listen for (depth, iterations) pairs,
 * call makeAndCheckAll(), and sent the result back via $socket.
 *
 * This is run by each worker (child) process.
 */
function BTService($socket) {
    while ($data = fread($socket, 8)) {
        // Read off 2 32-bit ints representing depth and iterations.
        list($depth, $iterations) = array_values(
            unpack("ldepth/literations", $data));

//        $startTime = microtime(TRUE);
        $sum = makeAndCheckAll($depth, $iterations);
//        fprintf(STDERR, "Depth %d completed in %.4f seconds\n",
//            $depth, microtime(TRUE) - $startTime);

        // Send result back as 3 32-bit ints (depth, iterations, sum)
        fwrite($socket, pack("lll", $depth, $iterations, $sum));
    }

    fclose($socket);
}

/**
 * Invoked by master (parent) process to request jobs from workers via $socket.
 */
function BTSubmit($socket, $depth, $iterations) {
    fwrite($socket, pack("ll", $depth, $iterations));
}

/**
 * Invoked by master (parent) process to receive completed job
 * information from workers via $socket.
 */
function BTReceive($socket) {
    $data = fread($socket, 12);
    if ($data === NULL)
        return NULL;

    return array_values(unpack("ldepth/literations/lsum", $data));
}

$minDepth = 4;

$n = ($argc >= 2) ? $argv[1] : 1;
$maxDepth = max($minDepth + 2, $n);
$stretchDepth = $maxDepth + 1;

// Allow user to specify number of workers as optional third argument.
$procs = ($argc >= 3) ? $argv[2] : 4;


$stretchTree = bottomUpTree(0, $stretchDepth);
printf("stretch tree of depth %d\t check: %d\n",
    $stretchDepth, itemCheck($stretchTree));
unset($stretchTree);

$longLivedTree = bottomUpTree(0, $maxDepth);


// Fork worker processes, and create array of socket streams to talk to them.
$workerSockets = [];
for ($i = 0; $i < $procs; $i++) {
    list($socket1, $socket2) = stream_socket_pair(
        STREAM_PF_UNIX, STREAM_SOCK_STREAM, STREAM_IPPROTO_IP);

    $pid = pcntl_fork();
    if ($pid === -1) {
        die("Unable to fork\n");
    }
    else if ($pid === 0) {
        // Child - just run BTService()
        fclose($socket1);
        BTService($socket2);
        exit();
    }
    else {
        // Parent - store socket and continue
        fclose($socket2);
        $workerSockets[] = $socket1;
    }
}

// Accumulate results and print later (because work may be done out of order).
$results = [];

$iterations = 1 << $maxDepth;
$depth = $minDepth;

$ready = $workerSockets; // Initially, consider all workers ready

while (true) {
    // First, submit jobs to any workers that are ready.
    foreach ($ready as $socket) {
        if ($depth <= $maxDepth) {
            BTSubmit($socket, $depth, $iterations);
            $depth += 2;
            $iterations >>= 2;
        } else {
            // If there are no more jobs, remove the worker.
            unset($workerSockets[array_search($socket, $workerSockets)]);
            fclose($socket);
        }
    }

    // We're done when there are no more workers with jobs to do.
    if (count($workerSockets) === 0)
        break;

    // Wait until a worker becomes ready.
    $ready = $workerSockets;
    $write  = NULL;
    $except = NULL;
    $numReady = stream_select($ready, $write, $except, NULL);

    if ($numReady === false)
        die("Socket stream error\n");
    assert($numReady !== 0);

    // Receive responses from all ready workers
    foreach ($ready as $socket) {
        $response = BTReceive($socket);
        if ($response == NULL)
            die("Worker died unexpectedly\n");

        list($job_depth, $job_iterations, $sum) = $response;
        $results[$job_depth] = [$job_iterations, $sum];
    }
}

ksort($results); // In general, work is done out of order.
foreach ($results as $depth => list($iterations, $sum)) {
    printf("%d\t trees of depth %d\t check: %d\n",
        $iterations << 1, $depth, $sum);
}

printf("long lived tree of depth %d\t check: %d\n",
    $maxDepth, itemCheck($longLivedTree));

