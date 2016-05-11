/*  The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/

    Contributed by Joe Farro
    parts taken from solution contributed by
    Jesse Millikan which was modified by Matt Baker
    modified for Node.js by Joe Farro and Isaac Gouy
*/


// convert a character to a two-bit number
function charToInt(str) {
    switch (str) {
        case 'a': return 0;
        case 'c': return 1;
        case 'g': return 2;
        case 't': return 3;
    }
}


// convert a number, comprised of a sequence of two-bit encoded characters, to
// the corresponding string of characters
function toStr(num, len) {
    var res = '';
    while (len > 0) {
        switch (num & 3) {
            case 0: res = 'A' + res; break;
            case 1: res = 'C' + res; break;
            case 2: res = 'G' + res; break;
            case 3: res = 'T' + res; break;
        }
        num = num >>> 2;
        len--;
    }
    return res;
}


// convert a string of characters to a number derived from the corresponding
// sequence of two-bit numbers
function toUint(str) {
    var offset = 2 * str.length,
        uint = new Uint32Array(new ArrayBuffer(4)),
        i = 0;
    while (offset) {
        offset -= 2;
        uint[0] |= (charToInt(str[i]) << offset);
        i++;
    }
    return uint[0];
}


// A special-use data structure the end result of which is to store a mapping
// of two-bit encoded character sequences to integers. The integer value can be
// mutated only by incrementing it, with a default value of 0 being used when a
// key is first encountered. The data structure has additional state in that
// there is a "current key" at all times and only the value corresponding to it
// can be incremented. The data structure is catered to a particular method of
// input in which characters in the key to increment are input one at a time,
// with subsequent characters pushing previous characters to the left in the
// two-bit sequence. The number of characters to use as a key is configured via
// the `len` constructor parameter.
//
// An example: An instance can be initialized as `const s = new SeqSet(2)`.
// Then, the character sequence 'gc' can be input (e.g. the corresponding value
// is to be incremented). First 'g' (i.e. 0b10) is input via
// `s.pushToken(0b10)`. At this point the current key is `0b0010`. Then, 'c' is
// input (`s.pushToken(0b01)`), which results in the current key being
// `0b1001`. Next, to increment the corresponding value, `s.inc()` is called.
// To then increment the value associated with 'ca', `s.pushToken(0b00)` is
// called, which results in the current key being `0b0100`, effectively
// shifting the 'g' character out of the key (beacuse `len` is 2). Next,
// `s.inc()` increments the value associated with 'ca'.
//
// The process of pushing a single character and incrementing the corresponding
// value can be combined into one step with `s.incWithToken(...)`.
//
// Storage of the data is done in a two-tier structure where the keys are
// broken into two halves with 12 characters in the first half and the
// remaining characters being in the second half. The halves of the keys are
// stored as `s.uintLeft` and `s.uintRight`. And, values are accessed via
// `s.data[s.uintLeft][s.uintRight]`.
function SeqSets(len) {
    this.seqLen = len;
    this.uintLeft = 0;
    this.uintRight = 0;
    this.maskLeft = len <= 12 ? 0 : Math.pow(2, (len - 12) * 2) - 1;
    this.maskRight = Math.pow(2, Math.min(12, len) * 2) - 1;
    this.data = {};
    this.lastUintLeft = -1;
    this.lastLeftData = undefined;
}

// shift the current key left and add a new char to the right end
SeqSets.prototype.pushToken = function(char) {
    this.uintLeft = (this.uintLeft << 2 | this.uintRight >>> 22) & this.maskLeft;
    this.uintRight = (this.uintRight << 2 | char) & this.maskRight;
};

// increment the integer associated with the current key
SeqSets.prototype.inc = function() {
    if (this.uintLeft !== this.lastUintLeft) {
        this.lastUintLeft = this.uintLeft;
        this.lastLeftData = this.data[this.uintLeft] || (this.data[this.uintLeft] = {});
    }
    this.lastLeftData[this.uintRight] = (this.lastLeftData[this.uintRight] || 0) + 1;
};

// combine the token shifting and increment process
SeqSets.prototype.incWithToken = function(char) {
    this.uintLeft = (this.uintLeft << 2 | this.uintRight >>> 22) & this.maskLeft;
    this.uintRight = (this.uintRight << 2 | char) & this.maskRight;
    if (this.uintLeft !== this.lastUintLeft) {
        this.lastUintLeft = this.uintLeft;
        this.lastLeftData = this.data[this.uintLeft] || (this.data[this.uintLeft] = {});
    }
    this.lastLeftData[this.uintRight] = (this.lastLeftData[this.uintRight] || 0) + 1;
};

// get the count associated with a given character sequence
SeqSets.prototype.getCount = function(seq) {
    var seqLeft = seq.length <= 12 ? '' : seq.substr(0, seq.length - 12),
        seqRight = seq.substr(-12),
        uintLeft = seqLeft && toUint(seqLeft) || 0,
        uintRight = toUint(seqRight);

    return this.data[uintLeft][uintRight];
};

const stdin = process.stdin;
// the total length of data received
var dataLength = 0;
// the first 18 characers of relevant input are a special case because, for
// each sequence set, the full length of the key needs to be accumulated before
// any value is incremented
var first18 = 0;
const seq1 = new SeqSets(1);
const seq2 = new SeqSets(2);
const seq3 = new SeqSets(3);
const seq4 = new SeqSets(4);
const seq6 = new SeqSets(6);
const seq12 = new SeqSets(12);
const seq18 = new SeqSets(18);
const tables = [
    seq1,
    seq2,
    seq3,
    seq4,
    seq6,
    seq12,
    seq18,
];


// stdin listener to ignore the initial set of data which is irrelevant
function skipHead() {
    const chunk = stdin.read();
    const i = chunk.indexOf('>TH');
    if (i < 0) {
        return;
    }
    // stop reading with skipHead
    stdin.removeListener('readable', skipHead);
    // get the interesting part of the chunk
    const line = chunk.slice(chunk.indexOf('\n', i) + 1);
    if (line.length <= 18) {
        stdin.on('readable', readFirst18);
    }
    readFirst18(line);
}


// stdin listener to listen for the first 18 characters of data (in the event
// that the chunk happens to break in the middle of the first 18 characters of
// relevant data)
function readFirst18(initialChunk) {
    const line = initialChunk || stdin.read();
    // the first-line is a special case as not all the counts should start
    // saving immediately
    var j = 0;
    const slen = tables.length;
    var si = 0;
    var seqSet = tables[0];
    while (first18 < 18 && j < line.length) {
        const char = charToInt(line[j]);
        si = 0;
        for (; si < slen; si++) {
            seqSet = tables[si];
            seqSet.pushToken(char);
            if (seqSet.seqLen <= j + 1) {
                seqSet.inc();
            }
        }
        j++;
        first18++;
    }
    if (first18 === 18) {
        dataLength = 18;
        stdin.removeListener('readable', readFirst18);
        stdin.on('readable', readInput);
        if (j < line.length) {
            readInput(line.slice(j));
        }
    }
}


// stdin listener to read the relevant section of data (read to EOF)
function readInput(initialChunk) {

    const chunk = initialChunk || stdin.read();
    if (!chunk) {
        readingDone();
        return;
    }
    const len = chunk.length;
    var i = 0;
    var newLines = 0;
    var charCode = 0;
    var char = 0;
    while (i < len) {
        charCode = chunk[i].charCodeAt(0);
        i++;
        switch (charCode) {
            // a
            case 97:
                char = 0;
                break;
            // c
            case 99:
                char = 1;
                break;
            // g
            case 103:
                char = 2;
                break;
            // t
            case 116:
                char = 3;
                break;
            // new line
            case 10:
                newLines++;
                continue;
        }
        seq1.incWithToken(char);
        seq2.incWithToken(char);
        seq3.incWithToken(char);
        seq4.incWithToken(char);
        seq6.incWithToken(char);
        seq12.incWithToken(char);
        seq18.incWithToken(char);
    }
    dataLength += len - newLines;
}


function sortCounts(data, seqLen) {
    var keys = Object.keys(data),
        pctFactor = 100 / (dataLength - seqLen + 1);
    keys.sort(function(a, b) {
        return data[b] - data[a];
    });
    keys.forEach(function(code) {
        console.log(toStr(code, seqLen), (data[code] * pctFactor).toFixed(3));
    });
    console.log();
}


function readingDone(){

    sortCounts(seq1.data[0], 1);
    sortCounts(seq2.data[0], 2);

    console.log(seq3.getCount('ggt') +'\tGGT');
    console.log(seq4.getCount('ggta') +'\tGGTA');
    console.log(seq6.getCount('ggtatt') +'\tGGTATT');
    console.log(seq12.getCount('ggtattttaatt') +'\tGGTATTTTAATT');
    console.log(seq18.getCount('ggtattttaatttatagt') + '\tGGTATTTTAATTTATAGT');
}

stdin.setEncoding('ascii');
stdin.on('readable', skipHead);
