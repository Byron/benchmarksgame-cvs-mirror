/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   Transliterated from Scala #2 program by Daniil Kolpakov

*/
import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

class knucleotide {
    public static int BlockSize = 1024*1024;

    // Iterators are not specialized so we need our own
    interface LongIter {
        boolean hasNext();
        long next();
        default public void foreach(LongConsumer f) {
            while (hasNext()) {
                f.accept(next());
            }
        }
    }

    public static byte[] table = new byte[256];
    
    static {
        Arrays.fill(table, (byte)-1);
        table['a'] = table['A'] = 0;
        table['t'] = table['T'] = 1;
        table['g'] = table['G'] = 2;
        table['c'] = table['C'] = 3;
        table['\n'] = -3;
        table['>'] = -2;
    }

  // More efficient to store DNA sequence data as bits instead of bytes
    public static class Bits {
        int[] data;
        public Bits(int[] data) {
            this.data = data;
        }
        int size = 0;
        int index = 0;
        int n = 0;

        public void add2(byte b) {
            size += 1;
            if (n > 30) {
                index += 1;
                n = 0;
            }
            data[index] |= (b & 0x3) << n;
            n += 2;
        }

        public void addLots(Bits bs) {
            if (n == 0 || n > 30) {
                if (n > 30) {
                    index += 1;
                    n = 0;
                }
                System.arraycopy(bs.data,0,data,index,bs.index);
                index += bs.index;
                if (index > 0 && n == 0) {
                    index -= 1;
                    n = 32;
                }
            } else {
                int i = 0;
                while (i < bs.index) {
                    int j = bs.data[i];
                    data[index] |= j << n;
                    index += 1;
                    data[index] |= j >>> (32-n);
                    i += 1;
                }
            }
            size += bs.index * 16;
            if (bs.n != 0) {
                int n = bs.n;
                int i = bs.data[bs.index];
                while (n > 0) {
                    add2((byte)i);
                    i >>>= 2;
                    n -= 2;
                }
            }
        }

        public LongIter scan(int n, int offset) {
            return new LongIter() {
                int i = offset % 16;
                int j = offset / 16;
                long mask = (1L << (2*n)) - 1;
                public boolean hasNext() {
                    return j*16 + i + n <= size;
                }
                public long next() {
                    if (i+n <= 16) {
                        long l = ((data[j] >>> (2*i)) & mask);
                        i += n;
                        if (i>=16) {
                            j += 1;
                            i -= 16;
                        }
                        return l;
                    } else {
                        long l = ((long)(((data[j] >>> (2*i)))) | (((long)data[j+1]) << 2*(16-i))) & mask;
                        j += 1;
                        i += n - 16;
                        if (i>=16) {
                            j += 1;
                            i -= 16;
                        }
                        return l;
                    }
                }
            };
        }
    }
    private static Bits read(InputStream is, byte[] target, int[] need, boolean[] found, boolean[] nl, boolean[] done)
    throws IOException {
        byte[] data = new byte[BlockSize];
        int n = is.read(data);
        int i = 0;
        while (i<n && need[0]<target.length) {
            if (data[i]==target[need[0]]) need[0] += 1; else need[0] = 0;
            i += 1;
        }
        if (need[0] >= target.length && !found[0]) {
            while (i<n && data[i]!='\n') i += 1;
            if (i<n) found[0] = true;
        }
        if (found[0] && !done[0])
        {
            Bits bits = new Bits(new int[1+((n-i)>>4)]);
            while (i < n) {
                byte x = table[data[i]&0xFF];
                if (x >= 0) { bits.add2(x); nl[0]= false; }
                else if (x == -3) nl[0] = true;
                else if (nl[0] && x == -2) { i = n; done[0] = true; }
                i += 1;
            }
            return bits;
        }
        else if (n==BlockSize && !done[0]) return read(is, target, need, found, nl, done);
        else return new Bits(new int[0]);
    }
    public static Bits load(InputStream is, byte[] target) throws IOException {
        int[] need = {1};
        boolean[] found = {false};
        boolean[] nl = {false};
        boolean[] done = {false};
        List<Bits> dataList = new ArrayList<>();
        int dataSizeSum = 0;
        while(true) {
            Bits data = read(is, target, need, found, nl, done);
            if (data.size > 0) {
                dataList.add(data);
                dataSizeSum += data.size;
            } else {
                break;
            }
        }
        Bits all = new Bits(new int[dataSizeSum/16+1]);
        for (int i = 0; i < dataList.size(); i++) {
            all.addLots(dataList.get(i));
        }
        return all;
    }
    
    // Utility to go from binary to text representation
    public static String[] decode = {"A", "T", "G", "C"};
    public static String l2s(long l, int n) {
        if (n <= 0) {
            return "";
        } else {
            return decode[(int)(l & 0x3)] + l2s(l >>> 2, n - 1);
        }
    }
    
    // Custom counted hash set (neither Java nor Scala provides one)
    public static class DnaHash {
        public int z;
        public DnaHash(int z) {
            this.z = z;
        }
        int size = 16;
        int n = 0;
        long[] keys = new long[size];
        int[] counts = new int[size];
        final int hc(long l) {
            return ((int)( ((int)l) + (int)(l>>17) )) & (size-1);
        }
        final int nx(int i) {
            return (i+1) & (size - 1);
        }
        public final void plusEq(long key) {
            plusEq(key, 1);
        }
        public void plusEq(long key, int count) {
            int index = hc(key);
            if (counts[index] == 0) {
                keys[index] = key;
                counts[index] = count;
                n += 1;
            } else if (keys[index] == key) {
                counts[index] += count;
            } else if (6*n > size) {
                long[] oldk = keys;
                int[] oldc = counts;
                int olds = size;
                size *= 2;
                keys = new long[size];
                counts = new int[size];
                n = 0;
                int i = 0;
                while (i < olds) {
                    if (oldc[i] > 0) {
                        this.plusEq(oldk[i], oldc[i]);
                    }
                    i += 1;
                }
                this.plusEq(key);
            } else {
                int i = nx(index);
                while (counts[i] != 0 && keys[i] != key) {
                    i = nx(i);
                }
                if (counts[i] == 0) {
                    keys[i] = key;
                    counts[i] = count;
                    n += 1;
                } else {
                    counts[i] += count;
                }
            }
        }
        public int apply(long key) {
            int i = hc(key);
            while (counts[i] > 0 && keys[i] != key) {
                i = nx(i);
            }
            return counts[i];
        }
        static class Freq implements Comparable<Freq> {
            double freq;
            String label;
            public Freq(double freq, String label) {
                this.freq = freq;
                this.label = label;
            }
            
            public int compareTo(Freq another) {
                if ((freq > another.freq) || (freq == another.freq) && (label.compareTo(another.label) == 1)) {
                    return -1;
                } else if (freq == another.freq) {
                    return 0;
                } else {
                    return 1;
                }
            }
        }
        public void printSorted() {
            int countsSum = 0;
            for (int i = 0; i < counts.length; i++) {
                countsSum += counts[i];
            }
            double factor = 100.0 / countsSum;

            List<Freq> freqList = new ArrayList<>();
            
            for (int i = 0; i < counts.length; i++) {
                double freq = counts[i] * factor;
                if (freq > 0) {
                    freqList.add(new Freq(freq, l2s(keys[i], z)));
                }
            }
            
            Collections.sort(freqList);
            
            for (Freq freq : freqList) {
                System.out.printf("%s %.3f\n", freq.label, freq.freq);
            }
            System.out.println();
        }
        public void print(String s) {
            byte[] bytes = s.getBytes();
            long key = 0;
            for (int i = bytes.length - 1; i >= 0; i--) {
                key = 4L * key + table[bytes[i] & 0xFF];
            }
            System.out.printf("%d\t%s\n", apply(key), s);
        }
    }

    // Required function that adds data with offset to hash set
    public static void addToHash(Bits data, DnaHash hash, int n, int offset) {
        for (LongIter li = data.scan(n,offset); li.hasNext();) {
            hash.plusEq(li.next());
        }
    }

    public static void main(String[] args) throws Exception {
        ExecutorService es = Executors.newCachedThreadPool();
        List<Future<DnaHash>> results = new ArrayList<>();
        int[] sizes = new int[] { 1, 2, 3, 4, 6, 12, 18 };
        String sequence = "GGTATTTTAATTTATAGT";
        final Bits data = load(System.in, "\n>THREE".getBytes());
        for (int i = 0; i < sizes.length; i++) {
            final int n = sizes[i];
            results.add(es.submit(new Callable<DnaHash>() {
                public DnaHash call() {
                    DnaHash h = new DnaHash(n);
                    for (int j = 0; j < n; j++) {
                        addToHash(data, h, n, j);
                    }
                    return h;
                }
            }));
        }
        results.get(0).get().printSorted();
        results.get(1).get().printSorted();
        for (int i = 2; i < sizes.length; i++) {
            results.get(i).get().print(sequence.substring(0, sizes[i]));
        }
        es.shutdown();
    }
}
 
