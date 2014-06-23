/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Daryl Griffith 
 */

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;

public class knucleotide {

    static final HashTable MAP = new HashTable();
    static final int[] SEQUENCES1 = {1, 2};
    static final int[] SEQUENCES2 = {3, 4, 6, 12, 18};
    static final int LINE_LENGTH = 60;
    static final int EOF = -1;
    static byte[] nucleotides;

    public static void main(String[] args) {
        try (LineInputStream in = new LineInputStream(System.in)) {
            final byte[] temp = new byte[LINE_LENGTH];
            final byte[] buffer = new byte[125_000_000];
            final byte[] species = ">TH".getBytes();
            int n;
            int i;

            outer:
            for (;;) {
                n = in.readLine(temp);
                if (n == EOF) {
                    return;
                }
                if (n != LINE_LENGTH) {
                    for (i = 0; i < species.length; i++) {
                        if (temp[i] != species[i]) {
                            continue outer;
                        }
                    }
                    break;
                }
            }
            i = 0;
            for (;;) {
                n = in.readLine(temp);
                if (n == EOF) {
                    break;
                }
                for (int j = 0; j < n; i++, j++) {
                    buffer[i] = translate(temp[j]);
                }
            }
            if (i == buffer.length) {
                nucleotides = buffer;
            } else {
                nucleotides = new byte[i];
                System.arraycopy(buffer, 0, nucleotides, 0, i);
            }
        } catch (IOException e) {
        }
        countSequences(SEQUENCES1);
        printSequence(4, 8);
        printSequence(16, 32);
        countSequences(SEQUENCES2);
        {
            final String[] SPICIFIC_SEQUENCES = new String[] {"GGT"
                    , "GGTA"
                    , "GGTATT"
                    , "GGTATTTTAATT"
                    , "GGTATTTTAATTTATAGT"};
            final Entry key = new Entry();

            for (String sequence : SPICIFIC_SEQUENCES) {
                key.setKey(sequence);
                System.out.print(MAP.get(key).count);
                System.out.print('\t');
                System.out.println(sequence);
            }
        }
    }

    static byte translate(byte b) {
        return (byte) ((b >> 1) & 3);
    }

    static void countSequences(int[] sequences) {
        for (int sequence : sequences) {
            updateHashtable(sequence);
        }
    }

    static void updateHashtable(int sequence) {
        Entry key = new Entry();
        Entry value;
        final long sentinal = 1L << (sequence << 1);
        final long mask = sentinal - 1L;

        key.setKey(nucleotides.length - sequence + 1);
        for (int i = nucleotides.length - sequence; i >= 0; i--) {
            key.shiftKey(i, mask, sentinal);
            value = MAP.get(key);
            if (value != null) {
                value.count++;
                continue;
            }
            MAP.put(key);
        }
    }

    static void printSequence(int start, int end) {
        Entry[] sorted = new Entry[end - start];
        Entry key = new Entry();
        Entry value;
        int k = 0;
        int sum = 0;

        for (int i = start; i < end; i++) {
            key.key = i;
            value = MAP.get(key);
            if (value != null) {
                sum += value.count;
                sorted[k++] = value;
                for (i++ ; i < end; i++) {
                    key.key = i;
                    value = MAP.get(key);
                    if (value != null) {
                        int j;

                        sum += value.count;
                        for (j = k++; j > 0 && value.lessthan(sorted[j - 1]); j--) {
                            sorted[j] = sorted[j - 1];
                        }
                        sorted[j] = value;
                    }
                }
                break;
            }
        }
        for (int j = 0; j < k; j++) {
            value = sorted[j];
            System.out.format("%s %.3f\n", value, value.count * 100f / sum);
        }
        System.out.println();
    }

    static class LineInputStream implements Closeable {

        static final int LF = 10;
        final byte[] buf = new byte[8192];
        final InputStream in;
        int pos = 0;
        int end = 0;

        public LineInputStream(InputStream in) {
            this.in = in;
        }

        @Override
        public void close() throws IOException {
            in.close();
        }

        public int readLine(byte[] b) throws IOException {
            for (int i = pos; i < end; i++) {
                if (buf[i] == LF) {
                    if (i - pos == LINE_LENGTH) {
                        System.arraycopy(buf, pos, b, 0, LINE_LENGTH);
                        pos += LINE_LENGTH + 1;
                        return LINE_LENGTH;
                    }
                    int size = i - pos;

                    System.arraycopy(buf, pos, b, 0, size);
                    pos += size + 1;
                    return size;
                }
            }
            end = end - pos;
            System.arraycopy(buf, pos, buf, 0, end);
            pos = 0;
            int n = in.read(buf, end, buf.length - end);

            if (n == EOF) {
                if (end > 0) {
                    System.arraycopy(buf, pos, b, 0, end);
                    pos = end;
                    return end;
                }
                return EOF;
            }
            end += n;
            for (int i = pos; i < end; i++) {
                if (buf[i] == LF) {
                    if (i - pos == LINE_LENGTH) {
                        System.arraycopy(buf, pos, b, 0, LINE_LENGTH);
                        pos += LINE_LENGTH + 1;
                        return LINE_LENGTH;
                    }
                    int size = i - pos;

                    System.arraycopy(buf, pos, b, 0, size);
                    pos += size + 1;
                    return size;
                }
            }
            return EOF;
        }
    }

    static class Entry implements Cloneable {

        long key;
        int count = 1;
        Entry next;

        @Override
        protected Entry clone() throws CloneNotSupportedException {
            return (Entry) super.clone();
        }

        int hash() {
            return (int) (key ^ (key >>> 18));
        }

        boolean lessthan(Entry entry) {
            if (count != entry.count) {
                return count > entry.count;
            }
            return key > entry.key;
        }

        void setKey(int index) {
            key = 1;
            for (int i = nucleotides.length - 1; i >= index; i--) {
                key = (key << 2) | nucleotides[i];
            }
        }

        void setKey(String species) {
            key = 1;
            for (int i = species.length() - 1; i >= 0; i--) {
                key = (key << 2) | translate((byte) species.charAt(i));
            }
        }

        void shiftKey(int index, long mask, long sentinal) {
            key = ((key << 2) & mask) | nucleotides[index] | sentinal;
       }

        @Override
        public String toString() {
            final char[] name 
                    = new char[(63 - Long.numberOfLeadingZeros(key)) / 2];
            long temp = key;

            for (int i = 0; temp > 1; temp >>= 2, i++) {
                name[i] = (char) (((temp & 3) << 1) | 'A');
                if (name[i] == 'E') {
                    name[i] = 'T';
                }
            }
            return new String(name);
        }
    }

    static class HashTable {

        static final int LOAD_FACTOR = 0xc;
        Entry[] table = new Entry[1 << 4];
        int mask = table.length - 1;
        int size = 0;

        Entry get(Entry key) {
            Entry entry = table[key.hash() & mask];

            while (entry != null && entry.key != key.key) {
                entry = entry.next;
            }
            return entry;
        }

        void put(Entry entry) {
            if (((size << 4) / table.length) > LOAD_FACTOR) {
                resize();
            }
            try {
                putImpl(entry.clone());
            } catch (CloneNotSupportedException ex) {
            }
            size++;
        }

        void putImpl(Entry entry) {
            Entry e = table[entry.hash() & mask];
            Entry d;

            if (e == null) {
                table[entry.hash() & mask] = entry;
                return;
            }
            do {
                d = e;
                e = e.next;
            } while (e != null);
            d.next = entry;
        }

        void resize() {
            final Entry[] oldTable = table;
            Entry e;

            table = new Entry[table.length << 1];
            mask = table.length - 1;
            for (Entry entry : oldTable) {
                while (entry != null) {
                    e = entry.next;
                    entry.next = null;
                    putImpl(entry);
                    entry = e;
                }
            }
        }
    }
}