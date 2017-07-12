/*
 * The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/

 * contributed by Han Kai
 */

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class revcomp {
    private static final int NCPU = Runtime.getRuntime().availableProcessors();
    private static final int concurrency = NCPU >= 8 ? NCPU - 2 : NCPU - 1;
    private static final ExecutorService executor = Executors.newFixedThreadPool(concurrency);

    public static void main(String[] args) throws Exception {
        try (FileInputStream standIn = new FileInputStream(FileDescriptor.in);
                FileOutputStream standOut = new FileOutputStream(FileDescriptor.out);) {

            LinkedList<Strand> strandList = new LinkedList<>();
            Strand strand = new Strand(executor, concurrency);

            while (strand.readOneStrand(standIn) >= 0) {
                strandList.add(strand);
                strand.reverse();
                Strand head = strandList.getFirst();

                if (!head.isFinish() && strandList.size() > 1) {
                    head.waitUntilFinish(500);
                }

                if (head.isFinish()) {
                    head.write(standOut);
                    head.prepareNextStrand(strandList.getLast());
                    strandList.removeFirst();
                    strand = head;
                } else {
                    strand = strand.nextStrand();
                }
            }

            executor.shutdown();
            executor.awaitTermination(10, TimeUnit.MINUTES);

            for (Strand item : strandList) {
                item.write(standOut);
            }
        }
    }

    private static final class Chunk {
        public static final int CHUNK_SIZE = 64 * 1024;

        public int capacity = 0;
        public int length = 0;

        public final byte[] bytes = new byte[CHUNK_SIZE];

        public void clear() {
            capacity = 0;
            length = 0;
        }
    }

    private static final class Strand {

        private static final byte NEW_LINE = '\n';
        private static final byte ANGLE = '>';
        private static final int LINE_LENGTH = 61;

        private static final byte[] map = new byte[128];
        static {
            for (int i = 0; i < map.length; i++) {
                map[i] = (byte) i;
            }

            map['t'] = map['T'] = 'A';
            map['a'] = map['A'] = 'T';
            map['g'] = map['G'] = 'C';
            map['c'] = map['C'] = 'G';
            map['v'] = map['V'] = 'B';
            map['h'] = map['H'] = 'D';
            map['r'] = map['R'] = 'Y';
            map['m'] = map['M'] = 'K';
            map['y'] = map['Y'] = 'R';
            map['k'] = map['K'] = 'M';
            map['b'] = map['B'] = 'V';
            map['d'] = map['D'] = 'H';
            map['u'] = map['U'] = 'A';
        }

        private final int concurrency;
        private final ExecutorService executor;
        private final CountDownLatch latch;

        private int chunkCount = 0;
        private final ArrayList<Chunk> chunks = new ArrayList<>();

        private boolean finishNext = false;

        public Strand(ExecutorService executor, int concurrency) {
            super();
            this.executor = executor;
            this.concurrency = concurrency;
            this.latch = new CountDownLatch(concurrency);
        }

        public boolean isFinish() {
            return latch.getCount() == 0;
        }

        public void waitUntilFinish(long mills) throws InterruptedException {
            latch.await(mills, TimeUnit.MILLISECONDS);
        }

        private void ensureSize() {
            if (chunkCount == chunks.size()) {
                chunks.add(new Chunk());
            }
        }

        private boolean isLastChunk(Chunk chunk) {
            return chunk.length != chunk.capacity;
        }

        private void correctLentgh(Chunk chunk, boolean skipFirst) {
            final byte[] bytes = chunk.bytes;

            final int start = skipFirst ? 1 : 0;
            final int end = chunk.capacity;

            for (int i = start; i < end; i++) {
                if (ANGLE == bytes[i]) {
                    chunk.length = i;
                    return;
                }
            }

            chunk.length = chunk.capacity;
        }

        public void prepareNextStrand(Strand previous) {
            if (previous.chunkCount == 0 || previous.finishNext) {
                for (int i = 0; i < chunkCount; i++) {
                    chunks.get(i).clear();
                }

                chunkCount = 0;
                previous.finishNext = true;
                return;
            }

            Chunk first = chunks.get(0);
            Chunk previousLast = previous.chunks.get(previous.chunkCount - 1);

            if (previousLast.capacity == previousLast.length) {
                for (int i = 0; i < chunkCount; i++) {
                    chunks.get(i).clear();
                }

                chunkCount = 0;
                previous.finishNext = true;
                return;
            }

            System.arraycopy(previousLast.bytes, previousLast.length, //
                    first.bytes, 0, previousLast.capacity - previousLast.length);

            first.capacity = previousLast.capacity - previousLast.length;
            correctLentgh(first, true);

            for (int i = 1; i < chunkCount; i++) {
                chunks.get(i).clear();
            }

            chunkCount = 0;
            previous.finishNext = true;
        }

        public Strand nextStrand() {

            if (chunkCount == 0 || finishNext) {
                finishNext = true;
                return new Strand(executor, concurrency);
            }

            Chunk last = chunks.get(chunkCount - 1);

            if (last.capacity == last.length) {
                finishNext = true;
                return new Strand(executor, concurrency);
            }

            Strand strand = new Strand(executor, concurrency);
            strand.ensureSize();
            Chunk first = strand.chunks.get(0);

            System.arraycopy(last.bytes, last.length, first.bytes, 0, last.capacity - last.length);

            first.capacity = last.capacity - last.length;
            correctLentgh(first, true);

            finishNext = true;
            return strand;
        }

        public int readOneStrand(InputStream is) throws IOException {
            finishNext = false;

            while (true) {
                ensureSize();

                Chunk chunk = chunks.get(chunkCount);
                chunkCount++;

                if (isLastChunk(chunk)) {
                    return chunkCount;
                }

                byte[] bytes = chunk.bytes;

                int readLength = is.read(bytes, chunk.length, Chunk.CHUNK_SIZE - chunk.length);

                if (chunkCount == 1 && readLength < 0 && chunk.length == 0) {
                    return -1;
                }

                if (readLength > 0) {
                    chunk.capacity += readLength;
                    correctLentgh(chunk, chunkCount == 1);
                }

                if (readLength < 0 || isLastChunk(chunk)) {
                    return chunkCount;
                }
            }
        }

        public void write(OutputStream out) throws IOException {
            for (int i = 0; i < chunkCount; i++) {
                Chunk chunk = chunks.get(i);
                out.write(chunk.bytes, 0, chunk.length);
            }
        }

        public void reverse() throws InterruptedException {
            final int sumLength = getSumLength();
            final int titleLength = getTitleLength();
            final int dataLength = sumLength - titleLength;
            final int realDataLength = dataLength - ceilDiv(dataLength, LINE_LENGTH);

            final int leftEndIndex = realDataLength / 2;
            final int rawLeftEndIndex = leftEndIndex + leftEndIndex / (LINE_LENGTH - 1);
            final int leftEndChunkIndex = ceilDiv(rawLeftEndIndex + titleLength, Chunk.CHUNK_SIZE) - 1;
            final int realLeftEndIndex = (rawLeftEndIndex + titleLength) % Chunk.CHUNK_SIZE - 1;

            final int itemCount = ceilDiv(leftEndChunkIndex + 1, concurrency);

            for (int t = 0; t < concurrency; t++) {
                final int start = itemCount * t;
                final int end = Math.min(start + itemCount, leftEndChunkIndex + 1);

                Callable<Void> task = () -> {

                    for (int i = start; i < end; i++) {
                        int rawLeftIndex = i == 0 ? 0 : i * Chunk.CHUNK_SIZE - titleLength;

                        int leftIndex = rawLeftIndex - rawLeftIndex / LINE_LENGTH;
                        int rightIndex = realDataLength - leftIndex - 1;

                        int rawRightIndex = rightIndex + rightIndex / (LINE_LENGTH - 1);

                        int leftChunkIndex = i;
                        int rightChunkIndex = ceilDiv(rawRightIndex + titleLength, Chunk.CHUNK_SIZE) - 1;

                        int realLeftIndex = (rawLeftIndex + titleLength) % Chunk.CHUNK_SIZE;
                        int realRightIndex = (rawRightIndex + titleLength) % Chunk.CHUNK_SIZE;

                        int endIndex = leftEndChunkIndex == leftChunkIndex ? realLeftEndIndex : chunks
                                .get(leftChunkIndex).length - 1;

                        reverse(leftChunkIndex, rightChunkIndex, realLeftIndex, realRightIndex, endIndex);
                    }

                    latch.countDown();

                    return null;
                };

                executor.submit(task);
            }

        }

        private void reverse(int leftChunkIndex, int rightChunkIndex, int leftIndex, int rightIndex, int leftEndIndex) {

            final byte[] map = Strand.map;

            Chunk leftChunk = chunks.get(leftChunkIndex);
            Chunk rightChunk = chunks.get(rightChunkIndex);

            byte[] leftBytes = leftChunk.bytes;
            byte[] rightBytes = rightChunk.bytes;

            while (leftIndex <= leftEndIndex) {
                if (rightIndex < 0) {
                    rightChunk = chunks.get(--rightChunkIndex);
                    rightBytes = rightChunk.bytes;
                    rightIndex = rightChunk.length - 1;
                }

                if (leftBytes[leftIndex] == NEW_LINE) {
                    leftIndex++;
                }

                if (rightBytes[rightIndex] == NEW_LINE) {
                    rightIndex--;

                    if (rightIndex < 0) {
                        rightChunk = chunks.get(--rightChunkIndex);
                        rightBytes = rightChunk.bytes;
                        rightIndex = rightChunk.length - 1;
                    }
                }

                if (leftIndex <= leftEndIndex) {
                    byte lByte = leftBytes[leftIndex];
                    byte rByte = rightBytes[rightIndex];

                    leftBytes[leftIndex++] = map[rByte];
                    rightBytes[rightIndex--] = map[lByte];
                }
            }

        }

        private int ceilDiv(int a, int b) {
            return (a + b - 1) / b;
        }

        private int getSumLength() {
            int sumLength = 0;

            for (int i = 0; i < chunkCount; i++) {
                sumLength += chunks.get(i).length;
            }

            return sumLength;
        }

        private int getTitleLength() {
            Chunk first = chunks.get(0);
            byte[] bytes = first.bytes;

            for (int i = 0; i < first.length; i++) {
                if (bytes[i] == NEW_LINE) {
                    return (i + 1);
                }
            }

            return -1;
        }

    }
}
