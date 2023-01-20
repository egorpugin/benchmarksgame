/*
 * The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

 * contributed by Han Kai
 */

import java.io.Closeable;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class revcomp {

    public static void main(String[] args) throws Exception {
        try (Strand strand = new Strand();
                FileInputStream standIn = new FileInputStream(FileDescriptor.in)
;
                FileOutputStream standOut = new FileOutputStream(FileDescriptor.
out);) {

            while (strand.readOneStrand(standIn) >= 0) {
                strand.reverse();
                strand.write(standOut);
                strand.reset();
            }

        }
    }
}

class Chunk {
    public static final int CHUNK_SIZE = 64 * 1024;

    public int capacity = 0;
    public int length = 0;

    public final byte[] bytes = new byte[CHUNK_SIZE];

    public void clear() {
        capacity = 0;
        length = 0;
    }
}

class Strand implements Closeable {

    private static final byte NEW_LINE = ʼ\nʼ;
    private static final byte ANGLE = ʼ>ʼ;
    private static final int LINE_LENGTH = 61;

    private static final byte[] map = new byte[128];
    static {
        for (int i = 0; i < map.length; i++) {
            map[i] = (byte) i;
        }

        map[ʼtʼ] = map[ʼTʼ] = ʼAʼ;
        map[ʼaʼ] = map[ʼAʼ] = ʼTʼ;
        map[ʼgʼ] = map[ʼGʼ] = ʼCʼ;
        map[ʼcʼ] = map[ʼCʼ] = ʼGʼ;
        map[ʼvʼ] = map[ʼVʼ] = ʼBʼ;
        map[ʼhʼ] = map[ʼHʼ] = ʼDʼ;
        map[ʼrʼ] = map[ʼRʼ] = ʼYʼ;
        map[ʼmʼ] = map[ʼMʼ] = ʼKʼ;
        map[ʼyʼ] = map[ʼYʼ] = ʼRʼ;
        map[ʼkʼ] = map[ʼKʼ] = ʼMʼ;
        map[ʼbʼ] = map[ʼBʼ] = ʼVʼ;
        map[ʼdʼ] = map[ʼDʼ] = ʼHʼ;
        map[ʼuʼ] = map[ʼUʼ] = ʼAʼ;
    }

    private static int NCPU = Runtime.getRuntime().availableProcessors();
    private ExecutorService executor = Executors.newFixedThreadPool(NCPU);

    private int chunkCount = 0;

    private final ArrayList<Chunk> chunks = new ArrayList<Chunk>();

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

    private void prepareNextStrand() {
        if (chunkCount == 0) {
            return;
        }

        Chunk first = chunks.get(0);
        Chunk last = chunks.get(chunkCount - 1);

        if (last.capacity == last.length) {
            for (int i = 0; i < chunkCount; i++) {
                chunks.get(i).clear();
            }

            return;
        }

        System.arraycopy(last.bytes, last.length, first.bytes, 0, last.capacity
- last.length);

        first.capacity = last.capacity - last.length;
        correctLentgh(first, true);

        for (int i = 1; i < chunkCount; i++) {
            chunks.get(i).clear();
        }
    }

    public int readOneStrand(InputStream is) throws IOException {
        while (true) {
            ensureSize();

            Chunk chunk = chunks.get(chunkCount);
            chunkCount++;

            if (isLastChunk(chunk)) {
                return chunkCount;
            }

            byte[] bytes = chunk.bytes;

            int readLength = is.read(bytes, chunk.length, Chunk.CHUNK_SIZE - chu
nk.length);

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

    public void reset() {
        prepareNextStrand();
        chunkCount = 0;
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
        final int realDataLength = dataLength - ceilDiv(dataLength, LINE_LENGTH)
;

        final int leftEndIndex = realDataLength / 2;
        final int rawLeftEndIndex = leftEndIndex + leftEndIndex / (LINE_LENGTH -
 1);
        final int leftEndChunkIndex = ceilDiv(rawLeftEndIndex + titleLength, Chu
nk.CHUNK_SIZE) - 1;
        final int realLeftEndIndex = (rawLeftEndIndex + titleLength) % Chunk.CHU
NK_SIZE - 1;

        final List<Callable<Void>> tasks = new ArrayList<Callable<Void>>(NCPU);

        final int itemCount = ceilDiv(leftEndChunkIndex + 1, NCPU);

        for (int t = 0; t < NCPU; t++) {
            final int start = itemCount * t;
            final int end = Math.min(start + itemCount, leftEndChunkIndex + 1);

            Callable<Void> task = () -> {
                for (int i = start; i < end; i++) {
                    int rawLeftIndex = i == 0 ? 0 : i * Chunk.CHUNK_SIZE - title
Length;

                    int leftIndex = rawLeftIndex - rawLeftIndex / LINE_LENGTH;
                    int rightIndex = realDataLength - leftIndex - 1;

                    int rawRightIndex = rightIndex + rightIndex / (LINE_LENGTH -
 1);

                    int leftChunkIndex = i;
                    int rightChunkIndex = ceilDiv(rawRightIndex + titleLength, C
hunk.CHUNK_SIZE) - 1;

                    int realLeftIndex = (rawLeftIndex + titleLength) % Chunk.CHU
NK_SIZE;
                    int realRightIndex = (rawRightIndex + titleLength) % Chunk.C
HUNK_SIZE;

                    int endIndex = leftEndChunkIndex == leftChunkIndex ? realLef
tEndIndex
                            : chunks.get(leftChunkIndex).length - 1;

                    reverse(leftChunkIndex, rightChunkIndex, realLeftIndex, real
RightIndex, endIndex);
                }

                return null;
            };

            tasks.add(task);
        }

        executor.invokeAll(tasks);
    }

    private void reverse(int leftChunkIndex, int rightChunkIndex, int leftIndex,
 int rightIndex, int leftEndIndex) {

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

    @Override
    public void close() throws IOException {
        executor.shutdown();
    }

}
