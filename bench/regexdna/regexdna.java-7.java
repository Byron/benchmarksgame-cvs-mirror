/*
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Michael Stover
   modified by Tagir Valeev
 */

import java.io.ByteArrayOutputStream;
import java.nio.CharBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.basistech.tclre.HsrePattern;
import com.basistech.tclre.PatternFlags;
import com.basistech.tclre.ReMatcher;
import com.basistech.tclre.RegexException;

public final class regexdna {

    private static String getReplacement(final String in) {
        switch (in.charAt(0)) {
        case 'W': return "(a|t)";
        case 'Y': return "(c|t)";
        case 'K': return "(g|t)";
        case 'M': return "(a|c)";
        case 'S': return "(c|g)";
        case 'R': return "(a|g)";
        case 'B': return "(c|g|t)";
        case 'D': return "(a|g|t)";
        case 'V': return "(a|c|g)";
        case 'H': return "(a|c|t)";
        case 'N': return "(a|c|g|t)";
        default:  return null;
        }
    }

    public static void main(String[] args) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        {
            byte[] buf = new byte[65536];
            int count;
            while ((count = System.in.read(buf)) > 0) {
                baos.write(buf, 0, count);
            }
        }
        String sb = baos.toString("US-ASCII");

        int initialLength = sb.length();

        String sequence = sb.replaceAll(">.*\n|\n", "");
        // CharBuffer wrapper is necessary as tcl-re actively uses subSequence call
        // which is O(string_length) for strings since Java 1.7u6
        // while with CharBuffer it has constant complexity
        CharBuffer cb = CharBuffer.wrap(sequence);

        ExecutorService pool = Executors.newFixedThreadPool(
                Runtime.getRuntime().availableProcessors());

        int codeLength = sequence.length();

        String[] variants = { "agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
                "a[act]ggtaaa|tttacc[agt]t", "ag[act]gtaaa|tttac[agt]ct",
                "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
                "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct",
                "agggtaa[cgt]|[acg]ttaccct" };

        Future<StringBuffer> replaceResult = pool.submit(new Callable<StringBuffer>() {
            @Override
            public StringBuffer call() throws RegexException {
                StringBuffer buf = new StringBuffer();
                Matcher m = Pattern.compile("[WYKMSRBDVHN]").matcher(sequence);
                while (m.find()) {
                    m.appendReplacement(buf, "");
                    buf.append(getReplacement(m.group()));
                }
                m.appendTail(buf);
                return buf;
            }
        });

        final Map<String, Future<Integer>> results = new HashMap<>();
        for (String v : variants) {
            results.put(v, pool.submit(new Callable<Integer>() {
                @Override
                public Integer call() throws RegexException {
                    ReMatcher m = HsrePattern.compile(v, PatternFlags.ICASE,
                            PatternFlags.ADVANCED, PatternFlags.NOSUB).matcher(cb);
                    int count = 0;
                    while (m.find()) {
                        count++;
                    }
                    return count;
                }
            }));
        }

        pool.shutdown();

        for (String variant : variants) {
            System.out.println(variant + " " + results.get(variant).get());
        }

        System.out.println();
        System.out.println(initialLength);
        System.out.println(codeLength);
        System.out.println(replaceResult.get().length());
    }
}
