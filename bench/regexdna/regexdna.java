/*
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Michael Stover
   modified by Tagir Valeev
 */

import java.nio.CharBuffer;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.basistech.tclre.HsrePattern;
import com.basistech.tclre.PatternFlags;
import com.basistech.tclre.ReMatcher;
import com.basistech.tclre.RePattern;
import com.basistech.tclre.RegexException;

public final class regexdna {

   private static final Map<String, String> replacements = new HashMap<>();

   static {
      replacements.put("W", "(a|t)");
      replacements.put("Y", "(c|t)");
      replacements.put("K", "(g|t)");
      replacements.put("M", "(a|c)");
      replacements.put("S", "(c|g)");
      replacements.put("R", "(a|g)");
      replacements.put("B", "(c|g|t)");
      replacements.put("D", "(a|g|t)");
      replacements.put("V", "(a|c|g)");
      replacements.put("H", "(a|c|t)");
      replacements.put("N", "(a|c|g|t)");
   }

   public static void main(String[] args) throws IOException {
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      {
         byte[] buf = new byte[65536];
         int count;
         while((count = System.in.read(buf)) > 0) {
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

      int codeLength = sequence.length();

      String[] variants = { 
            "agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
            "a[act]ggtaaa|tttacc[agt]t", "ag[act]gtaaa|tttac[agt]ct",
            "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
            "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct",
            "agggtaa[cgt]|[acg]ttaccct" };

      final Map<String, Integer> results = new ConcurrentHashMap<>();
      ThreadGroup tg = new ThreadGroup("regexWork");
      for (String v : variants) {
         final String variant = v;
         new Thread(tg, v) {
            @Override
            public void run() {
               try {
                  ReMatcher m = HsrePattern.compile(v, PatternFlags.ICASE, 
                     PatternFlags.ADVANCED, PatternFlags.NOSUB).matcher(cb);
                  int count = 0;
                  while (m.find()) {
                     count++;
                  }
                  results.put(variant, count);
               }
               catch(RegexException re) {
                  throw new RuntimeException(re);
               }
            }
         }.start();
      }
      Thread[] threads = new Thread[variants.length];
      tg.enumerate(threads);
      for (Thread t : threads) {
         try {
            if (t != null) {
               t.join();
            }
         } catch (InterruptedException e) {
            // noop
         }
      }
      tg.destroy();
      for (String variant : variants) {
         System.out.println(variant + " " + results.get(variant));
      }
      
      StringBuilder buf = new StringBuilder();
      Matcher m = Pattern.compile("[WYKMSRBDVHN]").matcher(sequence);
      int lastEnd = 0;
      while (m.find()) {
         buf.append(sequence, lastEnd, m.start()).append(replacements.get(m.group()));
         lastEnd = m.end();
      }
      buf.append(sequence, lastEnd, codeLength);

      System.out.println();
      System.out.println(initialLength);
      System.out.println(codeLength);
      System.out.println(buf.length());
   }
}
