/*
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   regex-dna program contributed by Michael Stover
   converted from regex-dna program
 */

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class regexredux {

   private static final Map<String, String> replacements = new HashMap<String, String>();

   static {
      replacements.put("tHa[Nt]", "<4>");
      replacements.put("aND|caN|Ha[DS]|WaS", "<3>");
      replacements.put("a[NSt]|BY", "<2>");
      replacements.put("<[^>]*>", "|");
      replacements.put("\\|[^|][^|]*\\|", "-");
   }

   public static void main(String[] args) throws IOException {
      BufferedReader r = new BufferedReader(new InputStreamReader(System.in,
            "US-ASCII"));
      StringBuffer sb = new StringBuffer();
      String line;
      while ((line = r.readLine()) != null) {
         sb.append(line);
         sb.append("\n");
      }

      int initialLength = sb.length();

      final String sequence = sb.toString().replaceAll(">.*\n|\n", "");

      int codeLength = sequence.length();

      String[] variants = { "agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
            "a[act]ggtaaa|tttacc[agt]t", "ag[act]gtaaa|tttac[agt]ct",
            "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
            "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct",
            "agggtaa[cgt]|[acg]ttaccct" };

      final Map<String, Integer> results = new HashMap<String, Integer>();
      ThreadGroup tg = new ThreadGroup("regexWork");
      for (String v : variants) {
         final String variant = v;
         new Thread(tg, v) {
            @Override
            public void run() {
               int count = 0;
               Matcher m = Pattern.compile(variant).matcher(sequence);
               while (m.find()) {
                  count++;
               }
               results.put(variant, count);
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
      StringBuffer buf = new StringBuffer();
      Matcher m = Pattern.compile("[WYKMSRBDVHN]").matcher(sequence);
      while (m.find()) {
         m.appendReplacement(buf, "");
         buf.append(replacements.get(m.group()));
      }
      m.appendTail(buf);

      System.out.println();
      System.out.println(initialLength);
      System.out.println(codeLength);
      System.out.println(buf.length());
   }
}
