/*
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   regex-dna program contributed by Razii, 
      idea taken from Elliott Hughes and Roger Millington
   converted from regex-dna program
*/

import java.io.*;
import java.util.regex.*;
import java.util.*;

public final class regexredux {

   private static final Map<String, String> replacements = new HashMap<String, String>();

   static {

      replacements.put("tHa[Nt]", "<4>");
      replacements.put("aND|caN|Ha[DS]|WaS", "<3>");
      replacements.put("a[NSt]|BY", "<2>");
      replacements.put("<[^>]*>", "|");
      replacements.put("\\|[^|][^|]*\\|", "-");
   }

   static abstract class Rewriter {
      private Pattern pattern;
      private Matcher matcher;

      public Rewriter(String regularExpression) {

         this.pattern = Pattern.compile(regularExpression);
      }

      public String group(int i) {
         return matcher.group(i);
      }

      public abstract String replacement();

      public String rewrite(CharSequence original) {
         return rewrite(original, new StringBuffer(original.length())).toString();
      }

      public StringBuffer rewrite(CharSequence original, StringBuffer destination) {
         this.matcher = pattern.matcher(original);
         while (matcher.find()) {
            matcher.appendReplacement(destination, "");
            destination.append(replacement());
         }
         matcher.appendTail(destination);
         return destination;
      }
   }

   public static void main(String[] args)
   throws IOException {

      Reader r = new InputStreamReader(System.in, "ISO-8859-1");
      StringBuilder sb = new StringBuilder(5100000);
      char[] cbuf = new char[16384];
      int charsRead;
      while ((charsRead = r.read(cbuf)) != -1)
         sb.append(cbuf, 0, charsRead);

      int initialLength = sb.length();

      String sequence = new Rewriter(">.*\n|\n") {

         public String replacement() {
            return "";
         }
      }.rewrite(sb);


      int codeLength = sequence.length();

      String[] variants = { "agggtaaa|tttaccct" ,
                       "[cgt]gggtaaa|tttaccc[acg]",
                       "a[act]ggtaaa|tttacc[agt]t",
                       "ag[act]gtaaa|tttac[agt]ct",
                       "agg[act]taaa|ttta[agt]cct",
                       "aggg[acg]aaa|ttt[cgt]ccct",
                       "agggt[cgt]aa|tt[acg]accct",
                       "agggta[cgt]a|t[acg]taccct",
                       "agggtaa[cgt]|[acg]ttaccct"
                     };

      for (String variant : variants) {

         int count = 0;
         Matcher m = Pattern.compile(variant).matcher(sequence);
         while (m.find())
            count++;
         System.out.println(variant + " " + count);
      }

      sequence = new Rewriter("[WYKMSRBDVHN]") {

         public String replacement() {
            return replacements.get(group(0));
         }
      }.rewrite(sequence);

      System.out.println();
      System.out.println(initialLength);
      System.out.println(codeLength);
      System.out.println(sequence.length());

   }
}
