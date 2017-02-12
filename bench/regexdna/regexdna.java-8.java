/*
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Francois Green 
*/

import java.io.*;

import java.util.*;
import java.util.Map.Entry;
import java.util.function.*;
import java.util.regex.*;

import static java.util.stream.Collectors.*;

public class regexdna {
  
  static final Map<String, String> REPLACEMENTS = new HashMap<>();

  static {
    REPLACEMENTS.put("W", "(a|t)");
    REPLACEMENTS.put("Y", "(c|t)");
    REPLACEMENTS.put("K", "(g|t)");
    REPLACEMENTS.put("M", "(a|c)");
    REPLACEMENTS.put("S", "(c|g)");
    REPLACEMENTS.put("R", "(a|g)");
    REPLACEMENTS.put("B", "(c|g|t)");
    REPLACEMENTS.put("D", "(a|g|t)");
    REPLACEMENTS.put("V", "(a|c|g)");
    REPLACEMENTS.put("H", "(a|c|t)");
    REPLACEMENTS.put("N", "(a|c|g|t)");
  }
  
  public static void main(String[] args) throws IOException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    {
        byte[] buf = new byte[65536];
        int count;
        while ((count = System.in.read(buf)) > 0) {
            baos.write(buf, 0, count);
        }
    }
    final String input = baos.toString("US-ASCII");
            
    final int initialLength = input.length();
    
    final String sequence = input.replaceAll(">.*\n|\n", "");
           
    final int codeLength = sequence.length();
    
    final List<String> variants = Arrays.asList("agggtaaa|tttaccct", 
                                                "[cgt]gggtaaa|tttaccc[acg]",
                                                "a[act]ggtaaa|tttacc[agt]t", 
                                                "ag[act]gtaaa|tttac[agt]ct",
                                                "agg[act]taaa|ttta[agt]cct", 
                                                "aggg[acg]aaa|ttt[cgt]ccct",
                                                "agggt[cgt]aa|tt[acg]accct", 
                                                "agggta[cgt]a|t[acg]taccct",
                                                "agggtaa[cgt]|[acg]ttaccct");
    
    BiFunction<String, String, Entry<String, Integer>> counts = (v, s) -> {
      final Matcher matcher = Pattern.compile(v).matcher(s);
      Integer count = 0;
      while(matcher.find()) {
        count++;
      }
      return new AbstractMap.SimpleEntry<>(v, count);
    };
    
    final Map<String, Integer> results = 
         variants.parallelStream()
                 .map(variant -> counts.apply(variant, sequence))
                 .collect(toMap(Map.Entry::getKey, Map.Entry::getValue));
                           
    variants.forEach(
         variant -> System.out.println(variant + " " + results.get(variant)));
 
    StringBuffer buf = new StringBuffer();
    Matcher m = Pattern.compile("[WYKMSRBDVHN]").matcher(sequence);
    while (m.find()) {
         m.appendReplacement(buf, "");
         buf.append(REPLACEMENTS.get(m.group()));
    }
    m.appendTail(buf);
    
    System.out.println();
    System.out.println(initialLength);
    System.out.println(codeLength);
    System.out.println(buf.length());
  }
}


