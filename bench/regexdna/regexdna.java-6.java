/*
   The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Michael Stover
   modified by Stefan Feldbinder
 */

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class regexdna {

  private static String getReplacement(final String in) {
    switch (in) {
      case "W":
        return "(a|t)";
      case "Y":
        return "(c|t)";
      case "K":
        return "(g|t)";
      case "M":
        return "(a|c)";
      case "S":
        return "(c|g)";
      case "R":
        return "(a|g)";
      case "B":
        return "(c|g|t)";
      case "D":
        return "(a|g|t)";
      case "V":
        return "(a|c|g)";
      case "H":
        return "(a|c|t)";
      case "N":
        return "(a|c|g|t)";
      default:
        return null;
    }
  }

  public static void main(String[] args) throws IOException {
    ByteArrayOutputStream bao = new ByteArrayOutputStream();
    byte[] buffer = new byte[1024 * 4];
    int bytesRead;
    while ((bytesRead = System.in.read(buffer)) > 0) {
      bao.write(buffer, 0, bytesRead);
    }
    String input = bao.toString("US-ASCII");

    int initialLength = input.length();

    final String sequence = input.replaceAll(">.*\n|\n", "");

    int codeLength = sequence.length();

    String[] variants = {"agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t", "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct"};

    for (String variant : variants) {
      int count = 0;
      Matcher m = Pattern.compile(variant).matcher(sequence);
      while (m.find()) {
        ++count;
      }
      System.out.println(variant + " " + count);
    }

    StringBuffer buf = new StringBuffer();
    Matcher m = Pattern.compile("[WYKMSRBDVHN]").matcher(sequence);
    while (m.find()) {
      m.appendReplacement(buf, "");
      buf.append(getReplacement(m.group()));
    }
    m.appendTail(buf);

    System.out.println();
    System.out.println(initialLength);
    System.out.println(codeLength);
    System.out.println(buf.length());
  }
}
