/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
 *
 * byte processing, C# 3.0 idioms, frame level paralellism by Robert F. Tobler
 * modified by Rene Koeldorfer
 */

using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;


public static class Extensions
{
   public static byte[] GetBytes(this List<string> lines)
   {
      int count = lines.Aggregate(0, (cnt, str) => cnt + str.Length);
      var array = new byte[count];
      lines.Aggregate(0, (pos, str) => {
         Encoding.ASCII.GetBytes(str, 0, str.Length, array, pos);
         return pos + str. Length;
      });
      return array;
   }
}

public class Program
{
   public static int TaskCount;
   public static int Current = -1;
   public static KNucleotide[] kna;
      public static void Main(string[] args) {

      string line;
      StreamReader source = new StreamReader(Console.OpenStandardInput());
      var input = new List<string>();

      while ( (line = source.ReadLine() ) != null )
         if (line[0] == '>' && line.Substring(1, 5) == "THREE")
            break;

      while ( (line = source.ReadLine()) != null ) {
         char c = line[0];
         if (c == '>') break;
         if (c != ';') input.Add(line.ToUpper());
      }

      var lengths = new [] { 1, 2, 3, 4, 6, 12, 18 };

      TaskCount = lengths.Aggregate(0, (cnt, len) => cnt + len);
      kna = new KNucleotide[TaskCount];

      var bytes = input.GetBytes();        
      lengths.Aggregate(0, (cnt, len) => 
         {
            for (int i = 0; i < len; i++)
               kna[cnt + i] = new KNucleotide(bytes, len, i); 
            return cnt + len;
         });

      var threads = new Thread[Environment.ProcessorCount];
      for (int i = 0; i < threads.Length; i++)
         (threads[i] = new Thread(CountFrequencies)).Start();

      foreach (var t in threads)
         t.Join();

      var seqs = new[] { null, null,
         "GGT", "GGTA", "GGTATT", "GGTATTTTAATT",
         "GGTATTTTAATTTATAGT"};

      int index = 0;
      lengths.Aggregate(0, (cnt, len) =>
         {
            if (len < 3)
            {
               for (int i = 1; i < len; i++)
                  kna[cnt].AddFrequencies(kna[cnt+i]);
               kna[cnt].WriteFrequencies();                    
            }
            else
            {
               var fragment = seqs[index];
               int freq = 0;
               for (int i = 0; i < len; i++)
                  freq += kna[cnt + i].GetCount(fragment);
               Console.WriteLine("{0}\t{1}", freq, fragment);
            }
            index++;
            return cnt + len;
         });

   
   }

   static void CountFrequencies()
   {
      int index;
      while ((index = Interlocked.Increment(ref Current)) < TaskCount)
         kna[index].KFrequency();
   }

}

public class KNucleotide {

   private class KNucleotideNode {
      public int V;
      public int KeyLength;
      public KNucleotideNode(int v,int keyLength) { V = v;KeyLength=keyLength; }
   }
   private static byte[] tonum=new byte[256];
   private static char[] tochar=new char[4];

   static KNucleotide()
   {
      tonum['A'] = 0;
      tonum['C'] = 1;
      tonum['T'] = 2;
      tonum['G'] = 3;
      tochar[0] = 'A';
      tochar[1] = 'C';
      tochar[2] = 'T';
      tochar[3] = 'G';
   }

   private static ulong CreateKey(byte[] s,int keyStart,int keyLength)
   {
      ulong key = 0;
      for(int i = 0; i < keyLength; i++)
      {
         key <<= 2;
         key |= tonum[s[keyStart+i]];
      }
      return key;
   }

   private static string PrintKey(ulong key,int keyLenght)
   {

      char[] items = new char[keyLenght];
      for(int i = 0; i < keyLenght; ++i)
      {
         items[keyLenght-i-1]=tochar[key & 0x3];
         key >>= 2;
      }
      return new string(items);
   }

   private ulong CreateKey(byte[] s,int start)
   {
      return CreateKey (s, start, length);
   }

   private static ulong CreateKey(string keyString)
   {
      byte[] byteArray = System.Text.Encoding.ASCII.GetBytes (keyString);
      return CreateKey (byteArray, 0, keyString.Length);
   }


   private Dictionary<ulong, KNucleotideNode> frequencies
   = new Dictionary<ulong, KNucleotideNode>();
   private byte[] sequence;
   int length;
   int frame;

   public KNucleotide(byte[] s, int l, int f)
   {   
      sequence = s; length = l; frame = f;
   }

   public void AddFrequencies(KNucleotide other)
   {
      foreach(var kvp in other.frequencies)            
      {
         KNucleotideNode kNucleotideNode;
         if (frequencies.TryGetValue(kvp.Key, out kNucleotideNode))
            kNucleotideNode.V += kvp.Value.V;
         else
            frequencies[kvp.Key] = kvp.Value;
      }
   }

   public void WriteFrequencies() {
      var items = new List<KeyValuePair<ulong, KNucleotideNode>>(frequencies);
      items.Sort(SortByFrequencyAndCode);    
      double percent = 100.0 / (sequence.Length - length + 1);
      foreach (var item in items)
         Console.WriteLine("{0} {1:f3}",
            PrintKey(item.Key,item.Value.KeyLength), item.Value.V * percent);
      Console.WriteLine();
   }

   public int GetCount(string fragment) {
      KNucleotideNode kNucleotideNode;
      ulong key = CreateKey (fragment);
      if (!frequencies.TryGetValue(key, out kNucleotideNode))
         kNucleotideNode = new KNucleotideNode(0,0);
      return kNucleotideNode.V;
   }

   public void KFrequency() {
      int n = sequence.Length - length + 1;
      for (int i = frame; i < n; i += length) {
         var key = CreateKey(sequence, i, length);
         KNucleotideNode kNucleotideNode;
         if (frequencies.TryGetValue(key, out kNucleotideNode))
            kNucleotideNode.V++;
         else
            frequencies[key] = new KNucleotideNode(1,length);
      }
   }

   int SortByFrequencyAndCode(
      KeyValuePair<ulong, KNucleotideNode> i0,
      KeyValuePair<ulong, KNucleotideNode> i1) {
      int order = i1.Value.V.CompareTo(i0.Value.V);
      if (order != 0) return order;
      return i0.Key.CompareTo(i1.Key);
   }
}
