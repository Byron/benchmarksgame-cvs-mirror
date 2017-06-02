/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/

 * contributed by Baolin Qu
 */


import java.util.Scanner;

public class revcomp
{
    private static final byte[] differntial = new byte[128];
    
    private static final String lineSeparator = System.getProperty("line.separator");
    
    private static boolean firstTime = true;
    
    static
    {
        char[] c1 = new char[] { 'A', 'C', 'G', 'T', 'U', 'M', 'R', 'W', 'S', 'Y', 'K', 'V', 'H', 'D', 'B', 'N' };
        char[] c2 = new char[] { 'T', 'G', 'C', 'A', 'A', 'K', 'Y', 'W', 'S', 'R', 'M', 'B', 'D', 'H', 'V', 'N' };
        for (int i = 0; i < c1.length; i++)
        {
            differntial[c1[i]] = (byte) ((c2[i] - c1[i]));
            differntial[(c1[i] - 'A') + 'a'] = (byte) ((c2[i] - ((c1[i] - 'A') + 'a')));
        }
    }
    
    public static void main(String[] args)
    {
        StringBuilder sb = new StringBuilder(90 * 60);
        StringBuilder cache = new StringBuilder(90 * 61);
        String head = null;
        try (Scanner scanner = new Scanner(System.in))
        {
            while (scanner.hasNext())
            {
                String line = scanner.nextLine();
                if (line.charAt(0) == '>')
                {
                    write(head, sb, cache);
                    head = line;
                    continue;
                }
                char[] array = line.toCharArray();
                for (int i = 0; i < array.length; i++)
                {
                    array[i] = (char) ((array[i] + differntial[array[i]]));
                }
                sb.append(array);
            }
        }
        finally
        {
            write(head, sb, cache);
            System.out.flush();
        }
        
    }
    
    private static final void write(String head, StringBuilder complement, StringBuilder cache)
    {
        if (head == null)
        {
            return;
        }
        cache.delete(0, cache.length());
        int length = complement.length();
        if (!firstTime)
        {
            cache.append(lineSeparator);
        }
        cache.append(head).append(lineSeparator);
        for (int i = 1; i <= length; i++)
        {
            cache.append(complement.charAt(length - i));
            if (((i % 60) == 0) && (i != length))
            {
                cache.append(lineSeparator);
            }
        }
        System.out.append(cache, 0, cache.length());
        complement.delete(0, complement.length());
        firstTime = false;
    }
    
}
