/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Robert F. Tobler to process large blocks of byte arrays

   modified by Peperud to add concurrency
*/

using System;
using System.IO;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Threading.Tasks;

static class revcomp
{
    struct Block
    {
        public byte[] Data;
        public int Count;

        public int Read()
        {
            try
            {
                Data = inputBufferQue.Take();
                return Data.Length;
            }
            catch (InvalidOperationException)
            {
                Data = null;
                return 0;
            }
            finally
            {
                Count++;
            }
        }

        public Index IndexOf(byte b, int o)
        {
            return new Index
            {
                Block = Count,
                Pos = Array.IndexOf(Data, b, o)
            };
        }
    }

    struct Index
    {
        public int Block;
        public int Pos;
        public static readonly Index None = new Index
        {
            Block = -1,
            Pos = -1
        };
        public bool InBlock(Block b)
        {
            return Block == b.Count;
        }
    }

    const byte Gt = (byte)'>';
    const byte Lf = (byte)'\n';

    static int READER_BUFFER_SIZE = 1024 * 1024 * 8;

    static BlockingCollection<WorkItem> workQue = new BlockingCollection<WorkItem>();
    static BlockingCollection<byte[]> inputBufferQue = new BlockingCollection<byte[]>(5);

    static void Main(string[] args)
    {
        Task.Run(() => Reader());
     
        InitComplements();
        var seq = new List<byte[]>();
        var b = new Block
        {
            Count = -1
        };
        Index
            line = Index.None,
            start = Index.None,
            end = Index.None;

        var worker = Task.Run(() => Worker());

        while (b.Read() > 0)
        {
            seq.Add(b.Data);
            if (line.Pos < 0) line = b.IndexOf(Gt, 0);
            while (line.Pos >= 0)
            {
                if (start.Pos < 0)
                {
                    var off = line.InBlock(b) ? line.Pos : 0;
                    start = b.IndexOf(Lf, off);
                    if (start.Pos < 0)
                    {
                        WorkItem.SendWriteJob(b.Data, off, b.Data.Length - off);
                        seq.Clear();
                        break;
                    }
                    WorkItem.SendWriteJob(b.Data, off, start.Pos + 1 - off);
                }
                if (end.Pos < 0)
                {
                    end = b.IndexOf(Gt, start.InBlock(b) ? start.Pos : 0);
                    if (end.Pos < 0)
                    {
                        break;
                    }
                }
                WorkItem.SendReverseJob(start.Pos, end.Pos, seq);
                if (seq.Count > 1)
                {
                    seq.RemoveRange(0, seq.Count - 1);
                }
                line = end;
                end = Index.None;
                start = Index.None;
            }
        }
        if (start.Pos >= 0 && end.Pos < 0)
            WorkItem.SendReverseJob(start.Pos, seq[seq.Count - 1].Length, seq);

        workQue.CompleteAdding();

        worker.Wait();
    }

    const string Seq = "ABCDGHKMRTVYabcdghkmrtvy";
    const string Rev = "TVGHCDMKYABRTVGHCDMKYABR";
    static byte[] comp = new byte[256];

    static void InitComplements()
    {
        for (byte i = 0; i < 255; i++) comp[i] = i;
        for (int i = 0; i < Seq.Length; i++)
            comp[(byte)Seq[i]] = (byte)Rev[i];
        comp[Lf] = 0; comp[(byte)' '] = 0;
    }

    const int LineLen = 61;
    const int BufSize = LineLen * 269;
    static byte[] buf = new byte[BufSize];

    static void Reverse(this Stream w, int si, int ei, List<byte[]> bl)
    {
        int bi = 0, line = LineLen - 1;
        for (int ri = bl.Count - 1; ri >= 0; ri--)
        {
            var b = bl[ri]; int off = ri == 0 ? si : 0;
            for (int i = (ri == bl.Count - 1 ? ei : b.Length) - 1; i >= off; i--)
            {
                var c = comp[b[i]]; if (c > 0) buf[bi++] = c;
                if (bi == line)
                {
                    buf[bi++] = Lf; line += LineLen;
                    if (bi == BufSize)
                    {
                        w.Write(buf, 0, BufSize); bi = 0; line = LineLen - 1;
                    }
                }
            }
        }
        if (bi > 0)
        {
            if (buf[bi - 1] != Lf) buf[bi++] = Lf; w.Write(buf, 0, bi);
        }
    }


    struct ReverseJob
    {
        public List<byte[]> listBuffer;
        public int start;
        public int end;
    }

    class WorkItem
    {
        public byte[] WJob;
        public ReverseJob RJob;

        public static void SendWriteJob(byte[] buffer, int offset, int count)
        {
            var tmp = new byte[count];
            Buffer.BlockCopy(buffer, offset, tmp, 0, count);
            workQue.Add(new WorkItem { WJob = tmp });
        }

        public static void SendReverseJob(int si, int ei, List<byte[]> bl)
        {
            var tmp = new List<byte[]>(bl.Count);
            byte[] item;
            for (var i = 0; i < bl.Count; i++)
            {
                if (bl[i] != null)
                {
                    item = new byte[bl[i].Length];
                    Buffer.BlockCopy(bl[i], 0, item, 0, item.Length);
                }
                else
                {
                    item = null;
                }
                tmp.Add(item);
            }

            workQue.Add(new WorkItem { RJob = new ReverseJob { listBuffer = tmp, start = si, end = ei } });
        }
    }

    static void Worker()
    {
        try
        {
            using (var w = Console.OpenStandardOutput())
            {
                while (!workQue.IsCompleted)
                {
                    var job = workQue.Take();

                    if (job.WJob != null)
                    {
                        w.Write(job.WJob, 0, job.WJob.Length);
                    }
                    else
                    {
                        Reverse(w, job.RJob.start, job.RJob.end, job.RJob.listBuffer);
                    }
                }
            }
        }
        catch (InvalidOperationException) { }
    }

    static void Reader()
    {
        using (var rdr = new BinaryReader(Console.OpenStandardInput()))
        {
            byte[] buf;

            do
            {
                buf = rdr.ReadBytes(READER_BUFFER_SIZE);
                if (buf != null)
                {
                    inputBufferQue.Add(buf);
                }
            } while (buf != null);

            inputBufferQue.CompleteAdding();
        }
    }
}
