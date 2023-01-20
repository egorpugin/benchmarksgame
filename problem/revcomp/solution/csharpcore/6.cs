/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Contributed by Peperud
   Modified to reduce memory use by Anthony Lloyd
*/

using System;
using System.IO;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Threading;

class RevCompSequence { public List<byte[]> Pages; public int StartHeader, EndEx
clusive; public Thread ReverseThread; }

public static class revcomp
{
    const int READER_BUFFER_SIZE = 1024 * 1024;
    const byte LF = 10, GT = (byte)ʼ>ʼ, SP = 32;
    static BlockingCollection<byte[]> readQue = new BlockingCollection<byte[]>()
;
    static BlockingCollection<RevCompSequence> writeQue = new BlockingCollection
<RevCompSequence>();
    static byte[] map;

    static int read(Stream stream, byte[] buffer, int offset, int count)
    {
        var bytesRead = stream.Read(buffer, offset, count);
        return bytesRead==count ? offset+count
             : bytesRead==0 ? offset
             : read(stream, buffer, offset+bytesRead, count-bytesRead);
    }
    static void Reader()
    {
        using (var stream = Console.OpenStandardInput())
        {
            int bytesRead;
            do
            {
                var buffer = new byte[READER_BUFFER_SIZE];
                bytesRead = read(stream, buffer, 0, READER_BUFFER_SIZE);
                readQue.Add(buffer);
            } while(bytesRead==READER_BUFFER_SIZE);
            readQue.CompleteAdding();
        }
    }

    static bool tryTake<T>(BlockingCollection<T> q, out T t) where T : class
    {
        t = null;
        while(!q.IsCompleted && !q.TryTake(out t)) Thread.SpinWait(0);
        return t!=null;
    }

    static void Grouper()
    {
        // Set up complements map
        map = new byte[256];
        for (byte b=0; b<255; b++) map[b]=b;
        map[(byte)ʼAʼ] = (byte)ʼTʼ;
        map[(byte)ʼBʼ] = (byte)ʼVʼ;
        map[(byte)ʼCʼ] = (byte)ʼGʼ;
        map[(byte)ʼDʼ] = (byte)ʼHʼ;
        map[(byte)ʼGʼ] = (byte)ʼCʼ;
        map[(byte)ʼHʼ] = (byte)ʼDʼ;
        map[(byte)ʼKʼ] = (byte)ʼMʼ;
        map[(byte)ʼMʼ] = (byte)ʼKʼ;
        map[(byte)ʼRʼ] = (byte)ʼYʼ;
        map[(byte)ʼTʼ] = (byte)ʼAʼ;
        map[(byte)ʼVʼ] = (byte)ʼBʼ;
        map[(byte)ʼYʼ] = (byte)ʼRʼ;
        map[(byte)ʼaʼ] = (byte)ʼTʼ;
        map[(byte)ʼbʼ] = (byte)ʼVʼ;
        map[(byte)ʼcʼ] = (byte)ʼGʼ;
        map[(byte)ʼdʼ] = (byte)ʼHʼ;
        map[(byte)ʼgʼ] = (byte)ʼCʼ;
        map[(byte)ʼhʼ] = (byte)ʼDʼ;
        map[(byte)ʼkʼ] = (byte)ʼMʼ;
        map[(byte)ʼmʼ] = (byte)ʼKʼ;
        map[(byte)ʼrʼ] = (byte)ʼYʼ;
        map[(byte)ʼtʼ] = (byte)ʼAʼ;
        map[(byte)ʼvʼ] = (byte)ʼBʼ;
        map[(byte)ʼyʼ] = (byte)ʼRʼ;

        var startHeader = 0;
        var i = 0;
        bool afterFirst = false;
        var data = new List<byte[]>();
        byte[] bytes;
        while (tryTake(readQue, out bytes))
        {
            data.Add(bytes);
            while((i=Array.IndexOf<byte>(bytes, GT, i+1))!=-1)
            {
                var sequence = new RevCompSequence { Pages = data
                    , StartHeader = startHeader, EndExclusive = i };
                if(afterFirst)
                    (sequence.ReverseThread = new Thread(() => Reverse(sequence)
)).Start();
                else
                    afterFirst = true;
                writeQue.Add(sequence);
                startHeader = i;
                data = new List<byte[]> { bytes };
            }
        }
        i = Array.IndexOf<byte>(data[data.Count-1],0,0);
        var lastSequence = new RevCompSequence { Pages = data
            , StartHeader = startHeader, EndExclusive = i==-1 ? data[data.Count-
1].Length : i };
        Reverse(lastSequence);
        writeQue.Add(lastSequence);
        writeQue.CompleteAdding();
    }

    static void Reverse(RevCompSequence sequence)
    {
        var startPageId = 0;
        var startBytes = sequence.Pages[0];
        var startIndex = sequence.StartHeader;

        // Skip header line
        while((startIndex=Array.IndexOf<byte>(startBytes, LF, startIndex))==-1)
        {
            startBytes = sequence.Pages[++startPageId];
            startIndex = 0;
        }

        var endPageId = sequence.Pages.Count - 1;
        var endIndex = sequence.EndExclusive - 1;
        if(endIndex==-1) endIndex = sequence.Pages[--endPageId].Length-1;
        var endBytes = sequence.Pages[endPageId];

        // Swap in place across pages
        do
        {
            var startByte = startBytes[startIndex];
            if(startByte<SP)
            {
                if (++startIndex == startBytes.Length)
                {
                    startBytes = sequence.Pages[++startPageId];
                    startIndex = 0;
                }
                if (startIndex == endIndex && startPageId == endPageId) break;
                startByte = startBytes[startIndex];
            }
            var endByte = endBytes[endIndex];
            if(endByte<SP)
            {
                if (--endIndex == -1)
                {
                    endBytes = sequence.Pages[--endPageId];
                    endIndex = endBytes.Length - 1;
                }
                if (startIndex == endIndex && startPageId == endPageId) break;
                endByte = endBytes[endIndex];
            }

            startBytes[startIndex] = map[endByte];
            endBytes[endIndex] = map[startByte];

            if (++startIndex == startBytes.Length)
            {
                startBytes = sequence.Pages[++startPageId];
                startIndex = 0;
            }
            if (--endIndex == -1)
            {
                endBytes = sequence.Pages[--endPageId];
                endIndex = endBytes.Length - 1;
            }
        } while (startPageId < endPageId || (startPageId == endPageId && startIn
dex < endIndex));
        if (startIndex == endIndex) startBytes[startIndex] = map[startBytes[star
tIndex]];
    }

    static void Writer()
    {
        using (var stream = Console.OpenStandardOutput())
        {
            bool first = true;
            RevCompSequence sequence;
            while (tryTake(writeQue, out sequence))
            {
                var startIndex = sequence.StartHeader;
                var pages = sequence.Pages;
                if(first)
                {
                    Reverse(sequence);
                    first = false;
                }
                else
                {
                    sequence.ReverseThread?.Join();
                }
                for (int i = 0; i < pages.Count - 1; i++)
                {
                    var bytes = pages[i];
                    stream.Write(bytes, startIndex, bytes.Length - startIndex);
                    startIndex = 0;
                }
                stream.Write(pages[pages.Count-1], startIndex, sequence.EndExclu
sive - startIndex);
            }
        }
    }

    public static void Main(string[] args)
    {
        new Thread(Reader).Start();
        new Thread(Grouper).Start();
        Writer();
    }
}

