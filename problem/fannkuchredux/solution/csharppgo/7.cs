/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Isaac Gouy, transliterated from Oleg Mazurovʼs Java program
   concurrency fix and minor improvements by Peperud
   parallel and small optimisations by Anthony Lloyd
   15% improvement by optimizing inner loop and compiler options by Thom Kiesewe
tter
*/

using System.Numerics;
using System.Runtime.CompilerServices;

public static class FannkuchRedux
{
    private static int taskCount;
    private static int[] fact, chkSums, maxFlips;

    private static void FirstPermutation(int[] p, int[] pp, int[] count, int idx
, int n)
    {
        for (var i = 0; i < n; ++i) p[i] = i;
        for (var i = count.Length - 1; i > 0; --i)
        {
            var d = idx / fact[i];
            count[i] = d;
            if (d > 0)
            {
                idx %= fact[i];
                new Vector<int>(p).CopyTo(pp);
                new Vector<int>(p, 4).CopyTo(pp, 4);
                for (var j = 0; j <= i; ++j) p[j] = pp[(j + d) % (i + 1)];
            }
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static int NextPermutation(int[] p, int[] count)
    {
        var first = p[1];
        p[1] = p[0];
        p[0] = first;
        var i = 1;
        while (++count[i] > i)
        {
            count[i++] = 0;
            var next = p[1];
            p[0] = next;
            for (var j = 1; j < i;) p[j] = p[++j];
            p[i] = first;
            first = next;
        }

        return first;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static int CountFlips(int first, int[] p, int[] pp)
    {
        if (first == 0) return 0;
        if (p[first] == 0) return 1;
        new Vector<int>(p).CopyTo(pp);
        new Vector<int>(p, 4).CopyTo(pp, 4);
        var flips = 1;
        var tp = pp[first];
        while (tp != 0)
        {
            var hi = first - 1;
            pp[first] = first;
            first = tp;
            for (var lo = 1; lo < hi; lo++, hi--)
            {
                var t = pp[lo];
                pp[lo] = pp[hi];
                pp[hi] = t;
            }

            tp = pp[first];
            flips++;
        }

        return flips;
    }

    private static void Run(int n, int taskSize)
    {
        int[] p = new int[16], pp = new int[16];
        var count = new int[n];
        int taskId, chkSum = 0, maxFlip = 0;
        while ((taskId = Interlocked.Decrement(ref taskCount)) >= 0)
        {
            FirstPermutation(p, pp, count, taskId * taskSize, n);
            var flips = CountFlips(p[0], p, pp);
            chkSum += flips;
            if (flips > maxFlip) maxFlip = flips;
            for (var i = 1; i < taskSize; i++)
            {
                flips = CountFlips(NextPermutation(p, count), p, pp);
                chkSum += (1 - i % 2 * 2) * flips;
                if (flips > maxFlip) maxFlip = flips;
            }
        }

        chkSums[-taskId - 1] = chkSum;
        maxFlips[-taskId - 1] = maxFlip;
    }

    public static void Main(string[] args)
    {
        var n = args.Length > 0 ? int.Parse(args[0]) : 7;
        fact = new int[n + 1];
        fact[0] = 1;
        var factN = 1;
        for (var i = 1; i < fact.Length; i++) fact[i] = factN *= i;

        taskCount = n > 9
            ? factN / (7 * 6 * 5 * 4 * 3 * 2)
            : Environment.ProcessorCount;
        var taskSize = factN / taskCount;
        var nThreads = Environment.ProcessorCount;
        chkSums = new int[nThreads];
        maxFlips = new int[nThreads];
        var threads = new Thread[nThreads];
        for (var i = 1; i < nThreads; i++)
            (threads[i] = new Thread(() => Run(n, taskSize))).Start();
        Run(n, taskSize);
        int chkSum = chkSums[0], maxFlip = maxFlips[0];
        for (var i = 1; i < threads.Length; i++)
        {
            threads[i].Join();
            chkSum += chkSums[i];
            if (maxFlips[i] > maxFlip) maxFlip = maxFlips[i];
        }

        Console.Out.WriteLineAsync(chkSum + "\nPfannkuchen(" + n + ") = " + maxF
lip);
    }
}

