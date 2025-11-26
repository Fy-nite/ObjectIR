using System;
using System.Collections.Generic;

namespace ObjectIR.Examples;

/// <summary>
/// Compact sample that exercises common language features the ObjectIR C# frontend supports.
/// Start with the HelloWorld path in <see cref="Main"/> and extend the helpers to validate
/// more syntax (loops, conditionals, arrays, simple types).
/// </summary>
public static class FeatureWalkthrough
{
    public static void Main()
    {
        Console.WriteLine("=== ObjectIR walkthrough ===");

        var greeting = BuildGreeting("ObjectIR");
        Console.WriteLine(greeting);

        var sequence = BuildSequence(5);
        var runningTotal = Sum(sequence);
        Console.WriteLine($"Sum: {runningTotal}");

        var countdown = Countdown(3);
        Console.WriteLine("Countdown: " + string.Join(", ", countdown));

        var stats = DescribeSample(sequence);
        Console.WriteLine($"Min={stats.Min}, Max={stats.Max}, Avg={stats.Average}");
    }

    private static string BuildGreeting(string name)
    {
        if (string.IsNullOrWhiteSpace(name))
        {
            return "Hello, world!";
        }

        return $"Hello, {name.Trim()}!";
    }

    private static int[] BuildSequence(int count)
    {
        if (count < 0)
            throw new ArgumentOutOfRangeException(nameof(count));

        var values = new int[count];
        for (var i = 0; i < count; i++)
        {
            values[i] = i + 1;
        }

        return values;
    }

    private static int Sum(int[] values)
    {
        var total = 0;
        for (var i = 0; i < values.Length; i++)
        {
            total += values[i];
        }

        return total;
    }

    private static IList<int> Countdown(int start)
    {
        var parts = new List<int>();
        var current = start;

        while (current >= 0)
        {
            parts.Add(current);
            current--;
        }

        return parts;
    }

    private static SampleStats DescribeSample(int[] values)
    {
        if (values.Length == 0)
            return new SampleStats(0, 0, 0);

        var min = values[0];
        var max = values[0];
        var total = 0;

        for (var i = 0; i < values.Length; i++)
        {
            var value = values[i];
            if (value < min)
                min = value;
            if (value > max)
                max = value;
            total += value;
        }

        var avg = total / values.Length;
        return new SampleStats(min, max, avg);
    }

    private readonly record struct SampleStats(int Min, int Max, int Average);
}
