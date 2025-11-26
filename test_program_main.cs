using System;

public class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("Hello from Program.Main!");
        if (args.Length > 0)
        {
            Console.WriteLine($"First argument: {args[0]}");
        }
        else
        {
            Console.WriteLine("No arguments provided");
        }
    }
}
