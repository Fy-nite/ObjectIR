using System;

public class ScientificCalculator
{
    public double Power(double x, double y)
    {
        return Math.Pow(x, y);
    }

    public double SquareRoot(double x)
    {
        return Math.Sqrt(x);
    }

    public double Sine(double angle)
    {
        return Math.Sin(angle);
    }

    public double Cosine(double angle)
    {
        return Math.Cos(angle);
    }
}

public struct ComplexNumber
{
    public double Real { get; set; }
    public double Imaginary { get; set; }
}
