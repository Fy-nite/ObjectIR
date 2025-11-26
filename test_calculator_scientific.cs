using System;

namespace Calculator
{
    /// <summary>
    /// Advanced calculator with scientific operations
    /// </summary>
    public class ScientificCalculator
    {
        public double Square(double x) => x * x;
        
        public double Cube(double x) => x * x * x;
        
        public double Power(double x, double exp)
        {
            return Math.Pow(x, exp);
        }
        
        public double SquareRoot(double x)
        {
            if (x < 0)
                throw new ArgumentException("Cannot compute square root of negative number");
            return Math.Sqrt(x);
        }
    }
}
