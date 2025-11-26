using System;
using System.Collections.Generic;

namespace Calculator
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Calculator calc = new Calculator();
            Console.WriteLine("Addition: " + calc.Add(5, 3));
            Console.WriteLine("Subtraction: " + calc.Subtract(10, 4));
            Console.WriteLine("Multiplication: " + calc.Multiply(7, 6));
            Console.WriteLine("Division: " + calc.Divide(20, 5));
        }
    }
    /// <summary>
    /// Basic calculator operations
    /// </summary>
    public class Calculator
    {
        private int _result = 0;
        private List<string> _history = new();

        public int Add(int a, int b)
        {
            _result = a + b;
            _history.Add($"Add({a}, {b}) = {_result}");
            return _result;
        }

        public int Subtract(int a, int b)
        {
            _result = a - b;
            _history.Add($"Subtract({a}, {b}) = {_result}");
            return _result;
        }

        public int Multiply(int a, int b)
        {
            _result = a * b;
            _history.Add($"Multiply({a}, {b}) = {_result}");
            return _result;
        }

        public double Divide(double a, double b)
        {
            if (b == 0)
                throw new ArgumentException("Cannot divide by zero");
            
            var result = a / b;
            _history.Add($"Divide({a}, {b}) = {result}");
            return result;
        }

        public int GetResult() => _result;
        
        public void ClearHistory() => _history.Clear();
    }

    /// <summary>
    /// Interface for mathematical operations
    /// </summary>
    public interface IMathOperations
    {
        int Execute(int a, int b);
        string GetName();
    }

    /// <summary>
    /// Represents a basic arithmetic operation
    /// </summary>
    public abstract class Operation : IMathOperations
    {
        protected string _name = "Operation";
        
        public abstract int Execute(int a, int b);
        
        public string GetName() => _name;
    }
}
