using System;
using System.Collections.Generic;

public class Calculator
{
    private int _result;
    private List<string> _history;

    public Calculator()
    {
        _result = 0;
        _history = new List<string>();
    }

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
        double result = a / b;
        _result = (int)result;
        _history.Add($"Divide({a}, {b}) = {result}");
        return result;
    }

    public int GetResult()
    {
        return _result;
    }

    public void ClearHistory()
    {
        _history.Clear();
    }
}

public interface IMathOperations
{
    int Add(int a, int b);
    int Subtract(int a, int b);
}

public enum Operation
{
    Add,
    Subtract,
    Multiply,
    Divide
}
