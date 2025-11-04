using System;
using System.Collections.Generic;
using System.Linq;

namespace SampleModule;
public class Greeter
{
    public string Greet(string name)
    {
        return;
    }
}

public class Person
{
    private string _name;

    public int Age { get; set; }

    public  Person(string name)
    {
        _name = name;
    }
}

public interface ILogger
{
    void Log(string message);
}

