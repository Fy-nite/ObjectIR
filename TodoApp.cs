using System;
using System.Collections.Generic;
using System.Linq;

namespace TodoApp;
public interface IItem
{
    int GetId();

    string GetDescription();

    bool IsComplete();
}

public class TodoItem : TodoApp.IItem
{
    private int id;
    private string description;
    private bool isComplete;

    public  TodoItem(int id, string description);

    public int GetId();

    public string GetDescription();

    public bool IsComplete();

    public void MarkComplete();
}

public class TodoList
{
    private System.List<TodoApp.TodoItem> items;
    private int nextId;

    public  TodoList();

    public TodoApp.TodoItem Add(string description);
}

public class Program
{
    public static void Main();
}

