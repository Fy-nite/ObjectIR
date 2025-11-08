# ObjectIR C++ Runtime: Standard Library Specification

This document outlines the comprehensive standard library features planned for the ObjectIR C++ Runtime. These features are designed to support Fortran programs compiled to ObjectIR, providing essential functionality for I/O, mathematics, collections, file operations, and more.

## Table of Contents

1. [Core Types and Utilities](#core-types-and-utilities)
2. [Console and I/O](#console-and-io)
3. [Mathematics](#mathematics)
4. [String Operations](#string-operations)
5. [Collections](#collections)
6. [File System](#file-system)
7. [Date and Time](#date-and-time)
8. [Random Numbers](#random-numbers)
9. [Environment](#environment)
10. [Exception Handling](#exception-handling)
11. [Arrays](#arrays)
12. [Path Operations](#path-operations)

---

## Core Types and Utilities

### System.Object

- Base class for all objects
- `ToString()` - Returns string representation
- `GetType()` - Returns type information
- `Equals(Object)` - Equality comparison
- `GetHashCode()` - Hash code generation

### System.ValueType

- Base class for value types
- Inherits from `System.Object`

### System.Enum

- Base class for enumerations
- `Parse(string)` - Parse string to enum value
- `ToString()` - Convert enum to string

### System.Exception

- Base exception class
- `Message` - Error message
- `InnerException` - Nested exception
- `StackTrace` - Call stack information

---

## Console and I/O

### System.Console (Extended)

**Current Implementation:** Basic WriteLine/Write/ReadLine

**Additional Methods:**

- `WriteLine(object)` - Overloaded for all types
- `Write(object)` - Overloaded for all types
- `Read()` - Read single character
- `ReadKey()` - Read key with/without echo
- `Clear()` - Clear console
- `SetCursorPosition(int, int)` - Set cursor position
- `GetCursorPosition()` - Get current cursor position
- `ForegroundColor` - Get/set text color
- `BackgroundColor` - Get/set background color
- `ResetColor()` - Reset colors to default
- `Beep()` - Produce beep sound
- `Beep(int, int)` - Beep with frequency and duration

### System.IO.Stream

- Abstract base class for streams
- `Read(byte[], int, int)` - Read bytes into buffer
- `Write(byte[], int, int)` - Write bytes from buffer
- `Seek(long, SeekOrigin)` - Seek to position
- `Position` - Current position
- `Length` - Stream length
- `Flush()` - Flush buffered data
- `Close()` - Close stream

### System.IO.FileStream

- File-based stream implementation
- Constructor: `FileStream(string path, FileMode)`
- Constructor: `FileStream(string path, FileMode, FileAccess)`

### System.IO.StreamReader

- Text reader for streams
- Constructor: `StreamReader(Stream)`
- Constructor: `StreamReader(string path)`
- `ReadLine()` - Read line of text
- `ReadToEnd()` - Read all text
- `Read()` - Read single character
- `Peek()` - Peek at next character
- `Close()` - Close reader

### System.IO.StreamWriter

- Text writer for streams
- Constructor: `StreamWriter(Stream)`
- Constructor: `StreamWriter(string path)`
- `Write(string)` - Write string
- `WriteLine(string)` - Write line
- `Flush()` - Flush buffer
- `Close()` - Close writer

---

## Mathematics

### System.Math
**Methods to implement:**
- **Constants:**
  - `PI` - π (3.141592653589793)
  - `E` - e (2.718281828459045)
  - `Tau` - τ (2π)

- **Trigonometric:**
  - `Sin(double)` - Sine
  - `Cos(double)` - Cosine
  - `Tan(double)` - Tangent
  - `Asin(double)` - Arc sine
  - `Acos(double)` - Arc cosine
  - `Atan(double)` - Arc tangent
  - `Atan2(double, double)` - Arc tangent of y/x

- **Hyperbolic:**
  - `Sinh(double)` - Hyperbolic sine
  - `Cosh(double)` - Hyperbolic cosine
  - `Tanh(double)` - Hyperbolic tangent

- **Exponential and Logarithmic:**
  - `Exp(double)` - e^x
  - `Log(double)` - Natural logarithm (base e)
  - `Log10(double)` - Common logarithm (base 10)
  - `Log(double, double)` - Logarithm with specified base
  - `Pow(double, double)` - Power function (x^y)
  - `Sqrt(double)` - Square root

- **Rounding:**
  - `Ceiling(double)` - Ceiling (round up)
  - `Floor(double)` - Floor (round down)
  - `Round(double)` - Round to nearest integer
  - `Round(double, int)` - Round to specified decimal places
  - `Truncate(double)` - Truncate decimal part

- **Sign and Absolute:**
  - `Abs(double)` - Absolute value (overloaded for int, float)
  - `Sign(double)` - Sign function (-1, 0, 1)

- **Min/Max:**
  - `Min(double, double)` - Minimum (overloaded for all numeric types)
  - `Max(double, double)` - Maximum (overloaded for all numeric types)

- **Special Functions:**
  - `IEEERemainder(double, double)` - IEEE remainder

---

## String Operations

### System.String (Extended)
**Current Implementation:** Concat, IsNullOrEmpty, Length, Substring

**Additional Methods:**
- **Comparison:**
  - `Equals(string, string)` - Case-sensitive equality
  - `Equals(string, string, StringComparison)` - Equality with comparison options
  - `Compare(string, string)` - Lexicographic comparison
  - `CompareOrdinal(string, string)` - Ordinal comparison

- **Searching:**
  - `IndexOf(char/string)` - Find first occurrence
  - `LastIndexOf(char/string)` - Find last occurrence
  - `Contains(string)` - Check if contains substring
  - `StartsWith(string)` - Check if starts with
  - `EndsWith(string)` - Check if ends with

- **Modification:**
  - `Insert(int, string)` - Insert substring
  - `Remove(int, int)` - Remove substring
  - `Replace(string, string)` - Replace occurrences
  - `Replace(char, char)` - Replace characters
  - `ToUpper()` - Convert to uppercase
  - `ToLower()` - Convert to lowercase
  - `Trim()` - Remove whitespace
  - `Trim(char[])` - Remove specified characters
  - `TrimStart()` / `TrimEnd()` - Trim from start/end
  - `PadLeft(int)` / `PadRight(int)` - Pad with spaces
  - `PadLeft(int, char)` / `PadRight(int, char)` - Pad with character

- **Splitting/Joining:**
  - `Split(char[])` - Split by characters
  - `Split(string[])` - Split by strings
  - `Join(string, string[])` - Join array with separator
  - `Join(string, IEnumerable<string>)` - Join collection

- **Formatting:**
  - `Format(string, params object[])` - String formatting
  - `ToCharArray()` - Convert to character array

---

## Collections

### System.Collections.Generic.List<T>
**Current Implementation:** Basic list operations

**Additional Methods:**
- `AddRange(IEnumerable<T>)` - Add collection of items
- `InsertRange(int, IEnumerable<T>)` - Insert collection at index
- `RemoveRange(int, int)` - Remove range of items
- `Find(Predicate<T>)` - Find first matching item
- `FindAll(Predicate<T>)` - Find all matching items
- `FindIndex(Predicate<T>)` - Find index of first match
- `FindLast(Predicate<T>)` - Find last matching item
- `Exists(Predicate<T>)` - Check if any item matches
- `TrueForAll(Predicate<T>)` - Check if all items match
- `Sort()` - Sort list
- `Sort(Comparison<T>)` - Sort with comparison
- `Sort(IComparer<T>)` - Sort with comparer
- `BinarySearch(T)` - Binary search
- `Reverse()` - Reverse order
- `AsReadOnly()` - Get read-only wrapper

### System.Collections.Generic.Dictionary<TKey, TValue>
**Methods to implement:**
- Constructor: `Dictionary<TKey, TValue>()`
- Constructor: `Dictionary<TKey, TValue>(int capacity)`
- `Add(TKey, TValue)` - Add key-value pair
- `Remove(TKey)` - Remove by key
- `ContainsKey(TKey)` - Check if key exists
- `ContainsValue(TValue)` - Check if value exists
- `TryGetValue(TKey, out TValue)` - Try to get value
- `Keys` - Get key collection
- `Values` - Get value collection
- `Count` - Number of items
- `Clear()` - Remove all items
- `[TKey]` - Indexer for get/set

### System.Collections.Generic.Queue<T>
**Methods to implement:**
- `Enqueue(T)` - Add item to end
- `Dequeue()` - Remove and return first item
- `Peek()` - Return first item without removing
- `Count` - Number of items
- `Clear()` - Remove all items
- `Contains(T)` - Check if contains item

### System.Collections.Generic.Stack<T>
**Methods to implement:**
- `Push(T)` - Add item to top
- `Pop()` - Remove and return top item
- `Peek()` - Return top item without removing
- `Count` - Number of items
- `Clear()` - Remove all items
- `Contains(T)` - Check if contains item

### System.Collections.Generic.HashSet<T>
**Methods to implement:**
- `Add(T)` - Add item
- `Remove(T)` - Remove item
- `Contains(T)` - Check if contains item
- `Count` - Number of items
- `Clear()` - Remove all items
- `UnionWith(IEnumerable<T>)` - Union operation
- `IntersectWith(IEnumerable<T>)` - Intersection operation
- `ExceptWith(IEnumerable<T>)` - Set difference

---

## File System

### System.IO.File
**Methods to implement:**
- `Exists(string)` - Check if file exists
- `Delete(string)` - Delete file
- `Move(string, string)` - Move/rename file
- `Copy(string, string)` - Copy file
- `ReadAllText(string)` - Read entire file as string
- `WriteAllText(string, string)` - Write string to file
- `ReadAllLines(string)` - Read all lines as string array
- `WriteAllLines(string, string[])` - Write lines to file
- `ReadAllBytes(string)` - Read file as byte array
- `WriteAllBytes(string, byte[])` - Write bytes to file
- `AppendAllText(string, string)` - Append text to file
- `GetCreationTime(string)` - Get creation time
- `GetLastWriteTime(string)` - Get last write time
- `GetLastAccessTime(string)` - Get last access time

### System.IO.Directory
**Methods to implement:**
- `Exists(string)` - Check if directory exists
- `CreateDirectory(string)` - Create directory
- `Delete(string)` - Delete directory
- `Delete(string, bool)` - Delete directory recursively
- `GetFiles(string)` - Get files in directory
- `GetDirectories(string)` - Get subdirectories
- `GetFileSystemEntries(string)` - Get all entries
- `Move(string, string)` - Move directory
- `GetCurrentDirectory()` - Get current working directory
- `SetCurrentDirectory(string)` - Set current working directory

### System.IO.Path
**Methods to implement:**
- `Combine(string, string)` - Combine path segments
- `GetDirectoryName(string)` - Get directory portion
- `GetFileName(string)` - Get filename portion
- `GetFileNameWithoutExtension(string)` - Get filename without extension
- `GetExtension(string)` - Get file extension
- `GetFullPath(string)` - Get absolute path
- `GetPathRoot(string)` - Get root directory
- `HasExtension(string)` - Check if has extension
- `IsPathRooted(string)` - Check if absolute path
- `ChangeExtension(string, string)` - Change extension

---

## Date and Time

### System.DateTime
**Properties:**
- `Now` - Current local date/time
- `UtcNow` - Current UTC date/time
- `Today` - Current date (time 00:00:00)
- `Year`, `Month`, `Day` - Date components
- `Hour`, `Minute`, `Second`, `Millisecond` - Time components
- `DayOfWeek` - Day of week
- `DayOfYear` - Day of year

**Methods:**
- Constructor: `DateTime(int year, int month, int day)`
- Constructor: `DateTime(int year, int month, int day, int hour, int minute, int second)`
- `AddYears(int)` - Add years
- `AddMonths(int)` - Add months
- `AddDays(double)` - Add days
- `AddHours(double)` - Add hours
- `AddMinutes(double)` - Add minutes
- `AddSeconds(double)` - Add seconds
- `AddMilliseconds(double)` - Add milliseconds
- `ToString()` - Format as string
- `ToString(string)` - Format with custom format
- `Parse(string)` - Parse from string
- `TryParse(string, out DateTime)` - Try to parse
- `Compare(DateTime, DateTime)` - Compare two dates
- `Equals(DateTime)` - Equality check

### System.TimeSpan
**Properties:**
- `Days`, `Hours`, `Minutes`, `Seconds`, `Milliseconds` - Time components
- `TotalDays`, `TotalHours`, `TotalMinutes`, `TotalSeconds`, `TotalMilliseconds` - Total time

**Methods:**
- Constructor: `TimeSpan(long ticks)`
- Constructor: `TimeSpan(int hours, int minutes, int seconds)`
- Constructor: `TimeSpan(int days, int hours, int minutes, int seconds)`
- `FromDays(double)` - Create from days
- `FromHours(double)` - Create from hours
- `FromMinutes(double)` - Create from minutes
- `FromSeconds(double)` - Create from seconds
- `FromMilliseconds(double)` - Create from milliseconds
- `Add(TimeSpan)` - Add time spans
- `Subtract(TimeSpan)` - Subtract time spans
- `Compare(TimeSpan, TimeSpan)` - Compare time spans

---

## Random Numbers

### System.Random
**Methods to implement:**
- Constructor: `Random()`
- Constructor: `Random(int seed)`
- `Next()` - Random int (0 to Int32.MaxValue-1)
- `Next(int maxValue)` - Random int (0 to maxValue-1)
- `Next(int minValue, int maxValue)` - Random int in range
- `NextDouble()` - Random double (0.0 to 1.0)
- `NextBytes(byte[])` - Fill array with random bytes

---

## Environment

### System.Environment
**Properties:**
- `CurrentDirectory` - Get/set current directory
- `MachineName` - Computer name
- `UserName` - Current user name
- `OSVersion` - Operating system version
- `Version` - Runtime version
- `ProcessorCount` - Number of processors
- `SystemDirectory` - System directory path
- `NewLine` - Newline string for platform

**Methods:**
- `GetEnvironmentVariable(string)` - Get environment variable
- `SetEnvironmentVariable(string, string)` - Set environment variable
- `GetEnvironmentVariables()` - Get all environment variables
- `GetCommandLineArgs()` - Get command line arguments
- `Exit(int)` - Exit application
- `GetFolderPath(SpecialFolder)` - Get special folder path

---

## Exception Handling

### System.Exception (Extended)
**Additional Properties:**
- `Source` - Source of exception
- `TargetSite` - Method that threw exception
- `HelpLink` - Help link

### Specific Exception Types
- `System.ArgumentException` - Invalid argument
- `System.ArgumentNullException` - Null argument
- `System.ArgumentOutOfRangeException` - Argument out of range
- `System.InvalidOperationException` - Invalid operation
- `System.NotSupportedException` - Operation not supported
- `System.IO.IOException` - I/O error
- `System.IO.FileNotFoundException` - File not found
- `System.IO.DirectoryNotFoundException` - Directory not found
- `System.FormatException` - Invalid format
- `System.OverflowException` - Arithmetic overflow

---

## Arrays

### System.Array
**Methods to implement:**
- `Length` - Get array length
- `Rank` - Get number of dimensions
- `GetLength(int)` - Get length of specific dimension
- `GetLowerBound(int)` - Get lower bound of dimension
- `GetUpperBound(int)` - Get upper bound of dimension
- `Copy(Array, Array, int)` - Copy elements
- `Clear(Array, int, int)` - Clear elements
- `IndexOf(Array, object)` - Find index of element
- `LastIndexOf(Array, object)` - Find last index
- `Reverse(Array)` - Reverse elements
- `Sort(Array)` - Sort elements
- `BinarySearch(Array, object)` - Binary search

---

## Path Operations

### System.IO.Path (Extended)
**Additional Methods:**
- `GetTempPath()` - Get temporary directory
- `GetTempFileName()` - Get temporary filename
- `GetRandomFileName()` - Get random filename
- `GetInvalidPathChars()` - Get invalid path characters
- `GetInvalidFileNameChars()` - Get invalid filename characters
- `IsPathFullyQualified(string)` - Check if fully qualified path

---

## Implementation Notes

### Priority Order
1. **High Priority** (Essential for basic Fortran support):
   - System.Math (basic functions)
   - System.IO.File (basic file operations)
   - System.IO.Directory (basic directory operations)
   - System.IO.Path (path manipulation)
   - Extended System.String operations
   - System.Array operations

2. **Medium Priority** (Enhanced functionality):
   - System.DateTime and System.TimeSpan
   - System.Random
   - System.Environment
   - Collections (Dictionary, Queue, Stack, HashSet)
   - Extended System.Console features

3. **Low Priority** (Advanced features):
   - Full exception hierarchy
   - Advanced I/O streams
   - Complex collections operations

### Implementation Strategy
- Each class should have a corresponding header (.hpp) and implementation (.cpp) file
- Follow the existing pattern in `stdlib.cpp` for native method registration
- Use C++ standard library where possible for implementation
- Provide both uppercase and lowercase class names for compatibility
- Include comprehensive error handling and type checking
- Add unit tests for each new feature

### Dependencies
- Use C++17 features where available
- Leverage STL containers and algorithms
- Platform-specific code should be abstracted for cross-platform compatibility
- Consider thread safety for shared resources</content>
<parameter name="filePath">/run/media/charlie/the cat storage v2/ObjectIR/src/ObjectIR.CppRuntime/STDLIB_SPEC.md