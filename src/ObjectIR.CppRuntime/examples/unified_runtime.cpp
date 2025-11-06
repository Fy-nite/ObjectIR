#include "../include/objectir_runtime.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <filesystem>
#include <algorithm>
#include <chrono>

using namespace ObjectIR;
namespace fs = std::filesystem;

// ============================================================================
// Unified ObjectIR Runtime - Combines Construct Compiler + ObjectIR Execution
// ============================================================================

/// Utility functions
namespace Utils
{
    /// Check if file exists
    bool FileExists(const std::string& path)
    {
        return fs::exists(path);
    }

    /// Read entire file to string
    std::string ReadFile(const std::string& path)
    {
        std::ifstream file(path);
        if (!file.is_open())
        {
            throw std::runtime_error("Cannot open file: " + path);
        }
        std::stringstream buffer;
        buffer << file.rdbuf();
        return buffer.str();
    }

    /// Get current timestamp for logging
    std::string GetTimestamp()
    {
        auto now = std::chrono::system_clock::now();
        auto time = std::chrono::system_clock::to_time_t(now);
        std::stringstream ss;
        ss << std::put_time(std::localtime(&time), "%H:%M:%S");
        return ss.str();
    }

    /// Print formatted section header
    void PrintHeader(const std::string& title)
    {
        std::cout << "\n" << std::string(70, '=') << "\n";
        std::cout << "  " << title << "\n";
        std::cout << std::string(70, '=') << "\n\n";
    }

    /// Print info message with timestamp
    void PrintInfo(const std::string& message)
    {
        std::cout << "[" << GetTimestamp() << "] INFO: " << message << "\n";
    }

    /// Print error message
    void PrintError(const std::string& message)
    {
        std::cerr << "[" << GetTimestamp() << "] ERROR: " << message << "\n";
    }

    /// Print success message
    void PrintSuccess(const std::string& message)
    {
        std::cout << "[" << GetTimestamp() << "] ✓ " << message << "\n";
    }
}

// ============================================================================
// Unified Runtime Configuration
// ============================================================================

struct RuntimeConfig
{
    std::string inputFile;      // Input IR file (JSON or Construct format)
    std::string outputFile;     // Optional: Output file for results
    bool verbose;               // Enable verbose logging
    bool dumpSymbols;           // Dump loaded symbols
    bool interactive;           // Interactive mode
    std::string mainClass;      // Entry point class name
    std::string mainMethod;     // Entry point method name
    std::vector<std::string> args; // Arguments to pass to main

    RuntimeConfig()
        : verbose(false), dumpSymbols(false), interactive(false),
          mainClass("Program"), mainMethod("Main")
    {
    }
};

// ============================================================================
// Unified Runtime Engine
// ============================================================================

class UnifiedObjectIRRuntime
{
private:
    RuntimeConfig config;
    std::shared_ptr<VirtualMachine> vm;
    std::stringstream outputBuffer;

public:
    UnifiedObjectIRRuntime(const RuntimeConfig& cfg)
        : config(cfg)
    {
    }

    /// Main entry point
    int Run()
    {
        try
        {
            Utils::PrintHeader("ObjectIR Unified Runtime");

            // Step 1: Load IR from file
            if (!LoadIR())
            {
                Utils::PrintError("Failed to load IR file");
                return 1;
            }

            // Step 2: Dump symbols if requested
            if (config.dumpSymbols)
            {
                DumpSymbols();
            }

            // Step 3: Execute entry point
            if (!ExecuteEntryPoint())
            {
                Utils::PrintError("Failed to execute entry point");
                return 1;
            }

            // Step 4: Save output if specified
            if (!config.outputFile.empty())
            {
                SaveOutput();
            }

            Utils::PrintSuccess("Execution completed successfully");
            return 0;
        }
        catch (const std::exception& ex)
        {
            Utils::PrintError(std::string("Runtime error: ") + ex.what());
            return 1;
        }
    }

private:
    /// Load ObjectIR module from JSON file
    bool LoadIR()
    {
        Utils::PrintInfo("Loading IR module from: " + config.inputFile);

        if (!Utils::FileExists(config.inputFile))
        {
            Utils::PrintError("Input file not found: " + config.inputFile);
            return false;
        }

        try
        {
            vm = IRLoader::LoadFromFile(config.inputFile);

            if (!vm)
            {
                Utils::PrintError("Failed to create virtual machine");
                return false;
            }

            if (config.verbose)
            {
                Utils::PrintInfo("Virtual machine initialized with loaded types");
            }

            Utils::PrintSuccess("IR module loaded successfully");
            return true;
        }
        catch (const std::exception& ex)
        {
            Utils::PrintError("Failed to load IR: " + std::string(ex.what()));
            return false;
        }
    }

    /// Dump all loaded types and methods
    void DumpSymbols()
    {
        Utils::PrintHeader("Symbol Table");

        std::cout << "Loaded classes and methods:\n\n";

        // Note: This would iterate through vm's registered classes
        // The actual implementation depends on VirtualMachine's public interface
        std::cout << "(Symbol dump depends on VirtualMachine's public API)\n";
    }

    /// Execute the entry point method
    bool ExecuteEntryPoint()
    {
        Utils::PrintHeader("Execution");

        if (!vm)
        {
            Utils::PrintError("Virtual machine not initialized");
            return false;
        }

        try
        {
            Utils::PrintInfo("Locating entry point: " + config.mainClass + "." + config.mainMethod);

            // Create instance of entry point class if not static
            ClassRef entryClass = vm->GetClass(config.mainClass);
            if (!entryClass)
            {
                Utils::PrintError("Entry class not found: " + config.mainClass);
                return false;
            }

            // Get method
            MethodRef entryMethod = entryClass->GetMethod(config.mainMethod);
            if (!entryMethod)
            {
                Utils::PrintError("Entry method not found: " + config.mainMethod);
                return false;
            }

            Utils::PrintInfo("Invoking: " + config.mainClass + "." + config.mainMethod);

            // Prepare arguments
            std::vector<Value> args;
            for (const auto& arg : config.args)
            {
                args.push_back(Value(arg));
            }

            // Create instance if not static
            ObjectRef instance = nullptr;
            if (!entryMethod->IsStatic())
            {
                instance = vm->CreateObject(entryClass);
            }

            // Invoke method
            Value result = vm->InvokeMethod(instance, entryMethod, args);

            // Print result if not void
            if (result.GetType() != PrimitiveType::Void)
            {
                std::cout << "\nResult: ";
                PrintValue(result);
                std::cout << "\n";
            }

            Utils::PrintSuccess("Execution completed");
            return true;
        }
        catch (const std::exception& ex)
        {
            Utils::PrintError("Execution failed: " + std::string(ex.what()));
            return false;
        }
    }

    /// Print a runtime value
    void PrintValue(const Value& value)
    {
        if (value.IsInt32())
        {
            std::cout << value.AsInt32();
        }
        else if (value.IsInt64())
        {
            std::cout << value.AsInt64();
        }
        else if (value.IsFloat32())
        {
            std::cout << value.AsFloat32();
        }
        else if (value.IsFloat64())
        {
            std::cout << value.AsFloat64();
        }
        else if (value.IsBool())
        {
            std::cout << (value.AsBool() ? "true" : "false");
        }
        else if (value.IsString())
        {
            std::cout << "\"" << value.AsString() << "\"";
        }
        else if (value.IsNull())
        {
            std::cout << "null";
        }
        else if (value.IsObject())
        {
            std::cout << "[Object]";
        }
    }

    /// Save execution output to file
    void SaveOutput()
    {
        Utils::PrintInfo("Saving output to: " + config.outputFile);

        std::ofstream outFile(config.outputFile);
        if (!outFile.is_open())
        {
            Utils::PrintError("Cannot open output file for writing");
            return;
        }

        outFile << outputBuffer.str();
        outFile.close();

        Utils::PrintSuccess("Output saved");
    }
};

// ============================================================================
// Command Line Interface
// ============================================================================

void PrintUsage(const char* programName)
{
    std::cout << "\n";
    std::cout << "ObjectIR Unified Runtime - Construct Compiler + ObjectIR Executor\n";
    std::cout << "=====================================================================\n\n";

    std::cout << "Usage: " << programName << " [OPTIONS] <input-file>\n\n";

    std::cout << "Arguments:\n";
    std::cout << "  <input-file>           ObjectIR JSON module file\n\n";

    std::cout << "Options:\n";
    std::cout << "  -h, --help             Show this help message\n";
    std::cout << "  -o, --output FILE      Save output to file\n";
    std::cout << "  -v, --verbose          Enable verbose logging\n";
    std::cout << "  -d, --dump-symbols     Dump loaded symbol table\n";
    std::cout << "  -c, --class CLASS      Entry point class (default: Program)\n";
    std::cout << "  -m, --method METHOD    Entry point method (default: Main)\n";
    std::cout << "  --args ARGS            Arguments to pass to main method\n\n";

    std::cout << "Examples:\n";
    std::cout << "  # Run with defaults\n";
    std::cout << "  " << programName << " calculator.json\n\n";

    std::cout << "  # Run with verbose output and symbol dump\n";
    std::cout << "  " << programName << " -v -d todoapp.json\n\n";

    std::cout << "  # Run specific entry point\n";
    std::cout << "  " << programName << " -c MyApp -m Run app.json\n\n";

    std::cout << "  # Run and save output\n";
    std::cout << "  " << programName << " -o results.txt program.json\n\n";
}

bool ParseCommandLine(int argc, char* argv[], RuntimeConfig& config)
{
    if (argc < 2)
    {
        PrintUsage(argv[0]);
        return false;
    }

    for (int i = 1; i < argc; ++i)
    {
        std::string arg = argv[i];

        if (arg == "-h" || arg == "--help")
        {
            PrintUsage(argv[0]);
            return false;
        }
        else if (arg == "-v" || arg == "--verbose")
        {
            config.verbose = true;
        }
        else if (arg == "-d" || arg == "--dump-symbols")
        {
            config.dumpSymbols = true;
        }
        else if (arg == "-o" || arg == "--output")
        {
            if (i + 1 < argc)
            {
                config.outputFile = argv[++i];
            }
            else
            {
                std::cerr << "Error: -o requires an argument\n";
                return false;
            }
        }
        else if (arg == "-c" || arg == "--class")
        {
            if (i + 1 < argc)
            {
                config.mainClass = argv[++i];
            }
            else
            {
                std::cerr << "Error: -c requires an argument\n";
                return false;
            }
        }
        else if (arg == "-m" || arg == "--method")
        {
            if (i + 1 < argc)
            {
                config.mainMethod = argv[++i];
            }
            else
            {
                std::cerr << "Error: -m requires an argument\n";
                return false;
            }
        }
        else if (arg == "--args")
        {
            if (i + 1 < argc)
            {
                while (i + 1 < argc && argv[i + 1][0] != '-')
                {
                    config.args.push_back(argv[++i]);
                }
            }
        }
        else if (arg[0] != '-')
        {
            config.inputFile = arg;
        }
    }

    if (config.inputFile.empty())
    {
        std::cerr << "Error: No input file specified\n";
        PrintUsage(argv[0]);
        return false;
    }

    return true;
}

// ============================================================================
// Main Entry Point
// ============================================================================

int main(int argc, char* argv[])
{
    RuntimeConfig config;

    if (!ParseCommandLine(argc, argv, config))
    {
        return config.inputFile.empty() ? 0 : 1;
    }

    UnifiedObjectIRRuntime runtime(config);
    return runtime.Run();
}
