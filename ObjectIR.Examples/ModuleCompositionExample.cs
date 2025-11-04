using System;
using System.IO;
using ObjectIR.Core.IR;
using ObjectIR.Core.Composition;
using ObjectIR.Core.Serialization;

namespace ObjectIR.Examples;

/// <summary>
/// Demonstrates module composition - combining multiple modules into a unified module
/// </summary>
public static class ModuleCompositionExample
{
    public static void Main()
    {
        Console.WriteLine("=== ObjectIR Module Composition Example ===\n");

        // Create individual modules
        var animalsModule = BuildAnimalsModule();
        var vehiclesModule = BuildVehiclesModule();

        // Compose modules
        var composer = new ModuleComposer();
        composer.AddModule(animalsModule);
        composer.AddModule(vehiclesModule);

        // Validate composition
        var validation = composer.Validate();
        if (validation.HasErrors)
        {
            Console.WriteLine("Composition Errors:");
            foreach (var error in validation.Errors)
            {
                Console.WriteLine($"  ERROR: {error}");
            }
            return;
        }

        if (validation.HasWarnings)
        {
            Console.WriteLine("Composition Warnings:");
            foreach (var warning in validation.Warnings)
            {
                Console.WriteLine($"  WARNING: {warning}");
            }
        }

        // Compose into unified module
        var composed = composer.Compose("UnifiedApp", "2.0.0");

        Console.WriteLine("Composition succeeded!\n");
        Console.WriteLine(composer.GenerateReport());

        Console.WriteLine("\nComposed Module Information:");
        Console.WriteLine($"  Name: {composed.Name}");
        Console.WriteLine($"  Version: {composed.Version}");
        Console.WriteLine($"  Total Types: {composed.Types.Count}");
        Console.WriteLine($"  Total Functions: {composed.Functions.Count}");

        // Display dependency graph
        Console.WriteLine("\nDependency Graph:");
        var graph = composer.GetDependencyGraph();
        foreach (var (typeName, dependencies) in graph)
        {
            if (dependencies.Count > 0)
            {
                Console.WriteLine($"  {typeName} -> {string.Join(", ", dependencies)}");
            }
        }

        // Save composed module
        var json = new ObjectIR.Core.Serialization.ModuleSerializer(composed).DumpToJson();
        File.WriteAllText("ComposedModule.json", json);
        Console.WriteLine("\nComposed module saved to ComposedModule.json");
    }

    private static Module BuildAnimalsModule()
    {
        var module = new Module("Animals", "1.0.0");

        // Define Animal interface
        var animalInterface = module.DefineInterface("IAnimal");
        animalInterface.DefineMethod("Speak", TypeReference.String);
        animalInterface.DefineMethod("Move", TypeReference.Void);

        // Define Animal base class
        var animalClass = module.DefineClass("Animal");
        animalClass.IsAbstract = true;
        animalClass.DefineField("name", TypeReference.String);
        animalClass.DefineField("age", TypeReference.Int32);

        // Define Dog class
        var dogClass = module.DefineClass("Dog");
        dogClass.BaseType = TypeReference.FromName("Animal");
        dogClass.Interfaces.Add(TypeReference.FromName("IAnimal"));
        dogClass.DefineField("breed", TypeReference.String);

        var speakMethod = dogClass.DefineMethod("Speak", TypeReference.String);
        speakMethod.Instructions.Emit(new LoadConstantInstruction("Woof!", TypeReference.String));
        speakMethod.Instructions.Emit(new ReturnInstruction(null));

        var moveMethod = dogClass.DefineMethod("Move", TypeReference.Void);
        moveMethod.Instructions.Emit(new ReturnInstruction(null));

        // Define Cat class
        var catClass = module.DefineClass("Cat");
        catClass.BaseType = TypeReference.FromName("Animal");
        catClass.Interfaces.Add(TypeReference.FromName("IAnimal"));

        var catSpeakMethod = catClass.DefineMethod("Speak", TypeReference.String);
        catSpeakMethod.Instructions.Emit(new LoadConstantInstruction("Meow!", TypeReference.String));
        catSpeakMethod.Instructions.Emit(new ReturnInstruction(null));

        var catMoveMethod = catClass.DefineMethod("Move", TypeReference.Void);
        catMoveMethod.Instructions.Emit(new ReturnInstruction(null));

        module.Metadata["Author"] = "Zoo Team";
        module.Metadata["Description"] = "Animal class hierarchy";

        return module;
    }

    private static Module BuildVehiclesModule()
    {
        var module = new Module("Vehicles", "1.0.0");

        // Define IVehicle interface
        var vehicleInterface = module.DefineInterface("IVehicle");
        vehicleInterface.DefineMethod("Start", TypeReference.Void);
        vehicleInterface.DefineMethod("Stop", TypeReference.Void);

        // Define Vehicle base class
        var vehicleClass = module.DefineClass("Vehicle");
        vehicleClass.IsAbstract = true;
        vehicleClass.DefineField("make", TypeReference.String);
        vehicleClass.DefineField("model", TypeReference.String);
        vehicleClass.DefineField("year", TypeReference.Int32);

        // Define Car class
        var carClass = module.DefineClass("Car");
        carClass.BaseType = TypeReference.FromName("Vehicle");
        carClass.Interfaces.Add(TypeReference.FromName("IVehicle"));
        carClass.DefineField("numDoors", TypeReference.Int32);

        var startMethod = carClass.DefineMethod("Start", TypeReference.Void);
        startMethod.Instructions.Emit(new ReturnInstruction(null));

        var stopMethod = carClass.DefineMethod("Stop", TypeReference.Void);
        stopMethod.Instructions.Emit(new ReturnInstruction(null));

        // Define Bike class
        var bikeClass = module.DefineClass("Bike");
        bikeClass.BaseType = TypeReference.FromName("Vehicle");
        bikeClass.Interfaces.Add(TypeReference.FromName("IVehicle"));
        bikeClass.DefineField("engineSize", TypeReference.Int32);

        var bikeStartMethod = bikeClass.DefineMethod("Start", TypeReference.Void);
        bikeStartMethod.Instructions.Emit(new ReturnInstruction(null));

        var bikeStopMethod = bikeClass.DefineMethod("Stop", TypeReference.Void);
        bikeStopMethod.Instructions.Emit(new ReturnInstruction(null));

        module.Metadata["Author"] = "Transport Team";
        module.Metadata["Description"] = "Vehicle class hierarchy";

        return module;
    }
}
