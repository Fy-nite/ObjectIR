using Xunit;
using ObjectIR.Core.Serialization;
using ObjectIR.Core.IR;
using ObjectIR.Core.Builder;

namespace ObjectIR.Tests;

/// <summary>
/// Tests for ModuleLoader caching functionality
/// </summary>
public class ModuleLoaderCachingTests
{
    [Fact]
    public void LoadFromText_CachesModuleAutomatically()
    {
        // Arrange
        var textFormat = @"
module CacheTestModule

class CacheClass {
    field id: int32
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);
        var cached = loader.GetCachedModule("CacheTestModule");

        // Assert
        Assert.NotNull(cached);
        Assert.Equal(module.Name, cached.Name);
    }

    [Fact]
    public void GetCachedModule_ReturnsSameInstance()
    {
        // Arrange
        var textFormat = @"
module InstanceTestModule

class InstanceClass {
    field id: int32
}
";
        var loader = new ModuleLoader();
        var module1 = loader.LoadFromText(textFormat);

        // Act
        var module2 = loader.GetCachedModule("InstanceTestModule");

        // Assert
        Assert.Same(module1, module2);
    }

    [Fact]
    public void GetCachedModule_NonExistent_ReturnsNull()
    {
        // Arrange
        var loader = new ModuleLoader();

        // Act
        var cached = loader.GetCachedModule("NonExistentModule");

        // Assert
        Assert.Null(cached);
    }

    [Fact]
    public void GetAllCachedModules_ReturnsAllLoadedModules()
    {
        // Arrange
        var loader = new ModuleLoader();
        var textFormat1 = @"
module Module1

class C1 {
    field id: int32
}
";
        var textFormat2 = @"
module Module2

class C2 {
    field id: int32
}
";

        // Act
        loader.LoadFromText(textFormat1);
        loader.LoadFromText(textFormat2);
        var allCached = loader.GetAllCachedModules();

        // Assert
        Assert.Equal(2, allCached.Count);
        Assert.Contains("Module1", allCached.Keys);
        Assert.Contains("Module2", allCached.Keys);
    }

    [Fact]
    public void ClearCache_RemovesAllCachedModules()
    {
        // Arrange
        var loader = new ModuleLoader();
        loader.LoadFromText("module M1 class C1 {}");
        loader.LoadFromText("module M2 class C2 {}");

        // Act
        loader.ClearCache();
        var allCached = loader.GetAllCachedModules();

        // Assert
        Assert.Empty(allCached);
    }

    [Fact]
    public void LoadFromJson_CachesModule()
    {
        // Arrange
        var loader = new ModuleLoader();
        var builder = new IRBuilder("JsonCacheModule");
        builder.Class("JsonCacheClass").Field("id", TypeReference.Int32).EndField();
        var originalModule = builder.Build();
        var json = originalModule.DumpJson();

        // Act
        var module = loader.LoadFromJson(json);
        var cached = loader.GetCachedModule("JsonCacheModule");

        // Assert
        Assert.NotNull(cached);
        Assert.Equal("JsonCacheModule", cached.Name);
    }

    [Fact]
    public void GetAllCachedModules_IsReadOnly()
    {
        // Arrange
        var loader = new ModuleLoader();
        loader.LoadFromText("module M1 class C1 {}");
        var cached = loader.GetAllCachedModules();

        // Assert - Should be read-only dictionary
        Assert.IsAssignableFrom<IReadOnlyDictionary<string, Module>>(cached);
    }

    [Fact]
    public void LoadFromText_DuplicateModuleName_OverwritesInCache()
    {
        // Arrange
        var loader = new ModuleLoader();
        var text1 = @"
module DupModule

class C1 {
    field id: int32
}
";
        var text2 = @"
module DupModule

class C2 {
    field id: int32
}
";

        // Act
        var module1 = loader.LoadFromText(text1);
        loader.LoadFromText(text2);
        var module2 = loader.GetCachedModule("DupModule");

        // Assert
        Assert.NotNull(module1);
        Assert.NotNull(module2);
        Assert.Single(loader.GetAllCachedModules());
    }

    [Fact]
    public void ClearCache_AllowsReloadingModules()
    {
        // Arrange
        var loader = new ModuleLoader();
        var textFormat = @"module ClearTestModule class ClearClass {}";

        // Act
        var module1 = loader.LoadFromText(textFormat);
        loader.ClearCache();
        var module2 = loader.LoadFromText(textFormat);

        // Assert
        Assert.NotSame(module1, module2);
        Assert.Single(loader.GetAllCachedModules());
    }
}
