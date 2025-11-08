namespace ObjectIR.Fortran.Compiler;

public sealed class FortranCompilationOptions
{
    // Single canonical constructor
    public FortranCompilationOptions(FortranIntrinsicRegistry? intrinsics = null, FortranIOMapping? io = null, bool debug = false)
    {
        Intrinsics = intrinsics ?? FortranIntrinsicRegistry.CreateDefault();
        IO = io ?? FortranIOMapping.CreateDefault();
        Debug = debug;
    }

    public static FortranCompilationOptions Default => new FortranCompilationOptions();

    public FortranIntrinsicRegistry Intrinsics { get; }
    public FortranIOMapping IO { get; }
    public bool Debug { get; }

    public FortranCompilationOptions WithIntrinsics(FortranIntrinsicRegistry intrinsics)
        => new FortranCompilationOptions(intrinsics, IO, Debug);

    public FortranCompilationOptions WithIO(FortranIOMapping io)
        => new FortranCompilationOptions(Intrinsics, io, Debug);

    public FortranCompilationOptions WithDebug(bool debug)
        => new FortranCompilationOptions(Intrinsics, IO, debug);
}
