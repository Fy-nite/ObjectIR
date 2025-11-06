namespace ObjectIR.Fortran.Compiler;

public sealed class FortranCompilationOptions
{
    public static FortranCompilationOptions Default => new(FortranIntrinsicRegistry.CreateDefault());

    public FortranIntrinsicRegistry Intrinsics { get; }

    public FortranCompilationOptions(FortranIntrinsicRegistry? intrinsics = null)
    {
        Intrinsics = intrinsics ?? FortranIntrinsicRegistry.CreateDefault();
    }

    public FortranCompilationOptions WithIntrinsics(FortranIntrinsicRegistry intrinsics)
    {
        return new FortranCompilationOptions(intrinsics);
    }
}
