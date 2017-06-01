namespace csharp_engine.lr_engine
{
    interface LrTable
    {
        LrAction action(int state, char term);
        LrAction action(int state, string nonTerm);
    }
}
