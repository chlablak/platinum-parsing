namespace csharp_engine.lr_engine
{
    /**
     * LR action
     */
    public class LrAction
    {
        public enum Type { Shift, Reduce, Goto, Accept, Error }

        public Type type { get; }
        public int value { get; }      // for Shift, Reduce and Goto
        public string name { get; }    // for Reduce only

        public LrAction(Type type, int value = -1, string name = null)
        {
            this.type = type;
            this.value = value;
            this.name = name;
        }
    }
}
