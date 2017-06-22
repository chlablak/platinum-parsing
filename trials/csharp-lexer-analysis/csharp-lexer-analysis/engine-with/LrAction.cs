namespace csharp_lexer_analysis.engine_with
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

        public override string ToString()
        {
            return type.ToString() + " " + value + " " + name;
        }
    }
}
