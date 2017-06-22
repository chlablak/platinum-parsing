namespace csharp_lexer_analysis.engine_with
{
    public class Token
    {
        public string type { get; }
        public string value { get; }

        public Token(string type, string value = null)
        {
            this.type = type;
            this.value = value;
        }

        public override string ToString()
        {
            return "(" + type + ", " + value + ")";
        }
    }
}
