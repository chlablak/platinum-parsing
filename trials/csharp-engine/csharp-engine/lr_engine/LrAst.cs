using System.Collections.Generic;

namespace csharp_engine.lr_engine
{
    class LrAst
    {
        public class Node
        {
            public string name { get; set; }
            public string value { get; set; }
            public List<Node> children { get; set; }

            public Node(string name = "", string value = "")
            {
                this.name = name;
                this.value = value;
                this.children = new List<Node>();
            }
        }

        public Node root { get; set; }

        public LrAst()
        {
            this.root = new Node();
        }
    }
}
