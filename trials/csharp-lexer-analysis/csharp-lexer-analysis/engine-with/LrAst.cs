using System.Collections.Generic;

namespace csharp_lexer_analysis.engine_with
{
    /**
     * AST computed by the LR parser
     */
    class LrAst
    {
        /**
         * Node type 
         */
        public class Node
        {
            public enum Type { Root, Term, NonTerm }

            public Type type { get; set; }
            public string value { get; set; }
            public List<Node> children { get; set; }

            public Node(Type type, string value)
            {
                this.type = type;
                this.value = value;
                this.children = new List<Node>();
            }

            public override string ToString()
            {
                return ToString(this, 0);
            }

            private string ToString(Node node, int level)
            {
                string str = new string('|', level);
                str += "- " + node.value + '\n';
                foreach (Node child in node.children)
                    str += ToString(child, level + 1);
                return str;
            }
        }

        /**
         * Root node
         */
        public Node root { get; set; }

        /**
         * Constructor
         */
        public LrAst()
        {
            root = new Node(Node.Type.Root, null);
        }

        /**
         * Modify the AST by a shift action
         */
        public void Shift(string value)
        {
            root.children.Add(new Node(Node.Type.Term, value));
        }

        public void Shift(char value)
        {
            Shift(new string(value, 1));
        }

        /**
         * Modify the AST by a reduce action
         */
        public void Reduce(string name, int length)
        {
            int index = root.children.Count - length;
            Node nonTerm = new Node(Node.Type.NonTerm, name);
            nonTerm.children.InsertRange(0, root.children.GetRange(index, length));
            root.children.RemoveRange(index, length);
            root.children.Add(nonTerm);
        }

        /**
         * Merge contiguous Term nodes
         */
        public void Merge()
        {
            root = Merge(root);
        }

        private Node Merge(Node node)
        {
            Node mnode = new Node(node.type, node.value);
            foreach(Node child in node.children)
            {
                if (mnode.children.Count == 0)
                    mnode.children.Add(Merge(child));
                else
                {
                    Node last = mnode.children[mnode.children.Count - 1];
                    if (last.type == Node.Type.Term && child.type == Node.Type.Term)
                        last.value += child.value;
                    else
                        mnode.children.Add(Merge(child));
                }
            }
            return mnode;
        }

        /**
         * Print the AST in a string
         */
        public override string ToString()
        {
            return root.ToString();
        }
    }
}
