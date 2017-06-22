namespace csharp_lexer_analysis.engine_with
{
    /**
     * LR table interface
     */
    interface LrTable
    {
        /**
         * Return the EMPTY term ($)
         */
        Token Empty();

        /**
         * Return the associated action (action table)
         */
        LrAction Action(int state, Token term);

        /**
         * Return the associated action (goto table)
         */
        LrAction Action(int state, string nonTerm);
    }
}
