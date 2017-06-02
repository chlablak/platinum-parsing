namespace csharp_engine.lr_engine
{
    /**
     * LR table interface
     */
    interface LrTable
    {
        /**
         * Return the EMPTY term ($)
         */
        char Empty();

        /**
         * Return the associated action (action table)
         */
        LrAction Action(int state, char term);

        /**
         * Return the associated action (goto table)
         */
        LrAction Action(int state, string nonTerm);
    }
}
