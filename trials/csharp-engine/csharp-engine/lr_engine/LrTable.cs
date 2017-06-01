namespace csharp_engine.lr_engine
{
    /**
     * LR table interface
     */
    interface LrTable
    {
        /**
         * Return the associated action (action table)
         */
        LrAction action(int state, char term);

        /**
         * Return the associated action (goto table)
         */
        LrAction action(int state, string nonTerm);
    }
}
