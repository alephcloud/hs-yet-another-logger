0.1
===

*   Added `localScope` function to `MonadLog` and and implement `withLabel`
    based on it.

*   Added functions `popLabel` and `clearScope` are useful when setting
    log-labels for bracket style functions.

*   Remove overlapping `MonadLog` instances.

*   Lift `MonadTrace` instances into `LoggerCtxT`.

0.0.1
=====

First public release

