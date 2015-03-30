0.2
===

*   Removed `MonadLogIO`.

*   Issue #16: add an argument to all command-line options parsers that
    adds a prefix to the command-line options.

*   Issue #9: more robust logger backend

    *   new functions that take an function of type `Text -> IO ()` as
        an alternate backend to log issues in the logging system itself.

        *   `withLogger_`
        *   `withLogFunction_`
        *   `createLogger_`

    *   new parameters
        *   `loggerConfigExceptionLimit`:
            Number of consecutive backend exception that can occur before the logger
            raises an `BackendToManyExceptions` exception. If this is `Nothing`
            the logger will discard all exceptions.

        *   `loggerConfigExceptionWait`:
            Number of microseconds to wait after an exception from the backend.
            If this is 'Nothing' the logger won't wait at all after an exception.

        *   `loggerConfigExitTimeout`:
            Timeout in microseconds for the logger to flush the queue and
            deliver all remaining log messages on termination. If this is `Nothing`
            termination of the logger blogs until all mesages are delivered.

    *   new `LoggerException` type
        *   `QueueFullException` is thrown when the queue is full and the logger
            policy is to throw exceptions on a full queue.

        *   `BackendTerminatedException` can be thrown by a backend to force the
            logger immediately.

        *   `BackendToManyExceptions` is thrown when the backend is throwing some
            unexpected exception more than list `loggerConfigExceptionLimit` times.

*   Issue #12: a test-suite got added to the package. Note that this test-suite takes
    a relatively long time to run all tests. In particular this an cause timeouts
    with travis-ci.

0.1.1
=====

*   Added `MonadLogIO` for loggers that allow to extract a log
    function of type `LogFunctionIO`.

0.1
===

*   Added `localScope` function to `MonadLog` and and implemented `withLabel`
    based on it.

*   Added functions `popLabel` and `clearScope`. These are useful when setting
    log-labels for bracket style functions.

*   Remove overlapping `MonadLog` instances.

*   Lift `MonadTrace` instances into `LoggerCtxT`.

0.0.1
=====

First public release

