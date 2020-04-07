# yet-another-logger

## Unreleased

#### Changed

- `microlens` is now used in place of `lens`.
- The three lensy functions of the `LoggerCtx` class which were previously
  `Setter'` are now `Lens'`.
- The following functions are now less polymorphic, requiring a strict `Text`
  instead of anything that had a `IsText` instance:
  - `readColorOption`
  - `readLoggerHandleConfig`
  - `readLogLevel`
  - `readLogPolicy`

## 0.3.1

*   Add support for tasty-hunit >= 0.10.

*   Raise lower bounds on some dependencies:
    *   base >= 4.8
    *   deepseq >= 1.4
    *   aeson >= 0.11
    *   time >= 1.5

*   Require Cabal >= 1.22.

*   Drop support for GHC < 7.10.

## 0.3.0

*   Remove `EitherT` instance. It's recommended to use `ExceptT` instead.

*   Remove `TraceT` instance. It's not actively maintained any more.

*   Support aeson >= 0.11, which comes with instances for `Natural`.

*   Support GHC-8.

*   `MonadThrow`, `MonadCatch`, and `MonadMask` instances for `LoggerT`.

*   Support for Cabal-1.18 and time >= 1.4 && < 1.5.

## 0.2.3.1

*   Add missing NFData instance for `Natural` when build with
    deepseq>=1.4 and base<4.8.

## 0.2.3

*   Fix semantically invalid default NFData instance when build with
    deepseq <1.4.

## 0.2.2

*   [Issue #28] Generalized type of lens `logMsg` to support changing
    the message type.

*   [Issue #29] Fixed precision of `formatIso8601` function.

*   [Issue #30] Added a version of `withHandleBackend` that is generic
    in the log message type and accept a formatting function for
    formatting the log message as text.

## 0.2.1

*   Support for trace-0.2.

## 0.2

*   Removed `MonadLogIO`; `MonadBaseControl` should be used instead.

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

*   Issue #14: the command line option for setting the log-level got fixed to be
    spelled `--log-level` (instead of `--loglevel`).

*   Issue #22: added timestamp to log messages.

*   Replaced usage of `Int` by `Natural` where adequate.

*   Issue #6: use `TBMChan` as internal queue implementation until a new version of
    stm is released with a [fix for `TQueue`](https://ghc.haskell.org/trac/ghc/ticket/9539).

## 0.1.1

*   Added `MonadLogIO` for loggers that allow to extract a log
    function of type `LogFunctionIO`.

## 0.1

*   Added `localScope` function to `MonadLog` and and implemented `withLabel`
    based on it.

*   Added functions `popLabel` and `clearScope`. These are useful when setting
    log-labels for bracket style functions.

*   Remove overlapping `MonadLog` instances.

*   Lift `MonadTrace` instances into `LoggerCtxT`.

## 0.0.1

First public release
