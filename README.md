[![Build Status](https://travis-ci.org/alephcloud/hs-yet-another-logger.svg)](https://travis-ci.org/alephcloud/hs-yet-another-logger)

A logging framework that was written with flexibility and performance
in mind.

Quick Start
===========

```haskell
import System.Logger

main ∷ IO ()
main = withConsoleLogger Info $ do
    logg Info "moin"
    withLabel ("function", "f") f
    logg Warn "tschüss"
  where
    f = withLevel Debug $ do
        logg Debug "debug f"
```

Overview
========

**This Version is yet a preview**

The logging system consists of four main parts:

1. The logging front-end are those types and functions that are used
   to produce log messages in the code. This includes the `LogLevel`
   type, the `LogPolicy` type, the `LogLabel` and `LogScope` types,
   the `LogFunction` type, and the `MonadLog` type class.

2. The abstract `LoggerCtx` is the context through which the `LogFunction`
   delivers log messages to the logger back-end.

3. The formatter is a function for serializing log messages.

4. The logger back-end is a callback that is invoked by `Logger` on
   each log messages. The logger back-end applies the formatting function
   and delivers the log messages to some sink.

The framework allows to combine this components in a modular way. The
front-end types, the `Logger`, and the back-end callback are represented
by types or type classes. The formatter exists only as a concept
in the implementation of back-ends. These types and concepts together
form the abstract logger interface that is defined in the module
`System.Logger.Types`.

The package also provides a concrete Logger that implements these components
in the module `System.Logger.Logger`.

Logger Implementation
=====================

Writing a log message in a service application should introduce only minimal
latency overhead in the thread where the log message is written. Processing
should be done asynchronously as much as possible. This framework addresses
this by doing all serialization and IO in an asynchronous logger back-end
callback.

When a Log message is produced it is associated with a logger context. The
logger context includes

*   a log-level threshold,
*   a scope, which is a list of key-value labels which are used to
    tag log messages with additional information, and
*   a policy that specifies how to deal with a situation where the
    log message pipeline is congested.

A log message can be any Haskell type with `Show`, `Typeable`, and `NFData`
constraint. Ideally the logged value is computed anyways in the program so that
constructing and forcing it does not introduce any additional overhead.

When a log messages is produced it is tagged with a time stamp. This introduces
overhead and there is be room for optimizations here. A log message also has a
log-level. If the log-threshold that is effective at the time a log message is
written isn't met, no message is produced.

The logger has an internal log message queue. Further benchmarking should be
done in chosen the queue implementation that is best suited for this purpose.

The logger asynchronously reads log messages from the queue and calls the
back-end callback for each message. Right now the code includes only a single
back-end, namely for writing to a handle, but we are going to add more back-ends
soon. Due to the modular design it is possible to combine different back-ends
into a single back-end so that messages are processed by more than a single
back-end and delivered to more than a single sink.

A back-end includes a formatting function. This is where, beside IO, most
processing happens.

Delaying the serialization to the very end of the processing pipeline has
the following advantages:

1.  serialization is done asynchronously,
2.  serialization is done only for messages that are actually delivered and
    it is done only for those parts of the message that are relevant for the
    respective back-end, and
3.  it is easy to deploy different serialization methods.

For instance when logging to the console one usually wants a line-wise
UNIX-tool friendly format. For a cloud service one may chose an efficient
binary serialization with a back-end that stores messages in a remote database.
There may be circumstances where the data of all or some messages is just
aggregated for statistical analysis before the messages are discarded. The
modular design that decouples generation and serialization of log messages
allows to accommodate these different scenarios by just using different
back-ends, possibly parameterized by the formatting function.

