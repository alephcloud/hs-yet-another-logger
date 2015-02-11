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
    logg Info "tschüss"
  where
    f = withLevel Debug $ do
        logg Debug "debug f"
```

Description
===========

**This Version is yet a preview**

The framework consists of four main parts:

1.  The logging front-end are those types and functions that are used
    to produce log messages in the code. This includes the `LogLevel`
    type, the `LogFunction`, the abstract `LoggerCtx`, and the `MonadLog`
    type class along with a monad transformer instance.

2.  The logger queue is a channel into which the `LogFunction` delivers
    log messages. The log processor is a background worker that consumes
    log messages from the queue and delivers them to the back-end.

3.  The formatter is a function that takes log message and serializes
    it to the format that is expected by a back-end.

4.   The logger back-end is a callback that is invoked by the log processor.
    The logger back-end applies a log message formatting function and
    delivers the log messages to some sink.

This parts are described by function signatures and can thus be developed
and combined in a modular way.

This package provides the default implementations of these components.

Writing a log message in a service application should introduce only
minimal latency overhead in the thread where the log message is written.
Processing should be done asynchronously as much as possible.
This framework addresses this by doing all serialization through a formatting
function that is a parameter of the back-end callback. A log message can be
any Haskell type with `Show`, `Typeable`, and `NFData` constraint. Ideally the
logged value is computed anyways in the program so that constructing and
forcing it does not introduce any additional overhead. Log messages have a
time-stamp that is produced when the message is produced. This introduces
overhead and there is be room for optimizations here. A log message also has a
log-level. If the log-threshold that is effective at the time a log message is
written isn't met, no message is produced. A log message is associated with a
log-context which includes the currently effective log-level threshold and a
list of log-labels that are provided to the formatting function.

The front-end delivers new log messages to the log queue. Further
benchmarking should be done in chosen the queue implementation that is best
suited for this purpose.

The log processor asynchronously reads log messages from
the queue and calls the back-end callback for each message. Right now the
code includes only a single back-end, namely for writing to a handle, but we
are going to add more back-end soon. Due to the modular design it is possible
to deliver to combine different back-ends into a single back-end so that
messages are processed by more than a single back-end.

A back-end is parameterized with a formatting function. This is where, beside
IO, most processing happens. The formatting function serializes log
messages according to the needs of the respective back-end.

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
allows to accommodate to these different scenarios by just passing a different
formatting function to the back-end.
