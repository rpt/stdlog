-define(APP, stdlog).

-type severity() :: emergency
                  | alert
                  | critical
                  | error
                  | warning
                  | notice
                  | info
                  | debug.

-type message() :: iodata().

-type metadata() :: map().
