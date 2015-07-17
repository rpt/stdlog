-module(gen_logger).


-callback log(map()) ->
    ok.
