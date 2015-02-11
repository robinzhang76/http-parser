-module(socket_examples).
-compile(export_all).

test() -> 
   Bin = nano_get_url(),
   parse(Bin).

nano_get_url() ->
    nano_get_url("www.baidu.com").

nano_get_url(Host) ->
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            receive_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            list_to_binary(lists:reverse(SoFar))
    end.

remove_space(<<" ", T/binary>>) -> remove_space(T);
remove_space(T) -> T.

handle_token([Token], State) -> void;
handle_token([<<"Content-Length">>, Val], State) -> 
               {content_length, binary_to_integer(remove_space(Val))};
handle_token([Token, Val], State) -> void.


parse_line(<<>>, State) -> finish;
parse_line(Bin, State) ->
    L = binary:split(Bin, [<<":">>]),
    X = handle_token(L, State),
    {continue, X}.

parse_header(Bin, Acc) ->
    [A, B|[]] = binary:split(Bin, [<<13,10>>]),
    io:format("~p~n", [A]),
    case parse_line(A, Acc) of
      {continue, X} -> parse_header(B, [X|Acc]);
      finish -> {Acc, B}
    end.

parse_body(Header, Remaining) ->
    case lists:keyfind(content_length, 1, Header) of 
        {_, Len} -> 
            {Body, R} = erlang:split_binary(Remaining, Len);
        false -> {<<>>, Remaining}
    end.

parse(Bin) ->
    {Header,Remaining} = parse_header(Bin, []),
    {Body, R} = parse_body(Header, Remaining),
    case R of
     <<>> -> Body;
     _ -> parse(R)
    end.

start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).

start_seq_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
    seq_loop(Listen).

start_parallel_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    loop(Socket).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
           io:format("Server received binary = ~p~n", [Bin]),
           Str = binary_to_term(Bin),
           io:format("Server (unpacked) binary = ~p~n", [Str]),
           Reply = lib_misc:string2value(Str),
           io:format("Server replying = ~p~n", [Reply]),
           gen_tcp:send(Socket, term_to_binary(Reply)),
           loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
   end.

nano_client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
           io:format("Client received binary = ~p~n", [Bin]),
           Val = binary_to_term(Bin),
           io:format("Client result = ~p~n", [Val]),
           gen_tcp:close(Socket)
   end.
