-module(lib_misc).

-export([pmap/2, pmap1/2,  foreachWordInFile/2, string2value/1]).

-import(lists, [all/2, any/2, filter/2, reverse/1, reverse/2,
		member/2, sort/1]).

string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.

map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

foreach(_, []) -> void;
foreach(F, [H|T]) -> F(H), foreach(F, T).

% map function in parallel mode, each elment is processed by one process.

pmap(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    Pids = map(fun(I) -> spawn(fun() -> do_f(S, Ref, F, I) end) 
               end, L),
    gather(Pids, Ref).

do_f(S, Ref, F, I) -> 
   S ! { self(), Ref, F(I)}.

gather([], _) -> [];
gather([Pid|T], Ref) -> 
   receive
       {Pid, Ref, X} ->
         [X | gather(T, Ref)]
   end.


pmap1(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    foreach(fun(I) -> spawn(fun() -> do_f1(S, Ref, F, I) end) 
               end, L),
    gather1(length(L), Ref, []).

do_f1(S, Ref, F, I) -> 
   S ! {Ref, F(I)}.

gather1(0,_, Acc) -> Acc;
gather1(N, Ref, Acc) -> 
   receive
       {Ref, X} -> gather1(N-1, Ref, [X | Acc])
   end.


%% evalute F(Word) for each word in the file File		  
foreachWordInFile(File, F) ->
    case file:read_file(File) of
	{ok, Bin} -> foreachWordInString(binary_to_list(Bin), F);
	_         -> void
    end.



foreachWordInString(Str, F) ->
    case get_word(Str) of
	no -> 
	    void;
	{Word, Str1} ->
	    F(Word),
	    foreachWordInString(Str1, F)
    end.


isWordChar(X) when $A=< X, X=<$Z -> true;
isWordChar(X) when $0=< X, X=<$9 -> true;
isWordChar(X) when $a=< X, X=<$z -> true;
isWordChar(_)  -> false.

get_word([H|T]) ->
    case isWordChar(H) of
	true  -> collect_word(T, [H]);
	false -> get_word(T)
    end;
get_word([]) ->
    no.

collect_word([H|T]=All, L) ->
    case isWordChar(H) of
	true  -> collect_word(T, [H|L]);
	false -> {reverse(L), All}
    end;
collect_word([], L) ->
    {reverse(L), []}.

