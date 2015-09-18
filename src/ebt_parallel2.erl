%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc 并行节点, 当所有孩子节点准入时执行，当有所有孩子节点执行失败时退出
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_parallel2).

-ebt_extend(ebt_node).

-include("ebt.hrl").
-include("ebt_transform.hrl").

%% API
-export([do_evaluate/1, tick/1]).

do_evaluate(#ebt_node{childs = Childs}) ->
    check_child(Childs).

tick(#ebt_node{id = Id, childs = Childs}) ->
    Data = ?EBT_NODE_DATA(Id),

    ChildResults = maps:get(child_results, Data, #{}),

    case do_and_tick(Childs, ChildResults, length(Childs)) of
        done ->
            ?EBT_NODE_DATA(Id, Data#{child_results => #{}}),
            ?EBT_RESULT_FINISHED;
        Result2 ->
            ?EBT_NODE_DATA(Id, Data#{child_results => Result2}),
            ?EBT_RESULT_RUNNING
    end.

% 验证所有子节点是否可行
check_child([]) ->
    true;
check_child([#ebt_node{mod = Mod} = Node| Tail]) ->
    case Mod:evaluate(Node) of
        false ->
            false;
        true ->
            check_child(Tail)
    end.

do_and_tick([], _Results, 0) ->
    done;
do_and_tick([], Results, RunningCount) when RunningCount > 0 ->
    Results;
do_and_tick([#ebt_node{mod = ChildMod, id = ChildRef} = ChildNode | Tail], Results, RunningCount) ->
    case maps:get(ChildRef, Results, ?EBT_RESULT_RUNNING) of
        ?EBT_RESULT_RUNNING ->
            Result = ChildMod:tick(ChildNode),
            Results2 = Results#{ ChildRef => Result},

            case Result of
                ?EBT_RESULT_RUNNING ->
                    do_and_tick(Tail, Results2, RunningCount);
                ?EBT_RESULT_FINISHED ->
                    do_and_tick(Tail, Results2, RunningCount - 1)
            end;
        ?EBT_RESULT_FINISHED ->
            do_and_tick(Tail, Results, RunningCount - 1)
    end.
