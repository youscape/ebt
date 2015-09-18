%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc 并行节点, 当所有孩子节点准入时执行，当有一个孩子节点执行失败时退出
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_parallel).

-ebt_extend(ebt_node).

-include("ebt.hrl").
-include("ebt_transform.hrl").

%% API
-export([do_evaluate/1, tick/1]).

do_evaluate(#ebt_node{childs = Childs}) ->
    check_child(Childs).

tick(#ebt_node{childs = Childs}) ->
    do_or_tick(Childs).

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

do_or_tick([]) ->
    ?EBT_RESULT_RUNNING;
do_or_tick([#ebt_node{mod = ChildMod} = ChildNode | Tail]) ->
    case ChildMod:tick(ChildNode) of
        ?EBT_RESULT_RUNNING ->
            do_or_tick(Tail);
        ?EBT_RESULT_FINISHED ->
            ?EBT_RESULT_FINISHED
    end.
