%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc　顺序节点
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_sequence).

-ebt_extend(ebt_node).

-include("ebt.hrl").
-include("ebt_transform.hrl").

%% API
-export([do_evaluate/1, tick/1]).

do_evaluate(#ebt_node{id = Id, childs = [FirstChild | _Tail]}) ->
    Data = ?EBT_NODE_DATA(Id),

    case maps:get(left_childs, Data, []) of
        [] ->
            #ebt_node{mod = FirstChildMod} = FirstChild,
            FirstChildMod:evaluate(FirstChild);
        [#ebt_node{mod = ActiveChildMod} = ActiveChild | _LeftChilds] ->
            case ActiveChildMod:evaluate(ActiveChild) of
                true ->
                    true;
                false ->
                    ActiveChildMod:clear(ActiveChild),

                    Data2 = maps:remove(left_childs, Data),
                    ?EBT_NODE_DATA(Id, Data2),
                    false
            end
    end.

tick(#ebt_node{id = Id, childs = Childs}) ->
    Data = ?EBT_NODE_DATA(Id),

    [#ebt_node{mod = ActiveChildMod} = ActiveChild | TailChilds] =
        LeftChilds = maps:get(left_childs, Data, Childs),

    case ActiveChildMod:tick(ActiveChild) of
        ?EBT_RESULT_FINISHED ->
            ActiveChildMod:clear(ActiveChild),
            case TailChilds of
                [] ->
                    Data2 = maps:remove(left_childs, Data),
                    ?EBT_NODE_DATA(Id, Data2),
                    ?EBT_RESULT_FINISHED;
                TailChilds ->
                    Data2 = Data#{left_childs => TailChilds},
                    ?EBT_NODE_DATA(Id, Data2),
                    ?EBT_RESULT_RUNNING
            end;
        ?EBT_RESULT_RUNNING ->
            Data2 = Data#{left_childs => LeftChilds},
            ?EBT_NODE_DATA(Id, Data2),
            ?EBT_RESULT_RUNNING
    end.

