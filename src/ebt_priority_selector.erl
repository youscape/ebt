%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc　优先级选择节点
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_priority_selector).

-ebt_extend(ebt_node).

-include("ebt.hrl").

-include("ebt_transform.hrl").

%% API
-export([do_evaluate/1, tick/1]).

do_evaluate(#ebt_node{id = Id, childs = Childs}) ->
    Data = ?EBT_NODE_DATA(Id),
    LastActiveNode = maps:get(active_child, Data, undefined),
    case select(Childs, LastActiveNode) of
        false ->
            false;
        {true, ActiveChild} ->
            Data2 = Data#{active_child => ActiveChild},
            ?EBT_NODE_DATA(Id, Data2),
            true
    end.

tick(#ebt_node{id = Id}) ->
    Data = ?EBT_NODE_DATA(Id),
    case maps:get(active_child, Data, undefined) of
        undefined ->
            ?EBT_RESULT_FINISHED;
        #ebt_node{mod = Mod} = ChildNode ->
            case Mod:tick(ChildNode) of
                ?EBT_RESULT_FINISHED ->
                    Mod:clear(ChildNode),
                    Data2 = maps:remove(active_child, Data),
                    ?EBT_NODE_DATA(Id, Data2),
                    ?EBT_RESULT_FINISHED;
                ?EBT_RESULT_RUNNING ->
                    ?EBT_RESULT_RUNNING
            end
    end.

% @private 选择可进入子节点
select([], _LastActiveNode) ->
    false;
select([#ebt_node{mod = Mod} = Node| Tail], LastActiveNode) ->
    case Mod:evaluate(Node) of
        true ->
            case Node =/= LastActiveNode of
                true ->
                    case LastActiveNode of
                        #ebt_node{mod = LastMod} ->
                            LastMod:clear(LastActiveNode);
                        undefined ->
                            ok
                    end;
                false -> ok
            end,
            {true, Node};
        false ->
            select(Tail, LastActiveNode)
    end.
