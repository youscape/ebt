%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc　行为节点
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_action).

-ebt_extend(ebt_node).

-include("ebt.hrl").
-include("ebt_transform.hrl").

%% API
-export([tick/1, clear/1]).

%% actions
-export([do_enter/1, do_execute/1, do_exit/1]).

%%====================================================================
%% behaviour callback
%%====================================================================

%% 行为执行初始化
-callback do_enter(Node :: ebt_node()) -> ok.

%% 行为具体逻辑
-callback do_execute(Node :: ebt_node()) -> ebt_action_state().

%% 行为执行结束回调
-callback do_exit(Node :: ebt_node()) -> ok.

%%====================================================================
%% API functions
%%====================================================================
tick(#ebt_node{id = Id, mod = ActionMod} = Node) ->
    Data = ?EBT_NODE_DATA(Id),

    Data2 =
        case maps:get(action_state, Data, ?EBT_ACTION_FINISHED) of
            ?EBT_ACTION_FINISHED ->
                ActionMod:do_enter(Node),
                Data#{action_state => ?EBT_ACTION_RUNNING};
            ?EBT_ACTION_RUNNING ->
                Data
        end,

    case ActionMod:do_execute(Node) of
        ?EBT_RESULT_FINISHED ->
            ActionMod:do_exit(Node),
            DataNew = Data2#{action_state => ?EBT_ACTION_FINISHED},
            ?EBT_NODE_DATA(Id, DataNew),

            ?EBT_RESULT_FINISHED;
        ?EBT_RESULT_RUNNING ->
            ?EBT_RESULT_RUNNING
    end.

clear(#ebt_node{id = Id, mod = ActionMod} = Node) ->
    Data = ?EBT_NODE_DATA(Id),
    case maps:get(action_state, Data, ?EBT_ACTION_FINISHED) of
        ?EBT_ACTION_FINISHED ->
            ok;
        ?EBT_ACTION_RUNNING ->
            ActionMod:do_exit(Node),
            ebt_node:clear(Node)
    end.

%%====================================================================
%% action callback
%%====================================================================
do_enter(#ebt_node{id = Id, mod = Mod} = Node) ->
    io:format("(~p, ~p) enter:, ~p ~n", [Id, Mod, Node]),
    ok.

do_execute(#ebt_node{}) ->
    ?EBT_RESULT_RUNNING.

do_exit(#ebt_node{id = Id, mod = Mod} = Node) ->
    io:format("(~p, ~p) exit:, ~p ~n", [Id, Mod, Node]),
    ok.