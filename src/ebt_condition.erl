%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc　条件节点
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_condition).

-ebt_extend(ebt_node).

-include("ebt.hrl").
-include("ebt_transform.hrl").

%% API
-export([tick/1]).

%% API
-compile([export_all]).

%%====================================================================
%% behaviour callback
%%====================================================================
%% 条件验证（验证是否结束）
-callback do_check(Node :: ebt_node()) -> boolean().

%%====================================================================
%% API functions
%%====================================================================
tick(#ebt_node{mod = Mod} = Node) ->
    case Mod:do_check(Node) of
        true ->
            ?EBT_RESULT_FINISHED;
        false ->
            ?EBT_RESULT_RUNNING
    end.

%%====================================================================
%% condition callback
%%====================================================================
do_check(#ebt_node{id = Id, mod = Mod} = Node) ->
    io:format("(~p, ~p) check:, ~p ~n", [Id, Mod, Node]),
    true.