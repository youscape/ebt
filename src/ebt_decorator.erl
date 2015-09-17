%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc　装饰节点, 其孩子节点只能有一个
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_decorator).

-ebt_extend(ebt_node).

-include("ebt.hrl").
-include("ebt_transform.hrl").

%% API
-export([tick/1]).

%%====================================================================
%% behaviour callback
%%====================================================================
%% 条件验证（验证是否结束）
-callback do_decorate(Node :: ebt_node()) -> ebt_result().

%%====================================================================
%% API functions
%%====================================================================
tick(#ebt_node{} = Node) ->
    do_decorate(Node).

%%====================================================================
%% decorator callback
%%====================================================================
do_decorate(#ebt_node{id = Id, mod = Mod, childs = [#ebt_node{mod = ChildMod} = Child]} = Node) ->
    io:format("(~p, ~p) decorate:, ~p ~n", [Id, Mod, Node]),
    ChildMod:tick(Child).
