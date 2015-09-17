%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc 行为树
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_tree).

-include("ebt.hrl").

%% API
-export([init/1, run/1, destroy/1]).

%% (数据)初始化
-callback init(Root :: ebt_node())-> {ok, RootNew :: ebt_node()}.

%% 运行行为树
-callback run(Node :: ebt_node()) -> ok.

%% 行为树销毁
-callback destroy() -> ok.

init(#ebt_node{} = Root) ->
    {ok, init_node(Root)}.

init_node(#ebt_node{mod = RootMod, childs = Childs} = Root) ->
    RootMod:init(Root#ebt_node{id = make_ref(), childs = [init_node(Child) || #ebt_node{} = Child <- Childs]}).

run(#ebt_node{mod = Mod} = Node) ->
    case Mod:evaluate(Node) of
        true ->
            Mod:tick(Node);
        false ->
            false
    end.

destroy(#ebt_node{mod = Mod} = Node) ->
    Mod:clear(Node).
