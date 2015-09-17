%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc 行为树节点基类模块
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_node).

-include("ebt.hrl").

%% API
-export([init/1, precondition/1, evaluate/1, do_evaluate/1, tick/1, clear/1]).

%%====================================================================
%% behaviour callback
%%====================================================================
%% 初始化节点需要的数据或者节点状态等
-callback init(Node :: ebt_node()) -> NewNode :: ebt_node().

%% 准入条件
-callback precondition(Node :: ebt_node()) -> boolean().

%% 验证检查
-callback evaluate(Node :: ebt_node()) -> boolean().

%% 节点的个性化检查
-callback do_evaluate(Node :: ebt_node()) -> boolean().

%% 执行逻辑
-callback tick(Node :: ebt_node()) -> ebt_result().

%% 清除
-callback clear(Node :: ebt_node()) -> ok.

%%====================================================================
%% API functions
%%====================================================================
%% @doc 节点初始化
-spec init(Node :: ebt_node()) -> Node :: ebt_node().
init(#ebt_node{id = Id, static_fields = StaticFields} = Node) ->
    Data = ?EBT_NODE_DATA(Id),

    DataNew =
        lists:foldl(
            fun
                ({FieldName, FieldValue}, DataAcc) when is_atom(FieldName) ->
                    % use ':=' ensure no conflict fields
                    DataAcc#{FieldName := FieldValue};
                (_Fields, DataAcc) ->
                    % todo: ensure FieldName is atom?
                    DataAcc
            end, Data, StaticFields),

    ?EBT_NODE_DATA(Id, DataNew),

    Node.

%% @doc 是否准入判断
-spec precondition(Node :: ebt_node()) -> Result :: boolean().
precondition(#ebt_node{precondition = {PreconditionMod, PreconditionFun}} = Node) when is_atom(PreconditionMod), is_atom(PreconditionFun) ->
    PreconditionMod:PreconditionFun(Node);
precondition(#ebt_node{precondition = Precondition} = Node) when is_function(Precondition, 1) ->
    Precondition(Node);
precondition(#ebt_node{precondition = undefined}) ->
    true;
precondition(#ebt_node{precondition = Precondition} = Node) when is_atom(Precondition) ->
    Precondition:do_check(Node).

%% @doc 评估是否执行
-spec evaluate(Node :: ebt_node()) -> Result :: boolean().
evaluate(#ebt_node{mod = Mod} = Node) ->
    Mod:precondition(Node) andalso Mod:do_evaluate(Node).

%% @doc 执行验证
-spec do_evaluate(Node :: ebt_node()) -> Result :: boolean().
do_evaluate(#ebt_node{}) ->
    true.

%% @doc 执行节点操作
-spec tick(Node :: ebt_node()) -> Result :: ebt_result().
tick(#ebt_node{}) ->
    ?EBT_RESULT_FINISHED.

%% @doc 清除操作
-spec clear(Node :: ebt_node()) -> ok.
clear(#ebt_node{id = Id, childs = Childs}) ->
    ?EBT_CLEAR_NODE_DATA(Id),

    lists:foreach(
        fun(#ebt_node{mod = Mod} = Node) ->
            Mod:clear(Node)
        end, Childs),

    ok.
