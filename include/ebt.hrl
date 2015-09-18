%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%%-------------------------------------------------------------------
-ifndef(EBT_HRL).
-define(EBT_HRL, true).

-define(EBT_NODE_NAME, ebt_node).
-define(EBT_TREE_NAME, ebt_tree).

-define(EBT_RESULT_FINISHED, 'ebt_result_finished').            % 节点执行结果 - 结束
-define(EBT_RESULT_RUNNING, 'ebt_result_running').              % 节点执行结果 - 运行中
-type ebt_result() :: ?EBT_RESULT_FINISHED | ?EBT_RESULT_RUNNING.

-define(EBT_ACTION_FINISHED, 'ebt_action_finished').            % 行为状态 - 结束
-define(EBT_ACTION_RUNNING, 'ebt_action_running').              % 行为状态 - 运行中
-type ebt_action_state() :: ?EBT_ACTION_RUNNING | ?EBT_ACTION_FINISHED.

%% 行为树节点定义
-record(ebt_node, {
    id :: reference(),                              % 节点的唯一标识引用

    mod :: atom(),                                  % 节点业务逻辑模块

    precondition :: undefined | {module(), atom()} | atom() | fun((ebt_node()) -> boolean()),  % 节点准入条件,

    childs = [] :: list(ebt_node())                 % 节点的孩子节点
}).
-type ebt_node() :: #ebt_node{}.

-record(ebt_tree, {
    name :: string(),               % 行为树名称
    node :: ebt_node()              % 行为树节点
}).
-type ebt_tree() :: #ebt_tree{}.

-define(EBT_NODE_DATA(UidRef),
    case get(UidRef) of
        undefined -> #{};
        V -> V
    end).                                                   % 获取节点数据
-define(EBT_NODE_DATA(UidRef, Data), put(UidRef, Data)).    % 设置节点数据
-define(EBT_CLEAR_NODE_DATA(UidRef), erase(UidRef)).        % 清除节点数据

-define(EBT_NODE_MOD(Node, Mod), Node#ebt_node{mod = Mod}).
-define(EBT_SEL(Node), ?EBT_NODE_MOD(Node, ebt_priority_selector)).
-define(EBT_SEQ(Node), ?EBT_NODE_MOD(Node, ebt_sequence)).
-define(EBT_PAR(Node), ?EBT_NODE_MOD(Node, ebt_parallel)).

-endif. %% EBT_HRL