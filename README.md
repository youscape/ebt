ebt
=====

Erlang Behaviour Tree Framework.

基于进程字典来是实现类变量, parse_transform实现类方法的继承.

基于 **ys_bt**[https://git.oschina.net/youscape/ys_bt.git] 修改，原项目不再更新维护。

```erlang
%% 优先级选择节点，从左到右遍历子节点，若子节点的准入条件符合信息则执行该子节点
-define(EBT_SELECTOR, ebt_priority_selector).

%% 行为节点，具体的游戏逻辑放在这个节点里面
-define(EBT_ACTION, ebt_action).                         

%% 顺序节点，从左到右顺序执行子节点，并仅在一个子节点执行完成后才执行下一个子节点
-define(EBT_SEQUENCE, ebt_sequence).

%% 并行节点，当所有子节点准入，同时执行各个子节点，若任一子节点执行失败则结束
-define(EBT_PARALLEL, ebt_parallel).
                     
%% 并行节点，当所有子节点准入，同时执行各个子节点，若所有节点执行失败则结束
-define(EBT_PARALLEL2, ebt_parallel2).                     

%% 条件节点
-define(EBT_CONDITION, ebt_condition).

%% 装饰节点
-define(EBT_DECORATOR, ebt_decorator).
```

Build
-----
    $ make
    
Usage
-----
### 节点实现
    
在要实现的模块中加入 `-ebt_extend(BtNodeType).` 

**BtNodeType** 的值为`ebt_action`, `ebt_sequence`, `ebt_parallel`, `ebt_parallel2`, `ebt_priority_selector`, `ebt_condition`, `ebt_decorator`

`ebt_extend` 属性主要实现对`ebt_node.erl`的五个方法:
- `init/1`
- `precondition/1`
- `evaluate/1`
- `do_evaluate/1`
- `tick/1`
- `clear/1`

对应 `BtNodeType` 模块的继承,并自动导入 `BtNodeType` 的 `behavior` 属性.

### 行为树构造

```erlang
%% 行为树节点定义
-record(ebt_node, {
    id :: reference(),                              % 节点的唯一标识引用

    mod :: atom(),                                  % 节点业务逻辑模块

    precondition :: undefined | {module(), atom()} | atom() | fun((ebt_node()) -> boolean()),  % 节点准入条件,

    childs = [] :: list(ebt_node())                 % 节点的孩子节点
}).
-type ebt_node() :: #ebt_node{}.

```

使用 `ebt_node` 记录构造行为树, 例子如下:

```erlang
Node = #ebt_node{
    mod = ebt_priority_selector,
    childs = [
        #ebt_node{
            mod = action_fight,
            precondition = fun ebt_condition:check_fight/1
        },
        #ebt_node{
            mod = action_idle,
            precondition = fun ebt_condition:check_idle/1
        }
    ]
},
```

### 行为树运行

行为树运行之前需要使用 `ebt_tree:init/1` 对行为树节点进行初始化并创建相应节点引用.
```erlang
{ok, InitedNode} = ebt_tree:init(Node),
```
初始化后的行为树即可使用 `ebt_tree:run/1` 运行
```erlang
ebt_tree:run(InitedNode).
```

### 行为树销毁
```erlang
ebt_tree:destroy(InitedNode).
```