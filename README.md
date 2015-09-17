bt
=====

Erlang behaviour tree framework.

基于进程字典来是实现类变量, parse_transform实现类方法的继承

```erlang
%% 控制节点，从左到右遍历子节点，若子节点的准入条件符合信息则执行该子节点
-define(BT_SELECTOR, bt_selector).

%% 行为节点，具体的游戏逻辑放在这个节点里面
-define(BT_ACTION, bt_action).                         

%% 控制节点，从左到右顺序执行子节点，并仅在一个子节点执行完成后才执行下一个子节点
-define(BT_SEQUENCE, bt_sequence).

%% 控制节点，同时执行各个子节点，若任一子节点准入条件失败则结束
-define(BT_PARALLEL, bt_parallel).                     

%% 条件节点(未实现)
-define(BT_CONDITION, bt_condition).

%% 装饰节点(未实现)
-define(BT_DECORATOR, bt_decorator).
```

Build
-----
    $ make
    
Usage
-----
### 节点实现
    
在要实现的模块中加入 `-bt_extend(BtNodeType).` 

**BtNodeType** 的值为`bt_action`, `bt_sequence`, `bt_parallel`, `bt_selector`, `bt_condition`, `bt_decorator`

`bt_extend` 属性主要实现对`bt_node.erl`的五个方法:
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
-record(bt_node, {
    mod,

    id :: reference(),                              % 节点的唯一标识引用

    precondition :: fun((bt_node()) -> boolean()),  % 准入条件,

    childs = []
}).
-type bt_node() :: #bt_node{}.

```

使用 `bt_node` 记录构造行为树, 例子如下:

```erlang
Node = #bt_node{
    mod = bt_selector,
    childs = [
        #bt_node{
            mod = action_fight,
            precondition = fun bt_condition:check_fight/1
        },
        #bt_node{
            mod = action_idle,
            precondition = fun bt_condition:check_idle/1
        }
    ]
},
```

### 行为树运行

行为树运行之前需要使用 `bt_tree:init/1` 对行为树节点进行初始化并创建相应节点引用.
```erlang
{ok, InitedNode} = bt_tree:init(Node),
```
初始化后的行为树即可使用 `bt_tree:run/1` 运行
```erlang
bt_tree:run(InitedNode).
```

### 行为树销毁
```erlang
bt_tree:destroy(InitedNode).
```