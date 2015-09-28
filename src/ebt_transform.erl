%%%-------------------------------------------------------------------
%%% @author zyuyou yuyouchow@gmail.com
%%% @copyright (C) 2015, Y.S.
%%% @doc 实现行为树节点的继承
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_transform).

-include("ebt.hrl").

%% API
-export([parse_transform/2]).

-define(EBT_NODE_FUNCTIONS, [init, precondition, evaluate, do_evaluate, tick, clear]).
-define(EBT_ACTION_FUNCTIONS, [do_enter, do_execute, do_exit | ?EBT_NODE_FUNCTIONS]).
-define(EBT_EXTEND_TAG, ebt_extend).
-define(EBT_UNDEF, undefined).

%%====================================================================
%% transform
%%====================================================================
-ifndef(EBT_TEST).
-define(EBT_TEST, false).

parse_transform(Forms0, _Options) ->
    transform(Forms0).
-else.
parse_transform([{attribute, 1, file, _FilePath}, {attribute, _L, module, ModuleName} | _Tail] = Forms0, _Options) ->
    Forms2 = transform(Forms0),
    case ?EBT_TEST of
        false ->
            Forms2;
        true ->
            file:write_file(io_lib:format("transforms/~w.txt", [ModuleName]), io_lib:format("~p", [Forms2]));
        Dir ->
            file:write_file(io_lib:format("~s/~w.txt", [Dir, ModuleName]), io_lib:format("~p", [Forms2]))
    end.
-endif.

transform(Forms) ->
    case get_inherit_funs(Forms) of
        no_need ->
            Forms;
        {BaseModule, InheritFuns} ->
            put(?EBT_EXTEND_TAG, BaseModule),
            {ExportAttrExpr, FunExprs} = gen_funs_expr(InheritFuns, {[], []}),
            BehaviourAttrExpr = {attribute, 0, behaviour, BaseModule},
            Forms1 = add_attributes(Forms, [BehaviourAttrExpr, ExportAttrExpr]),
            erase(?EBT_EXTEND_TAG),
            add_new_funcs(Forms1, FunExprs)
    end.

%% get base-module and inherit-funs
get_inherit_funs(Forms) ->
    get_inherit_funs(Forms, {?EBT_UNDEF, ?EBT_NODE_FUNCTIONS}).

get_inherit_funs(_Forms, {_BasedClass, []}) ->
    no_need;
get_inherit_funs([], {?EBT_UNDEF, _Funs}) ->
    no_need;
get_inherit_funs([], {BasedClass, InheritFuns}) ->
    {BasedClass, InheritFuns};
get_inherit_funs([{attribute,_L, ?EBT_EXTEND_TAG, BasedClassName} | Tail], {?EBT_UNDEF, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClassName, InheritFuns});
get_inherit_funs([{function, _L, Name, 1, _ClauseBody} | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns -- [Name]});
get_inherit_funs([_H | Tail], {BasedClass, InheritFuns}) ->
    get_inherit_funs(Tail, {BasedClass, InheritFuns}).

%% gen inherit-funs exprs
gen_funs_expr([], {ExportAttrs, FunExprs}) ->
    {{attribute,0,export, ExportAttrs}, FunExprs};
gen_funs_expr([FunName | TailFuns], {ExportAttr, FunExprs}) ->
    FunExpr =
        {function,0,FunName, 1,
            [{clause,0, [{match,0, {record,0,?EBT_NODE_NAME,[]}, {var, 0,'Node'}}],
                [], [{call,0, {remote,0,{atom,0, get(?EBT_EXTEND_TAG)},{atom,0, FunName}}, [{var,0,'Node'}]}]}]},
    gen_funs_expr(TailFuns, {[{FunName, 1} | ExportAttr], [FunExpr | FunExprs]}).

add_attributes([{attribute,_,module,_}=F|Fs], Attrs) ->
    [F|Attrs++Fs];
add_attributes([F|Fs], Attrs) ->
    [F|add_attributes(Fs, Attrs)].

add_new_funcs([{eof,_}|_]=Fs, NewFs) ->
    NewFs ++ Fs;
add_new_funcs([F|Fs], Es) ->
    [F|add_new_funcs(Fs, Es)];
add_new_funcs([], NewFs) ->
    NewFs.