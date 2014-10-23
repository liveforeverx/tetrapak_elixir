-module(tetrapak_elixir).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([run/2, check/1]).

-include_lib("kernel/include/file.hrl").

-define(BUILDTASKS, [{"build:elixir", ?MODULE, "Compile Elixir modules"}]).
-define(APPSRCTASK, [{"build:appfile", ?MODULE,"Generate the application resource file"}]).
-define(ERLANGTASK, [{"build:erlang", ?MODULE, "Donn't compile Erlang modules"}]).
-define(CORETASK,   [{"build:core_elixir", ?MODULE, "Compile core Elixir modules", [ {run_before, ["build:elixir"]}] }]).
-define(DEFAULTERLANGTASK, [{"build:erlang", tetrapak_task_erlc, "Compile Erlang modules"}]).

-define(ElixirCompiler, 'Elixir.Mix.Compilers.Elixir').
-define(ElixirProject, 'Elixir.Mix.ProjectStack').
-define(ElixirCode, 'Elixir.Code').

app() ->
    App = elixir_source_files() orelse filelib:is_file(tetrapak:path("mix.exs")),
    {App, App}.

tasks(tasks) ->
    CompilerFile = filename:join(["src", "elixir_compiler.erl"]),
    case filelib:is_file(CompilerFile) of
        true ->
            ?BUILDTASKS ++ ?CORETASK;
        false ->
            BuildTask   = task_def(elixir_source_files() =/= [], ?BUILDTASKS),
            AppsrcTask  = task_def(filelib:is_file(tetrapak:path("mix.exs")), ?APPSRCTASK),
            BuildErlang = task_def(length(BuildTask) > 0, task_def(src_not_exists(), ?ERLANGTASK, ?DEFAULTERLANGTASK)),
            BuildTask ++ AppsrcTask ++ BuildErlang
    end;

tasks(before_app_exists_tasks) ->
    [].

check("build:core_elixir") ->
    application:set_env(tetrapak_elixir, force_build, true),
    {needs_run, tetrapak_elixir_core:core_main()}.

run("build:erlang", _) ->
    done;

run("build:appfile", _) ->
    done;

run("build:core_elixir", Files) ->
    tetrapak:require("build:appfile"),
    application:ensure_all_started(elixir),
    elixir_code_server:cast({ compiler_options, [{docs,false},{internal,true}] }),
    [try
         Lists = elixir_compiler:file(File),
         [tetrapak_elixir_core:binary_to_path(X, "ebin") || X <- Lists],
         io:format("Compiled ~ts~n", [File])
     catch
         Kind:Reason ->
             tetrapak:fail("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, erlang:get_stacktrace()])
     end || File <- Files],
    done;

run("build:elixir", _) ->
    application:ensure_all_started(mix),
    file:make_dir(tetrapak:path("ebin")),
    CompileOptions = tetrapak:config("build.elixirc_options", []),
    try
        'Elixir.Code':load_file(<<"mix.exs">>) of
            _ -> ok
    catch
        error:_ErrorMap1 ->
            % TODO: erlang .app.src compatibility
            ok
    end,
    ?ElixirCode:compiler_options([{ignore_module_conflict, true} | CompileOptions]),
    ?ElixirProject:start_link(),
    try ?ElixirCompiler:compile([<<".compile.elixir">>], [<<"lib">>], [ex], <<"ebin">>, true, fun() -> ok end) of
        ok ->
            done
    catch
        error:ErrorMap2 ->
            #{message := Message} = ErrorMap2,
            tetrapak:fail(Message)
    end.


% --------------------------------------------------------------------------------------------------
% -- helpers

task_def(Check, Definition) ->
    task_def(Check, Definition, []).

task_def(true, Definition, _Default) -> Definition;
task_def(false, _Definition, Default) -> Default.

src_not_exists() ->
    tpk_file:exists_in("src", "*.erl") == false.

elixir_source_files() ->
    tpk_file:exists_in("lib", "*.ex").
