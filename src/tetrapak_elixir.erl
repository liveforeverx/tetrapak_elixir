-module(tetrapak_elixir).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([run/2, check/1]).

-include_lib("kernel/include/file.hrl").

-define(BUILDTASKS, [{"build:elixir", ?MODULE, "Compile Elixir modules"}]).
-define(APPSRCTASK, [{"build:appfile", ?MODULE,"Generate the application resource file"}]).
-define(ERLANGTASK, [{"build:erlang", ?MODULE, "Donn't compile Erlang modules"}]).
-define(DEFAULTERLANGTASK, [{"build:erlang", tetrapak_task_erlc, "Compile Erlang modules"}]).

-define(ElixirCompiler, 'Elixir.Kernel.ParallelCompiler').
-define(ElixirCode, 'Elixir.Code').

app() ->
    App = elixir_source_files() orelse filelib:is_file(tetrapak:path("mix.exs")),
    {App, App}.

tasks(tasks) ->
    BuildTask = task_def(elixir_source_files() =/= [], ?BUILDTASKS),
    AppsrcTask = task_def(filelib:is_file(tetrapak:path("mix.exs")), ?APPSRCTASK),
    BuildErlang = case length(BuildTask) > 0 of
                      true -> task_def(src_not_exists(), ?ERLANGTASK, ?DEFAULTERLANGTASK);
                      false -> []
                  end,
    BuildTask ++ AppsrcTask ++ BuildErlang;

tasks(before_app_exists_tasks) ->
    [].

check("build:elixir") ->
    Sources = tpk_file:wildcard("lib", "**/*.ex"),
    io:format(user, "sources: ~p~n", [Sources]),
    AllCompiled = [mtime(File) || File <-tpk_file:wildcard("ebin", "*.beam")],
    SourceMTimes = [mtime(File) || File <- Sources],
    Filter = fun(CMTime) -> [MTime || MTime <- SourceMTimes, CMTime < MTime] =/= [] end,
    case lists:filter(Filter, AllCompiled) == [] andalso AllCompiled =/= [] of
        true -> done;
        _ -> {needs_run, Sources}
    end.

run("build:erlang", _) ->
    done;

run("build:appfile", _) ->
    done;

run("build:elixir", StringFiles) ->
    application:start(elixir),
    file:make_dir(tetrapak:path("ebin")),
    CompileOptions = tetrapak:config("build.elixirc_options", []),
    ?ElixirCode:compiler_options([{ignore_module_conflict, true} | CompileOptions]),
    Files = [list_to_binary(File) || File <- StringFiles],
    BaseDir = tetrapak:dir(),
    try
        ?ElixirCompiler:files_to_path(Files, <<"ebin">>, [{each_file, fun(File) ->
                                                                              Relative = tpk_file:relative_path(binary_to_list(File), BaseDir),
                                                                              io:format("Compiling ~s~n", [Relative])
                                                                      end}]),
        done
    catch
        _:Catch ->
            tetrapak:fail("** (~s) ~s", [Catch:'__record__'(name), Catch:message()])
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

mtime(File) ->
    case file:read_file_info(File) of
        {error, _} -> {{1970, 1, 1}, {0, 0, 0}};
        {ok, #file_info{mtime = MTime}} -> MTime
    end.
