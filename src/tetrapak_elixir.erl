-module(tetrapak_elixir).

-behaviour(tetrapak_task_boot).
-export([tasks/1]).

-behaviour(tetrapak_task).
-export([run/2]).

-define(BUILDTASKS, [{"build:elixir", ?MODULE, "Compile Elixir modules"},
                     {"clean:erlang", ?MODULE, "Delete compiled Erlang/Elixir modules"}]).
-define(APPSRCTASK, [{"build:appfile", ?MODULE,"Generate the application resource file"}]).
-define(ERLANGTASK, [{"build:erlang", ?MODULE, "Donn't compile Erlang modules"}]).
-define(DEFAULTERLANGTASK, [{"build:erlang", tetrapak_task_erlc, "Compile Erlang modules", [{run_before, ["build:elixir"]}]}]).

-define(Mix,'Elixir-Mix').
-define(MixCLI, 'Elixir-Mix-CLI').
-define(ElixirCompiler, 'Elixir-Kernel-ParallelCompiler').

tasks(tasks) ->
    BuildTask = task_def(elixir_source_files() =/= [], ?BUILDTASKS),
    AppsrcTask = task_def(filelib:is_file(tetrapak:path("mix.exs")), ?APPSRCTASK),
    BuildErlang = case length(BuildTask) > 0 of
                      true -> task_def(src_not_exists() andalso length(AppsrcTask) == 1, ?ERLANGTASK, ?DEFAULTERLANGTASK);
                      false -> []
                  end,
    BuildTask ++ AppsrcTask ++ BuildErlang;

tasks(before_app_exists_tasks) ->
    [].

run("build:erlang", _) ->
    done;

run("build:appfile", _) ->
    done;

run("build:elixir", _) ->
    run_mix([<<"compile">>]);
    %Fun = fun(Name) ->
    %              io:format("Compiled ~s~n", [Name])
    %      end,
    %Binarys = [list_to_binary(FileName) || FileName <- elixir_source_files()],
    %?ElixirCompiler:files_to_path(Binarys, list_to_binary(tetrapak:path("ebin")), Fun),

run("clean:erlang", _) ->
    run_mix([<<"clean">>]).

% --------------------------------------------------------------------------------------------------
% -- helpers

run_mix(Args) ->
    application:start(elixir),
    ?Mix:start(),
    run_mix_cmd(Args),
    done.

task_def(Check, Definition) ->
    task_def(Check, Definition, []).

task_def(true, Definition, _Default) -> Definition;
task_def(false, _Definition, Default) -> Default.

src_not_exists() ->
    filelib:is_dir(tetrapak:path("src")) == false.

elixir_source_files() ->
    elixir_source_files([tetrapak:path("src"), tetrapak:path("lib")]).

elixir_source_files(Pathes) ->
    lists:flatmap(fun(Path) ->
                          tpk_file:match_files(Path, "\\.ex$")
                  end, Pathes).

run_mix_cmd(Args) ->
    ?MixCLI:run(Args).
