-module(tetrapak_elixir).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([run/2]).

-define(BUILDTASKS, [{"build:elixir", ?MODULE, "Compile Elixir modules", [{run_before, ["build:erlang"]}] },
                     {"clean:erlang", ?MODULE, "Delete compiled Erlang/Elixir modules"}]).
-define(APPSRCTASK, [{"build:appfile", ?MODULE,"Generate the application resource file"}]).
-define(ERLANGTASK, [{"build:erlang", ?MODULE, "Donn't compile Erlang modules"}]).
-define(DEFAULTERLANGTASK, [{"build:erlang", tetrapak_task_erlc, "Compile Erlang modules"}]).

-define(Mix,'Elixir-Mix').
-define(MixCLI, 'Elixir-Mix-CLI').
-define(ElixirCompiler, 'Elixir-Kernel-ParallelCompiler').

app() ->
    App = elixir_source_files() orelse filelib:is_file(tetrapak:path("mix.exs")),
    {App, App}.

tasks(tasks) ->
    BuildTask = task_def(elixir_source_files() =/= [], ?BUILDTASKS),
    AppsrcTask = task_def(filelib:is_file(tetrapak:path("mix.exs")), ?APPSRCTASK),
    BuildErlang = case length(BuildTask) > 0 of
                      true -> task_def(src_not_exists() andalso length(AppsrcTask) == 1, ?ERLANGTASK, ?DEFAULTERLANGTASK);
                      false -> []
                  end,
    BuildTask ++ AppsrcTask ++ BuildErlang;

tasks(before_app_exists_tasks) ->
    [{"new:elixir", ?MODULE, "Generate a skeleton for a new Elixir application"}].

run("build:erlang", _) ->
    done;

run("build:appfile", _) ->
%    run_mix([<<"compile">>]);
    done;

run("build:elixir", _) ->
    run_mix([<<"compile">>]);
    %Fun = fun(Name) ->
    %              io:format("Compiled ~s~n", [Name])
    %      end,
    %Binarys = [list_to_binary(FileName) || FileName <- elixir_source_files()],
    %?ElixirCompiler:files_to_path(Binarys, list_to_binary(tetrapak:path("ebin")), Fun),

run("clean:erlang", _) ->
    run_mix([<<"clean">>]);

run("new:elixir", _) ->
    case init:get_argument(app) of
        {ok, [[Name]]} ->
            run_mix([<<"new">>, list_to_binary(Name)]);
        _ ->
            io:format("No name specified, use -app option~n", [])
    end.

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
    tpk_file:exists_in("src", ".erl") == false.

elixir_source_files() ->
    tpk_file:exists_in("src", ".ex") orelse tpk_file:exists_in("lib", ".ex").

run_mix_cmd(Args) ->
    ?MixCLI:run(Args).
