-module(tetrapak_elixir_core).
-compile([export_all]).

core_main() ->
    [list_to_binary(filename:join(["lib" | List])) || List <- core()].

core() ->
    [["kernel.ex"],
     ["macro", "env.ex"],
     ["keyword.ex"],
     ["module.ex"],
     ["list.ex"],
     ["macro.ex"],
     ["code.ex"],
     ["module", "locals_tracker.ex"],
     ["kernel", "typespec.ex"],
     ["exception.ex"],
     ["protocol.ex"],
     ["stream", "reducers.ex"],
     ["enum.ex"],
     ["inspect", "algebra.ex"],
     ["inspect.ex"],
     ["range.ex"],
     ["regex.ex"],
     ["string.ex"],
     ["string", "chars.ex"],
     ["io.ex"],
     ["path.ex"],
     ["file.ex"],
     ["system.ex"],
     ["kernel", "cli.ex"],
     ["kernel", "error_handler.ex"],
     ["kernel", "parallel_compiler.ex"],
     ["kernel", "lexical_tracker.ex"]].

binary_to_path({ModuleName, Binary}, CompilePath) ->
    Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
    ok = file:write_file(Path, Binary),
    Path.
