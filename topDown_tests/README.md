## Top-down README

After building the Docker container (see toplevel README), make sure you are on the `topDown_tests` branch:

    git checkout –track origin/topDown_tests

From the project directory (one above this one) run:

    ./topDown_tests/benchmark_script.sh

This will build HooglePlus with our top-down code, run it a few times against some handpicked queries (see the script), and log the results as files in this directory.

### Gritty details

This branch was forked from the `mj_popl_aec` branch. The only differences are:

- `src/HooglePlus/Synthesize.hs`: the main runner, contains the top-down DFS
- `src/Types/TopDown.hs`: contains the state type used in DFS
- addition of this `topDown_tests` folder
- the `Dockerfile` (it's just set up to switch to this branch instead)

The data flow in `src/HooglePlus/Synthesize.hs` is this:

1. `synthesize` has the goal, and it calls...
2. `dfsTop` with depth 3 (this is hardcoded right now), which uses `getUnifiedFunctions` to find the components unifying with the goal.
3. Each component is passed to `dfs` which does (2) and (3) recursively until either depth is 0 (in which case it returns if the component is ground) or until no components unify (which never actually happens).
4. Ground programs that solve the query are stored as strings and eventually passed back up to `dfsTop`.
5. For each of the toplevel components found in `dfsTop`, `dfsTop` filters for programs that contain both `arg0` and `arg1` (this is hardcoded right now) and picks the first one that does. This leaves one program for each of the toplevel components found.
6. These programs are returned to `synthesize` which prints them.



