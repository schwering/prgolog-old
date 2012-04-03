%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% File: osi.m.
% Main author: schwering.
%
% Interface to OSI (Open Solver Interface), particularly to the COIN-OR CLP
% solver.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%

:- module osi.
:- interface.

:- import_module list.
:- import_module assoc_list.
:- import_module pair.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type coeff == pair(var, float).

:- type operator ---> (=<) ; (=) ; (>=).

:- type constraint ---> cstr(list(coeff), operator, float).

:- type direction ---> max ; min.

:- type objective == list(coeff).

:- type result
    --->    unsatisfiable
    ;       satisfiable(float, assoc_list(var, float)).

%-----------------------------------------------------------------------------%

:- func construct_constraint(list(coeff)::in, operator::in, float::in)
    = (constraint::out) is det.

:- pred holds_trivially(constraint::in) is semidet.
:- pred fails_trivially(constraint::in) is semidet.

:- func solve(list(constraint)::in, direction::in, objective::in, varset::in)
           = (result::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module enum.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module require.

%-----------------------------------------------------------------------------%

construct_constraint(Sum, Op, Bnd) = cstr(Sum, Op, Bnd).


holds_trivially(cstr([], (=), 0.0)).
holds_trivially(cstr([], (>=), Bnd)) :- Bnd =< 0.0.
holds_trivially(cstr([], (=<), Bnd)) :- Bnd >= 0.0.


fails_trivially(cstr([], (=), Bnd)) :- Bnd \= 0.0.
fails_trivially(cstr([], (>=), Bnd)) :- Bnd > 0.0.
fails_trivially(cstr([], (=<), Bnd)) :- Bnd < 0.0.

%-----------------------------------------------------------------------------%

solve(Cstrs, _Dir, _Obj, VS) = R :-
    if      Cstrs = []
    then    R = satisfiable(0.0, [])
    else if one_fails_trivially(Cstrs)
    then    R = unsatisfiable
    else    some [!SC] (
                N = var_id(max_var(VS)),
                new_solver_context(N, !:SC),
                add_constraints(Cstrs, !SC),
                solve(Optimal, ObjValue, VarValues, !.SC, _)
            ),
            (   if      Optimal \= 0
                then    VarMap = list_to_map(VarValues),
                        R = satisfiable(ObjValue, VarMap)
                else    R = unsatisfiable
            ).


:- pred one_fails_trivially(list(constraint)::in) is semidet.

one_fails_trivially([C | Cs]) :-
    (   fails_trivially(C)
    ;   not fails_trivially(C), one_fails_trivially(Cs)
    ).


:- func var_to_int_id(var) = int is det.
:- func int_id_to_var(int) = var is det.

var_to_int_id(V) = var_id(V) - 1.
int_id_to_var(I) = V :-
    if      V0 = from_int(I + 1)
    then    V = V0
    else    error("cannot convert this int to var").


:- func list_to_map(list(float)) = assoc_list(var, float).

list_to_map(Fs) = list_to_map_2(0, Fs).


:- func list_to_map_2(int, list(float)) = assoc_list(var, float).

list_to_map_2(_, []) = [].
list_to_map_2(I, [F|Fs]) = Fs0 :-
    Fs1 = list_to_map_2(I+1, Fs),
    (   if      F \= 0.0
        then    Fs0 = [(int_id_to_var(I) - F) | Fs1]
        else    Fs0 = Fs1
    ).


:- pred add_constraints(list(constraint)::in,
                        solver_context::di, solver_context::uo) is det.

add_constraints([], SC, SC).
add_constraints([cstr(Sum, Op, Bnd) | Cs], SC0, SC2) :-
    N = length(Sum),
    As = map((func(_ - C) = C), Sum),
    Vs = map((func(V - _) = var_to_int_id(V)), Sum),
    (   Op = (=<),  Cmp = 1
    ;   Op = (=),   Cmp = 0
    ;   Op = (>=),  Cmp = -1
    ),
    add_constraint(N, As, Vs, Cmp, Bnd, SC0, SC1),
    add_constraints(Cs, SC1, SC2).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include ""coin-clp.h""").

:- type solver_context.

:- pragma foreign_type("C", solver_context, "SolverContext*").


:- pred new_solver_context(int::in, solver_context::uo) is det.

:- pragma foreign_proc("C",
    new_solver_context(N::in, Ctx::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ctx = new_solver_context(N);
").


:- pred finalize_solver_context(solver_context::di) is det.

:- pragma foreign_proc("C",
    finalize_solver_context(Ctx::di),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    finalize_solver_context(Ctx);
").


:- pred add_constraint(int::in, list(float)::in, list(int)::in, int::in,
                       float::in, solver_context::di, solver_context::uo)
                       is det.

:- pragma foreign_proc("C",
    add_constraint(N::in, As::in, Vs::in, Cmp::in, Bnd::in, Ctx0::di, Ctx1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    double* as_arr = GC_MALLOC(N * sizeof(double));
    int* vs_arr = GC_MALLOC(N * sizeof(int));
    MR_Word as_list = As;
    MR_Word vs_list = Vs;
    int i;
    for (i = 0; i < N; ++i) {
        assert(!MR_list_is_empty(as_list));
        assert(!MR_list_is_empty(vs_list));
        as_arr[i] = MR_word_to_float(MR_list_head(as_list));
        vs_arr[i] = (MR_Integer) MR_list_head(vs_list);
        vs_list = MR_list_tail(vs_list);
        as_list = MR_list_tail(as_list);
    }
    add_constraint(Ctx0, N, as_arr, vs_arr, Cmp, Bnd);

#if 0
    printf(""BLOCK {\\n"");

    printf(""    double* as_arr = (double*) GC_malloc(%d * sizeof(double));\\n"", N);
    printf(""    int* vs_arr = (int*) GC_malloc(%d * sizeof(int));\\n"", N);

    printf(""    for (int i = 0; i < %d; ++i) {\\n"", N);
    for (i = 0; i < N; ++i) {
        printf(""        as_arr[%d] = %lf;\\n"", i, as_arr[i]);
        printf(""        vs_arr[%d] = %d;\\n"", i, vs_arr[i]);
    }
    printf(""    };\\n"");

    printf(""    add_constraint(&ctx, %d, as_arr, vs_arr, %d, %lf);\\n"",
           N, Cmp, Bnd);
    printf(""    free(vs_arr);\\n"");
    printf(""    free(as_arr);\\n"");

    printf(""}\\n\\n"");
#endif

    GC_FREE(vs_arr);
    GC_FREE(as_arr);
    Ctx1 = Ctx0;
").


:- pred solve(int::out, float::out, list(float)::out,
              solver_context::di, solver_context::uo) is det.

:- pragma foreign_proc("C",
    solve(Optimal::out, ObjValue::out, VarValues::out, Ctx0::di, Ctx1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    double obj_val;
    double* var_values_arr = GC_MALLOC(varcnt(Ctx0) * sizeof(double));

    if (solve(Ctx0, &obj_val, var_values_arr)) {
        MR_Word var_values_list = MR_list_empty();
        int i;
        Optimal = (MR_Integer) 1;
        ObjValue = (MR_Float) obj_val;
        for (i = varcnt(Ctx0) - 1; i >= 0; --i) {
            var_values_list = MR_list_cons(
                MR_float_to_word((MR_Float) var_values_arr[i]),
                var_values_list);
        }
        VarValues = var_values_list;
    } else {
        Optimal = (MR_Integer) 0;
        ObjValue = (MR_Float) 0;
        VarValues = MR_list_empty();
    }
    finalize_solver_context(&Ctx0);
    GC_FREE(var_values_arr);

    Ctx1 = Ctx0;
").

%-----------------------------------------------------------------------------%
:- end_module osi.
%-----------------------------------------------------------------------------%
