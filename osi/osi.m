%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: osi.m.
% Main author: schwering.
%
% Interface to OSI (Open Solver Interface), particularly to the COIN-OR CLP
% solver.
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
    (   Op = (=<),  Cmp = -1
    ;   Op = (=),   Cmp = 0
    ;   Op = (>=),  Cmp = 1
    ),
    add_constraint(N, As, Vs, Cmp, Bnd, SC0, SC1),
    add_constraints(Cs, SC1, SC2).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
//#define STANDALONE
#ifdef STANDALONE
    #include ""coin-clp.h""
    typedef SolverContext* MercurySolverContext;
#else
    #define LP_MALLOC   GC_MALLOC
    #define LP_FREE     GC_FREE
    #include ""../lp-server/lp-sock.h""
    #include ""../lp-server/lp-msg.h""
    #define HOST ""localhost""
    typedef int MercurySolverContext;
#endif
").


:- pragma foreign_decl("C", "
    struct solver_context_node {
        pthread_t tid;
        int sockfd;
        struct solver_context_node *next;
    };
    extern struct solver_context_node *solver_context_head;
    extern pthread_mutex_t solver_context_mutex;
").
:- pragma foreign_code("C", "
    struct solver_context_node *solver_context_head = NULL;
    pthread_mutex_t solver_context_mutex = PTHREAD_MUTEX_INITIALIZER;
").

:- type solver_context.

:- pragma foreign_type("C", solver_context, "MercurySolverContext").


:- pred new_solver_context(int::in, solver_context::uo) is det.

:- pragma foreign_proc("C",
    new_solver_context(N::in, Ctx::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef STANDALONE
    Ctx = new_solver_context(N);
#else /* STANDALONE */
    const pthread_t self = pthread_self();
    struct solver_context_node *node;
    int sockfd = -1;
    Header h;
    void *payload;

    /* In most cases, the thread already has a connection, so we look for
     * it without a lock first.  If this fails, we prepare for creating a
     * connection.  This step is then protected with the mutex and at first
     * we need to re-check that no connection already exists.  If that's
     * the case, a connection is created. */

    for (node = solver_context_head; node != NULL; node = node->next) {
        if (pthread_equal(self, node->tid)) {
            sockfd = node->sockfd;
            break;
        }
    }

    if (sockfd < 0) {
        pthread_mutex_lock(&solver_context_mutex);
        for (node = solver_context_head; node != NULL; node = node->next) {
            if (pthread_equal(self, node->tid)) {
                sockfd = node->sockfd;
                break;
            }
        }
        if (sockfd < 0) {
            //printf(""creating a new connection %d\\n"", self);
#ifdef UNIX_SOCKETS
            sockfd = connect_unix_socket(UNIX_SOCKET_PATH);
#else /* UNIX_SOCKETS */
            sockfd = connect_tcp_socket(HOST, LP_PORT);
#endif /* UNIX_SOCKETS */
            node = GC_MALLOC(sizeof(struct solver_context_node));
            node->tid = self;
            node->sockfd = sockfd;
            node->next = solver_context_head;
            solver_context_head = node;
        }
        pthread_mutex_unlock(&solver_context_mutex);
    }

    h.type = MSG_INIT;
    h.len = N;
    payload = alloc_payload(&h);
    if (!send_msg(sockfd, &h, payload)) {
        fprintf(stderr, ""Couldn't connect to server\\n"");
        exit(1);
    }
    free_payload(&h, payload);

    Ctx = sockfd;
#endif /* STANDALONE */
").


:- pred finalize_solver_context(solver_context::di) is det.

:- pragma foreign_proc("C",
    finalize_solver_context(Ctx::di),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef STANDALONE
    finalize_solver_context(Ctx);
#else /* STANDALONE */
#endif /* STANDALONE */
").


:- pred add_constraint(int::in, list(float)::in, list(int)::in, int::in,
                       float::in, solver_context::di, solver_context::uo)
                       is det.

:- pragma foreign_proc("C",
    add_constraint(N::in, As::in, Vs::in, Cmp::in, Bnd::in, Ctx0::di, Ctx1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef STANDALONE
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

    GC_FREE(vs_arr);
    GC_FREE(as_arr);
#else /* STANDALONE */
    const int sockfd = Ctx0;
    Header h;
    void *payload;

    h.type = MSG_CSTR;
    h.len = N;

    payload = alloc_payload(&h);

    MR_Word as_list = As;
    MR_Word vs_list = Vs;
    int i;

    for (i = 0; i < N; ++i) {
        assert(!MR_list_is_empty(as_list));
        assert(!MR_list_is_empty(vs_list));
        ((Constraint *) payload)->as[i] = MR_word_to_float(MR_list_head(as_list));
        ((Constraint *) payload)->vs[i] = (MR_Integer) MR_list_head(vs_list);
        vs_list = MR_list_tail(vs_list);
        as_list = MR_list_tail(as_list);
    }
    ((Constraint *) payload)->cmp = Cmp;
    ((Constraint *) payload)->bnd = Bnd;
    if (!send_msg(sockfd, &h, payload)) {
        free_payload(&h, payload);
        fprintf(stderr, ""%s:%d: Couldn't write message\\n"", __FILE__, __LINE__);
        exit(1);
    }

    free_payload(&h, payload);
#endif /* STANDALONE */
    Ctx1 = Ctx0;
").


:- pred solve(int::out, float::out, list(float)::out,
              solver_context::di, solver_context::uo) is det.

:- pragma foreign_proc("C",
    solve(Optimal::out, ObjValue::out, VarValues::out, Ctx0::di, Ctx1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef STANDALONE
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
#else /* STANDALONE */
    const int sockfd = Ctx0;
    Header h;
    void *payload;

    h.type = MSG_SOLVE;
    h.len = 0;
    payload = alloc_payload(&h);
    if (!send_msg(sockfd, &h, payload)) {
        free_payload(&h, payload);
        fprintf(stderr, ""%s:%d: Couldn't write header\\n"", __FILE__, __LINE__);
        exit(1);
    }
    free_payload(&h, payload);

    if (!recv_msg(sockfd, &h, &payload)) {
        fprintf(stderr, ""%s:%d: Couldn't write header\\n"", __FILE__, __LINE__);
        exit(1);
    }
    if (h.type == MSG_SUCCESS) {
        MR_Word var_values_list = MR_list_empty();
        int i;
        Optimal = (MR_Integer) 1;
        ObjValue = (MR_Float) ((Solution *) payload)->val;
        for (i = h.len - 1; i >= 0; --i) {
            const num_t val = ((Solution *) payload)->as[i];
            var_values_list = MR_list_cons(
                MR_float_to_word((MR_Float) val),
                var_values_list);
        }
        VarValues = var_values_list;
    } else if (h.type == MSG_FAILURE) {
        Optimal = (MR_Integer) 0;
        ObjValue = (MR_Float) 0;
        VarValues = MR_list_empty();
    } else {
        fprintf(stderr, ""%s:%d: Received unexpected message %d\\n"",
                __FILE__, __LINE__, (int) h.type);
    }
    free_payload(&h, payload);
#endif /* STANDALONE */

    Ctx1 = Ctx0;
").

%-----------------------------------------------------------------------------%
:- end_module osi.
%-----------------------------------------------------------------------------%
