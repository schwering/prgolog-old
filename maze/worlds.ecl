% vim:filetype=prolog:textwidth=80:shiftwidth=4:softtabstop=4:expandtab
%
% Registration pool for world modules.
% Each world may define and export those predicates listed under forward/2, for
% example:
%
%   :- export(reward_function/1).   % Lookahead at choicepoints (singleton).
%   :- export(lookahead/1).         % Lookahead at choicepoints (singleton).
%   :- export(new_lookahead/3).     % Horizon due to an action (singleton).
%
%   :- export(get_type/2).          % Determines the type of a variable.
%   :- export(add_type/2).          % Marks a variable with a given type.
%   :- export(type_check/2).        % Type check declarations.
%   :- export(primitive_action/1).  % Primitive action declaration.
%   :- export(stochastic_action/1). % Stochastic action declaration.
%   :- export(random_outcome/3).    % Stochastic outcome action declaration.
%   :- export(fluent/1).            % Fluents.
%   :- export(nonfluent/1).         % Non-fluent predicates called from tests.
%   :- export(macro/2).             % Macro declaration for fluent-expressions.
%   :- export(proc/2).              % Program declaration.
%
%   :- export(poss/2).              % Precondition axiom.
%   :- export(gamma_plus/3).        % Positive effect axiom.
%   :- export(gamma_minus/3).       % Negative effect axiom.
%   :- export(initial/1).           % What holds in the initial situation.
%
% This package dispatches to each of these predicates in a non-specified order.
% For each predicate, there are two versions, both with the same functor as the
% destination predicate, one with the same arity and one with an additional
% first parameter for the world to which the call dispatches.
% For example, if myWorld defines fluent/2, the user can call
%   worlds:fluent(myWorld, X, Y).
%
% Primitive action declarations are time-suppressed. Precondition and effect
% axioms are not time-suppressed.
%
% Type checks can be used by the solver to get the right world's type check
% predicate for suspended type checks. The first argument of type_check/2 is a
% variable, the second argument must be set to the query that asserts the
% variable's type. For example, for a `degree' type, one would add the attribute
% `deg' to each variable `V' of this kind. Using this attribute, a query like
% `between(1, 360, 1, V)' as type check could be determined.
% It's the modeler's task to ensure that one variable has at most one type.
% Together with type_check/2, there is get_type/2. The corresponding mutator
% predicate is add_type/2.
%
% The predicates mintime/1, maxtime/1, lookahead/1 and new_lookahead/3 must be
% defined in at most one world.
% add_world/1 warns if this is not the case. Performance should be very bad
% if there are multiple solutions to lookahead(H) etc. mintime/1 and maxtime/1
% define the domain of time variables.
% lookahead/1 is the maximum lookahead of nondeterministic loops. new_lookahead/3
% determines how this lookahead develops when an action is executed. Note that
% this action is actually a non-stochastic singleton program, that is, either
% an untimestamped primitive action or a test action.
% The lookahead should always be non-negative.

:- module(worlds).

:- export(add_world/1).
:- export(world/1).

forward(get_type/2).
forward(add_type/2).
forward(type_check/2).
forward(initial/1).
forward(reward_function/1).
forward(lookahead/1).
forward(new_lookahead/3).
forward(primitive_action/1).
forward(stochastic_action/1).
forward(random_outcome/3).
forward(poss/2).
forward(fluent/1).
forward(nonfluent/1).
forward(macro/2).
forward(gamma_plus/3).
forward(gamma_minus/3).
forward(proc/2).

:- findall(Forward, forward(Forward), Forwards),
   ( foreach(Functor/Arity, Forwards) do
       length(Args, Arity),
       P =..[Functor|Args],
       compile_term(P :- fail),
       export(Functor/Arity)
   ),
   ( foreach(Functor/Arity, Forwards) do
       length(Args, Arity),
       P =..[Functor,_World|Args],
       ExportArity is Arity + 1,
       compile_term(P :- fail),
       export(Functor/ExportArity)
   ).

world(_) :- fail. % Initially, there's nothing.

add_world(W) :-
    ( world(W) ->
        writeln('already a world'(W))
    ;
        findall(world(W0), world(W0), Facts0),
        Facts1 = [world(W) | Facts0],
        ( compile_term(Facts1) ->
            writeln('new world'(W))
        ;
            writeln('failed new world'(W))
        )
    ),
    compile_forwards,
    check_singletons(reward_function(_)),
    check_singletons(lookahead(_)),
    check_singletons(new_lookahead(1, _, _)).

compile_forwards :-
    findall(W, world(W), Worlds),
    findall(Forward, forward(Forward), Forwards),
    ( param(Worlds),
      foreach(Functor/Arity, Forwards) do
        ( param(Functor,Arity),
          foreach(W, Worlds),
          fromto([], ShrtFacts0, ShrtFacts1, ShrtFacts),   % without world
          fromto([], LongFacts0, LongFacts1, LongFacts) do % with world param
            ( current_module_predicate(exported, Functor/Arity)@W ->
                length(Args, Arity),
                ShrtHead =..[Functor|Args],
                LongHead =..[Functor,W|Args],
                BodyCall =..[Functor|Args],
                ShrtFact = (ShrtHead :- W:BodyCall),
                LongFact = (LongHead :- W:BodyCall),
                ShrtFacts1 = [ShrtFact|ShrtFacts0],
                LongFacts1 = [LongFact|LongFacts0]
            ;
                ShrtFacts1 = ShrtFacts0,
                LongFacts1 = LongFacts0
            )
        ),
        compile_term(ShrtFacts),
        compile_term(LongFacts)
        % ShrtFacts (without world parameter) and LongFacts (with additional
        % world param) are not collected together so that they are compiled
        % continguously.
    ).

check_singletons(Singleton) :-
    functor(Singleton, Functor, Arity),
    findall(W, (world(W),
                current_module_predicate(exported, Functor/Arity)@W,
                call(Singleton)@W)
        , Worlds),
    length(Worlds) > 1,
    writeln(warning_output, 'Too many declarations of singleton in worlds:'),
    writeln(warning_output, Singleton --> Worlds),
    fail.
check_singletons(_) :-
    true.

