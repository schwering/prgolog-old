%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.vector_space.m.
% Main author: schwering.
%
% Typeclass for vector spaces.
%
% We don't use the standard arithmetic signs to avoid more ambiguous
% overloading.
%
% For some implementations have a look at the `impl' sub-module.
%
%-----------------------------------------------------------------------------%

:- module util.vector_space.

:- interface.

:- typeclass vector_space(T) where [
    func abs(T) = T,
    func T `plus` T = T,
    func T `minus` T = T,
    func float `sc_mult` T = T,
    pred (T::in) `lt` (T::in) is semidet,
    pred (T::in) `gt` (T::in) is semidet,
    pred (T::in) `leq` (T::in) is semidet,
    pred (T::in) `geq` (T::in) is semidet
].

%-----------------------------------------------------------------------------%

:- include_module impl.

%-----------------------------------------------------------------------------%
:- end_module util.vector_space.
%-----------------------------------------------------------------------------%
