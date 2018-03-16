% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X, Y) :- parent(Y, X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X, _), female(X).
isFather(X) :- parent(X, _), male(X).

% 3. Define a predicate `grandparent/2`.
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).


% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X, Y) :- parent(Z, X), parent(Z, Y), (X \= Y).


% 5. Define two predicates `brother/2` and `sister/2`.
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X, Y) :- married(X, Z), sibling(Z, Y).
siblingInLaw(X, Y) :- married(Y, Z), sibling(Z, X).


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X, Y) :- sister(X, Z), parent(Z, Y). 
aunt(X, Y) :- female(X), siblingInLaw(X, Z), parent(Z, Y). 
uncle(X, Y) :- brother(X, Z), parent(Z, Y).
uncle(X, Y) :- male(X), siblingInLaw(X, Z), parent(Z, Y). 


% 8. Define the predicate `cousin/2`.
cousin(X, Y) :- child(X, Z), aunt(Z, Y). 
cousin(X, Y) :- child(X, Z), uncle(Z, Y). 


% 9. Define the predicate `ancestor/2`.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).


% Extra credit: Define the predicate `related/2`.
%related(X, Y) :- parent(X, Y).
%related(X, Y) :- aunt(X, Y).
%related(X, Y) :- uncle(X, Y).
%related(X, Y) :- ancestor_(X, Y).
%related(X, Y) :- ancestor_(X, Z), ancestor_(Z, Y).



%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.
bool(t).
bool(f).

expr(X) :- bool(X).
expr(X) :- number(X).
expr(X) :- string(X).


cmd(C, S1, S2) :- expr(C), S2 = [C|S1].

cmd(add, [C1,C2|S1], S2) :- number(C1), number(C2), Z is C1+C2, S2 = [Z|S1].

cmd(lte, [C1,C2|S1], S2) :- number(C1), number(C2), C1 =< C2, S2 = [t|S1].
cmd(lte, [C1,C2|S1], S2) :- number(C1), number(C2), S2 = [f|S1].

cmd(if(P1,_), [B|S1], S2) :- bool(B), B = t, prog(P1, S1, S2).
cmd(if(_,P2), [B|S1], S2) :- bool(B), B = f, prog(P2, S1, S2).



% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.

prog(C, S1, S2) :- cmd(C, S1, S2).
prog([C], S1, S2) :- cmd(C, S1, S2).
prog([C|P], S1, S2) :- cmd(C, S1, F), prog(P, F, S2).
