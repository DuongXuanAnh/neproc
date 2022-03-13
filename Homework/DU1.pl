nat(0).
nat(s(X)) :- nat(X).

toNat_(N, R) :- N > 0 -> (N2 is N - 1, toNat_(N2, R2), R = s(R2));R = 0.

toNat(N, R) :- integer(N), toNat_(N, R).
  
% Odecitani
sub(X, Y, Z) :- add(Y, Z, X).

% Deleni
div(X, s(Y), 0) :- leq(s(X), s(Y)).
div(X, s(Y), s(R2)) :- sub(X , s(Y), R1), div(R1, s(Y), R2).


% Méně nebo rovno
leq(0, Y) :- nat(Y).
leq(s(X), s(Y)) :- leq(X, Y).

% Sčítání.
add(0, Y, Y) :- nat(Y).
add(s(X), Y, s(Z)) :-
  add(X, Y, Z).

% Puleni
half(0,0).
half(s(0), 0).
half(s(s(N)), s(R)) :- half(N, R).

% Logaritmus o základu 2 (dolní celou část) na unárně
% reprezentovaných číslech.
logtwo(s(0), 0).
logtwo(N, A):-
    leq(s(0), N),
    div(N, s(s(0)), N1),
    logtwo(N1, A1),
    add(A1, s(0), A).

% Jiny zpusob
log_two(s(0),0).
log_two(s(P), s(R2)) :- half(s(P), R), log_two(R, R2).


% n-té Fibonacciho číslo
fib(0, s(0)). 
fib(s(0), s(0)).
fib(N, R) :-  
    leq(s(0), N),
    sub(N, s(0), N1),
    sub(N, s(s(0)), N2),
    fib(N1, F1),
    fib(N2, F2),
    add(F1, F2, R).

% Jiny zpusob
fib2(0, Lo, _, Lo).
fib2(N, Lo, Hi, R) :- 
  N > 0,
  PN is N - 1,
  Next is Lo + Hi,
  fib2(PN, Hi, Next, R).

fib2(N, R) :- fib2(N, 0, 1, R).

% Obraceni seznamu
reverse([], []).
reverse([X|Xs], Y) :- reverse(Xs, R), append(R,[X],Y).

% Velikost seznamu
length_1(0,[]).
length_1(L+1, [H|T]) :- length_1(L,T).

% Prvni element v seznamu
head([H|_], H).

% Posledni element v seznamu
last([X], X).
last([_|T], Y) :- last(T,Y).

% predikát pro sčítání dvou binárních čísel.

addBits(0, 0, 0, 0, 0).
addBits(1, 0, 0, 1, 0).
addBits(0, 1, 0, 1, 0).
addBits(0, 0, 1, 1, 0).
addBits(1, 1, 0, 0, 1).
addBits(1, 0, 1, 0, 1).
addBits(0, 1, 1, 0, 1).
addBits(1, 1, 1, 1, 1).

addBin([], [], 0, []).
addBin([], [], 1, [1]).

addBin([], [B|R1], C, [Res|R]) :-
  addBits(B, 0, C, Res, COut),
  addBin([], R1, COut, R).

addBin([B|R1], [], C, [Res|R]) :-
  addBits(B, 0, C, Res, COut),
  addBin(R1,[], COut, R).

addBin([B1|R1], [B2|R2], C, [Res|R]) :-
  addBits(B1, B2, C, Res, COut),
  addBin(R1, R2, COut, R).

addBin(A, B, R) :- addBin(A, B, 0, R).





