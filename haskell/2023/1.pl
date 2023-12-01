main :-
    open("1.txt", read, In),
    read_string(In, _, S), split_string(S, "\n", "\n", Lines),
    member(Part, [1,2]),
    maplist(process(Part), Lines, Xs), sum_list(Xs, Sum), writeln(Sum),
    fail.
main.

process(Part, S, N) :-
    string_codes(S, C),
    findall(X, (append(_, P, C), (numeric1(P, X) ; Part=2,numeric2(P,X))), Xs),
    Xs=[A|_], last(Xs, B), N is A*10+B.

numeric1([C|_], N) :- number_string(N, [C]).
numeric2(C, N) :- between(1,9,N), numbers(Numbers), nth1(N, Numbers, P), prefix(P, C).
numbers([`one`,`two`,`three`,`four`,`five`,`six`,`seven`,`eight`,`nine`]).
