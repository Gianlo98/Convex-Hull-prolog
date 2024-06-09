%% -*- Mode: Prolog -*-
%%%% Occhipinti Gianlorenzo 829524, Piovani Davide 830113

:- use_module(library(pairs)).

read_points(Filename, Points) :-
    csv_read_file(Filename, Points, [functor(point), arity(2), separator(0'\t)]).

convexh(Points, CH) :-
    predsort(order_by_yx, Points, [P0 | Ps]),
    retractall(p0(_)),
    assert(p0(P0)),
    sort_point_clockwise_angle(Ps, [P1 | SortedPoints]),
    graham_scan(SortedPoints, [P1, P0], [], [], CHR, _Inner),
    reverse(CHR, CH).

area2(A, B, C, Area):-
     check_point_syntax(A, Xa, Ya),
     check_point_syntax(B, Xb, Yb),
     check_point_syntax(C, Xc, Yc),
     Area is ((Xb - Xa) * (Yc - Ya)) - ((Xc - Xa) * (Yb - Ya)).

left(A, B, C) :-
    area2(A, B, C, Area),
    Area > 0,
    !.

lefton(A, B, C) :-
    area2(A, B, C, Area),
    Area < 0,
    !.

coll(A, B, C) :-
    area2(A, B, C, 0),
    !.

graham_scan([Pi | Ss], [P2, P1 | Hs], Inner, NewStack, NewHull, NewInner) :-
    left(P1, P2, Pi),
    !,
    graham_scan(Ss, [Pi, P2, P1 | Hs], Inner, NewStack, NewHull, NewInner).

graham_scan([Pi | Ss], [P2, P1 | Hs], Inner, NewStack, NewHull, NewInner) :-
    lefton(P1, P2, Pi),
    !,
    graham_scan([Pi | Ss], [P1 | Hs], [P2 | Inner], NewStack, NewHull, NewInner).

graham_scan([Pi | Ss], [P2, P1 | Hs], Inner, NewStack, NewHull, NewInner) :-
    coll(P1, P2, Pi),
    !,
    graham_scan(Ss, [Pi, P2, P1 | Hs], Inner, NewStack, NewHull, NewInner).

graham_scan([], Hull, Inner, [], Hull, Inner) :- !.


order_by_yx(<, P0, P1) :-
    check_point_syntax(P0, _X0, Y0),
    check_point_syntax(P1, _X1, Y1),
    Y0 < Y1,
    !.

order_by_yx(>, P0, P1) :-
    check_point_syntax(P0, _X0, Y0),
    check_point_syntax(P1, _X1, Y1),
    Y0 > Y1,
    !.

order_by_yx(<, P0, P1) :-
    check_point_syntax(P0, X0, Y0),
    check_point_syntax(P1, X1, Y1),
    Y0 = Y1,
    X0 < X1,
    !.

order_by_yx(>, P0, P1) :-
    check_point_syntax(P0, X0, Y0),
    check_point_syntax(P1, X1, Y1),
    Y0 = Y1,
    X0 > X1,
    !.

order_by_yx(=, P0, P1) :-
    check_point_syntax(P0, X0, Y0),
    check_point_syntax(P1, X0, Y0),
    !.

sort_point_clockwise_angle(Points, RSPoints) :-
    map_list_to_pairs(angle_with_p0, Points, Pairs),
    predsort(order_by_r, Pairs, SortedPairs),
    pairs_values(SortedPairs, SPoints),
    reverse(SPoints, RSPoints).

order_by_r(>, K1-_P0, K2-_P1) :-
    K1 > K2,
    !.

order_by_r(<, K1-_P0, K2-_P1) :-
    K1 < K2,
    !.

order_by_r(<, K1-P0, K1-P1) :-
    check_point_syntax(P0, _X0, Y0),
    check_point_syntax(P1, _X1, Y1),
    Y0 > Y1,
    !.

order_by_r(>, K1-P0, K1-P1) :-
    check_point_syntax(P0, _X0, Y0),
    check_point_syntax(P1, _X1, Y1),
    Y0 < Y1,
    !.

order_by_r(<, K1-P0, K1-P1) :-
    check_point_syntax(P0, X0, Y0),
    check_point_syntax(P1, X1, Y0),
    X0 > X1,
    !.

order_by_r(>, K1-P0, K1-P1) :-
    check_point_syntax(P0, X0, Y0),
    check_point_syntax(P1, X1, Y0),
    X0 < X1,
    !.

angle_with_p0(P, K) :-
    p0(P0),
    angle2d(P0, P, Ks),
    Ks is -pi,
    !,
    K is pi * 2.

angle_with_p0(P, K) :-
    p0(P0),
    angle2d(P0, P, Kp),
    !,
    K is pi + Kp.


check_point_syntax(P, X, Y) :-
    functor(P, point, 2),
    P =.. [_, X, Y],
    integer(X),
    integer(Y).

angle2d(A, B, R) :-
    check_point_syntax(A, X0, Y0),
    check_point_syntax(B, X1, Y0),
    X0 > X1,
    !,
    R is pi.

angle2d(A, B, R) :-
    check_point_syntax(A, X0, Y0),
    check_point_syntax(B, X1, Y0),
    X0 < X1,
    !,
    R is -pi.

angle2d(A, B, R) :-
    check_point_syntax(A, X0, Y0),
    check_point_syntax(B, X1, Y1),
    !,
    R is atan((X0 - X1) / (Y0 - Y1)).
