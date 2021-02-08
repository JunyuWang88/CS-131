get_listtop([Hd|_], GT) :-
	GT is Hd.

tower(N, T, C) :-
	C = counts(Tp, B, L, R),
	length(L, N),
	length(R, N),
	length(Tp, N),
	length(B, N),
	constrain(N, T),
	is_unique(N, T),
	maplist(fd_labeling, T),
	check_bounds(T, L, R),
	transpose(T, TransT),
	check_bounds(TransT, Tp, B).

t_test(T) :-
	statistics(cpu_time, _),
	tower(5, _, counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
	statistics(cpu_time, [_|Tl_1]),
	get_listtop(Tl_1, Tp),
	T is Tp.

plain_tower(N, T, C) :-
	C = counts(Tp, B, L, R),
	length(L, N),
	length(R, N),
	length(Tp, N),
	length(B, N),
length(T, N),	
	bounds_list(1, N, Domain),
	confirm_constraints(N, Domain, T, L, R),
	transpose(T, TransPT),
	confirm_constraints(N, Domain, TransPT, Tp, B).

pt_test(T) :-
	statistics(cpu_time, _),
	plain_tower(5, _, counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
	statistics(cpu_time, [_|Tl_2]),
	get_listtop(Tl_2, Tp),
	T is Tp.

speedup(Ratio) :-
	pt_test(PTr),
	t_test(Tr),
	Ratio is PTr / Tr.

ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.

constrain(N, M) :-
	length(M, N),
	constrain_2(M, N).

constrain_2([], _).
constrain_2([Hd|Tl], N) :-
	length(Hd, N),
	constrain_2(Tl, N).

confirm_domainR(_, _, []).
confirm_domainR(Temp, Prev, [Hd|Tl]) :-
	member(Hd, Temp),
	rm_element(Hd, Temp, TempDom),
	confirm_domainR(TempDom, [Hd|Prev], Tl).

confirm_constraints(_, _, [], [], []).
confirm_constraints(N, Temp, [Hd|Tl], [Hd_2|Tl_2], [Hd_3|Tl_3]) :-
	length(Hd, N),
	confirm_domainR(Temp, [], Hd),
	is_visiblePt(Hd, 0, 0, Hd_2),
	reverse(Hd, Rhd),
	is_visiblePt(Rhd, 0, 0, Hd_3),
	confirm_constraints(N, Temp, Tl, Tl_2, Tl_3).

check_bounds([], [], []).
check_bounds([Hd|Tl], [Hd_2|Tl_2], [Hd_3|Tl_3]) :-
	is_visibleT(Hd, 0, 0, Hd_2),
	reverse(Hd, Rhd),
	is_visibleT(Rhd, 0, 0, Hd_3),
	check_bounds(Tl, Tl_2, Tl_3).

bounds_list(Minimum, Maximum, [Maximum|[]]) :- 
Minimum is Maximum.
bounds_list(Minimum, Maximum, [Minimum|Tl]) :-
	Minimum =< Maximum,
	Temp is Minimum + 1,
	bounds_list(Temp, Maximum, Tl).

/* determines how many towers are visible from the left side */
is_visibleT([], _, Num, Visi) :- 
Visi is Num.
is_visibleT([Hd|Tl], Maximum, Num, Visi) :-
	Hd #=< Maximum,
	is_visibleT(Tl, Maximum, Num, Visi).
is_visibleT([Hd|Tl], Maximum, Num, Visi) :-
	Hd #> Maximum,
	Temp is Num + 1,
	is_visibleT(Tl, Hd, Temp, Visi).

is_visiblePt([], _, TempV, Visi) 
:- Visi is TempV.
is_visiblePt([Hd|Tl], Maximum, TempV, Visi) :-
	Hd =< Maximum,
	is_visiblePt(Tl, Maximum, TempV, Visi).
is_visiblePt([Hd|Tl], Maximum, TempV, Visi) :-
	Hd > Maximum,
	Temp is TempV + 1,
	is_visiblePt(Tl, Hd, Temp, Visi).

rm_element(Element, [Element|Tl], Tl).
rm_element(Element, [Hd|Tl], [Hd|Out]) :-
	Element \= Hd,
	rm_element(Element, Tl, Out).

/* below from TA Amit's slides (week 5)  */
all_between(Minimum, Maximum, Row) :-
        maplist(between(Minimum, Maximum), Row).

all_unique([]).

all_unique([Hd|Tl]) :-
        member(Hd, Tl),
        !,
	fail.

all_unique([_|Tl]) :-
        all_unique(Tl).

/* below is my work but built off ta slides */
is_unique(N, M) :-
	is_uniqueR(N, M),
	is_uniqueC(N, M).

is_uniqueR(_, []).
is_uniqueR(N, [Hd|Tl]) :-
	fd_domain(Hd, 1, N),
	fd_all_different(Hd),
	is_uniqueR(N, Tl).

is_uniqueC(N, M) :-
	transpose(M, MT),
	is_uniqueR(N, MT).

/* below from https://www.generacodice.com/en/articolo/729059/How-to-transpose-a-matrix-in-prolog */
transpose([F|Fs], Ts) :-
	transpose(F, [F|Fs], Ts).
transpose([], []).

transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
transpose([], _, []).

lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).
lists_firsts_rests([], [], []).
