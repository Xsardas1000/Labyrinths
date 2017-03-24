/*append (L1, L2, L3): список L3 является слиянием (конкатенацией) списков L1 и L2;*/
/* детерм: (i,i,i), (i,i,o), (i,o,i), (o,i,i) */
/* недетерм: (o,o,i)*/
append2([],L,L).
append2([X|R1],L2,[X|R3]):-
  append2(R1,L2,R3).

/*reverse (L1, L2): L2 - перевернутый список L1;*/
/* детерм: (i,o) , (i,i)*/
reverse2(L1, L2):- rev(L1, [], L2).
rev([],L,L).
rev([X|T], Acc, L2):- rev(T, [X|Acc], L2).

/* delete_first (E, L1, L2): список L2 получен из L1 исключением первого вхождения объекта Е;*/
/* детерм: (o,i,o), (o,i,i), (i,i,i), (o,i,i), (i,o,i)*/
delete_first(E, [E|T], T):- !.
delete_first(E, [X|T], [X|P]):-
  delete_first(E, T, P).

/*delete_one (E, L1, L2): L2 - список L1, в котором исключен один элемент Е
(исключается какое-то одно вхождение Е в список L1);*/
/* детерм: (o,i,i), (i,i,i), (o,i,i)*/
/* недетерм: (o,i,o),  (i,o,i)*/
delete_one(E, [E|T], T).
delete_one(E, [X|T], [X|P]):-
  delete_one(E, T, P).

/*delete_all (E, L1, L2): L2-это список L1, из которого удалены все вхождения Е;*/
/* детерм: (o,i,i), (i,i,i), (o,i,i)*/
delete_all(_, [], []).
delete_all(E, [E|T], K):-
   !, delete_all(E, T, K).
delete_all(E, [Y|T], K):-
  delete_all(E, T, L), K = [Y|L].

/*no_doubles (L1, L2): L2 - это список, являющийся результатом удаления из L1 всех повторяющихся элементов;*/
/* детерм: (i,i), (i,o)*/
no_doubles([],[]).
no_doubles([X|L], R):-
  member(X,L), !, no_doubles(L, R).
no_doubles([X|L], [X|T]):-
  no_doubles(L,T).

/*sublist (L1, L2): L1 - любой подсписок списка L2, т.е. непустой отрезок из подряд идущих элементов L2; */
/* детерм: (i,i)*/
/* недетерм: (o,i), (i,o)*/
sublist2(L1, L2):-
  append2(X, Z, L2), append2(L1, Y, Z), not(L1=[]).

/*number (E, N, L): N - порядковый номер элемента E в списке L;*/
/* детерм: (o,i,i), (i,i,i), (o,i,i)*/
/* недетерм: (o,o,i),  (i,o,i)*/
number2(E, 0, [E|_]).
number2(E, N, [_|L]):-
  number2(E, K, L), N is K + 1.


maxElem([X|L], M):- maxElem(L, M, X).
maxElem([], M, M).
maxElem([Y|L], M, X):-
  (Y>X), !, maxElem(L, M, Y).
maxElem([Y|L], M, X):-
  (Y=<X), maxElem(L, M, X).


minElem([X|L], M):- minElem(L, M, X).
minElem([], M, M).
minElem([Y|L], M, X):-
  (Y<X), !, minElem(L, M, Y).
minElem([Y|L], M, X):-
  (Y>=X), minElem(L, M, X).


/*sort*/
/* детерм: (i,i), (i,o)*/
sort3([], []).
sort3(List, [Min|SortRest]) :-
  min_list_exclude(Min, List, Exclude), sort3(Exclude, SortRest).

min_list_exclude(M, [M],  []).
min_list_exclude(Min, List,  ExcludeRes) :-
  minElem(List, Min), delete_first(Min, List, ExcludeRes).


/*subset (М2, М1): проверка - множество М1 является подмножеством М2;*/
/* недетерм: (i,o)*/
subset2(_,[]).
subset2([_|T], L):-
  subset2(T, L).
subset2([X|T],[X|L]):-
  subset2(T,L).


/* (i, i) - детерм */
subset4(_,[]).
subset4(L, [X|T]):-
  delete_first(X, L, L1), subset4(L1, T).

/* union (М1, М2, М3): множество М3 - объединение множеств М1 и М2;
вместо этого предиката может быть взят предикат intersection (М1, М2, М3): М3 -
пересечение М1 и М2 или предикат substraction (М1, М2, М3): М3 - разность М1 и М2.*/
/* недетерм: (i,i,o) */
/* детерм: (i, i, i) */

union3(M1, M2, M3):-
  union2(M1, M2, M), permutation(M, M3).

union2([],M2,M2).
union2(M1,[],M1).
union2([X|T],M2,R):-
  member(X,M2), !, union2(T,M2,R).
union2([X|T],M2,[X|R]):-
  union2(T,M2,R).



substr2([],_,[]).
substr2([X|T],M2,M3):-
  member(X,M2), !, substr2(T,M2,M3).
substr2([X|T], M2, [X|M3]):-
  substr2(T,M2,M3).


max(X, Y, X):- X>=Y.
max(X, Y, Y):- Y>X.

/*tree_depth (Т, N): N - глубина дерева (т.е. количество ребер в самой длинной ветви дерева);*/
/* детерм: (i,o), (i,i) */
/* недетерм: (o,i) */
tree_depth(nil, 0).
tree_depth(tree(_,nil,nil), 1):- !.
tree_depth(tree(_,L,R), N):-
  tree_depth(L, N1), tree_depth(R, N2), max(N1, N2, K), N is K+1.


/*кол-во вершин не являющихся листьями*/
/* детерм: (i,o), (i,i) */
/* недетерм: (o,i) */
num_vert2(nil, 0).
num_vert2(tree(_,nil,nil), 0):- !.
num_vert2(tree(_,L,R), N):-
  num_vert2(L,N1), num_vert2(R,N2), N is N1+N2+1.


/*sub_tree (Т1, Т2): дерево Т1 является непустым поддеревом дерева Т2;*/
/* детерм: (i,o), (i,i) */
/* недетерм: (o,i) */
sub_tree(tree(E, nil, nil), tree(E, nil, nil)).
sub_tree(S,tree(_,L,_)):-sub_tree(S,L).
sub_tree(S,tree(_,_,R)):-sub_tree(S,R).

/*flatten_tree (Т, L): L - список меток всех узлов дерева Т («выровненное» дерево);*/
/* детерм: (i,o), (i,i) */
/* недет: (o, i) */
flatten_tree(nil, []).
flatten_tree(tree(X, L, R), Res):-
  flatten_tree(L, L1), flatten_tree(R, R1), append2(L1, [X|R1], Res).

/* с использованием накапливаюего параметра*/
flatten_tree2(T, L):- flat(T, L, []).
flat(nil, Res, Res).
flat(tree(X, L, R), Res, Acc):-
  flat(R, R1, Acc), flat(L, Res, [X|R1]).

/*substitute (Т1, V, Т, Т2): Т2 - дерево, полученное путем замены всех вхождений V в дереве Т1 на терм Т.*/
/* детерм: (i,i,i,o), (i,i,o,o), (i,o,o,o), (i,o,i,o),  */
/* недетерм: (o,i,i,o), (o,o,o,o), (o,i,o,o), (o,o,i,o) */
substitute(nil, _, _, nil).
substitute(tree(E, L, R), E, E1, tree(E1, L1, R1)):-
  substitute(L, E, E1, L1), substitute(R, E, E1, R1), !.
substitute(tree(E, L, R), X, Y, tree(E, L1, R1)):-
  substitute(L, X, Y, L1), substitute(R, X, Y, R1).



edge(a, c, 8).
edge(a, b, 3).
edge(c, d, 5).
edge(b, d, 0).
edge(e, d, 9).

/*path (Х, У, L): L - путь без петель между вершинами Х и У, т.е. список вершин между этими вершинами;*/
/* детерм: (i,i,i), (i,o,i), (o,i,i) */
/* недетерм: (i,i,o), (i,o,o), (o,o,o) , (o,i,o), (o,o,i)*/
g_path(A,B,P):-g_path1(A,[B],P).

g_path1(A,[A|P1],[A|P1]).
g_path1(A,[Y|P1],P) :-
   e(X,Y,_), not(member(X,[Y|P1])), g_path1(A,[X,Y|P1],P).

e(X,Y,V) :- edge(X,Y,V).
e(X,Y,V) :- edge(Y,X,V).


/* min_path (Х, У, L): L - путь между вершинами Х и У, имеющий минималь- ную стоимость
(стоимость пути равна сумме стоимостей входящих в него ре- бер);*/
/* детерм: (i,i,i) */
/* недетерм: (i,i,o)*/
min_path(X, Y, Res) :-
  findall(W, get_path_values(X, [Y], _, W), Ps),
  sort(Ps, [Min|_]),
  get_path_values(X, [Y], Res, Min).

get_path_values(X, [X|T], [X|T],0).
get_path_values(X, [Y | T], Res, C2) :-
  e(Y, Z, C), not(member(Z, T)), get_path_values(X, [Z,Y|T], Res, C1), C2 is C + C1.



/*short_path (Х, У, L): L - самый короткий путь между вершинами Х и У (длина пути равна количеству ребер,входящих в него);*/
/* детерм: (i,i,i) */
/* недетерм: (o,o,i), (i,i,o)*/
short_path(X, Y, Res) :-
  short_path1([[X]], Y, Res).

short_path1(X, Y, Res) :-
  found(X, Y),! , get_short_paths(X, Y, Res).
short_path1(X, Y, Res) :-
  get_follows(X, AddX), short_path1(AddX, Y, Res).


get_follows([],[]).
get_follows([X|T], Res) :-
  findall(AddX, add_follow(X,AddX), AllX), get_follows(T, AllTail), append(AllX, AllTail, Res).


add_follow([X|T], Res) :-
  e(X, Y, _), not(member(Y, T)), append([Y], [X|T], Res).

found([[X|_]|_], X).
found([_|T], Y) :- found(T, Y).


get_short_paths([[X|T]|_], X, Rev) :-
  reverse([X|T], Rev).
get_short_paths([_| T], Y, Res) :-
  get_short_paths(T, Y, Res).


/*cyclic : граф является циклическим (т.е. не является деревом);*/
/*кол-во различных вершин в списке */

cyclic:-
  findall(edge(X,Y,V),edge(X,Y,V),Es), length(Es,EN),
  findall(X, e(X,Y,_), Vs), no_doubles(Vs,V1), length(V1, VN), P is EN + 1, VN \= P.


cyclic2:-
  e(X,_,_), e(Y,_,_), dif(X, Y), g_path(X,Y, P1), g_path(X,Y,P2), dif(P1,P2), !.


/* is_connected : граф является связным (т.е. для любых двух его вершин существует связывающий их путь).*/
is_connected:-not(not_connected).

path(A,B,P):-
  p(A,[B],P).

p(A,[A|Tail],[A|Tail]).
p(A,[B|Tail],P):-
  e(B,C,_), not(member(C,Tail)), p(A,[C,B|Tail],P).

not_connected:-
  e(A,_,_), e(B,_,_), A\=B, not(path(A,B,_)).


/*has_intersect([], _):- fail.*/
has_intersect([X|_], S2):- has_intersect1(X, S2), !.
has_intersect([_|S1], S2):- has_intersect(S1, S2).

/*has_intersect1(_, []):- fail.*/
has_intersect1(X, [X|_]):- !.
has_intersect1(X, [_|T]):- has_intersect1(X, T).
