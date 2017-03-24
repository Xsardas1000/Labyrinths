/* P is a probability to make wall (from 0 to 100) and B is 0 or 1 (make or not to make the wall)*/
make_wall(P, B):-
  random_between(0,100,R), make_wall1(R, P, B).

right_wall_prob(P):- P is 60.
bottom_wall_prob(P):- P is 80.

make_wall1(R, P, 1):- (R<P), !.
make_wall1(R, P, 0):- (R>=P).

no_doubles([],[]).
no_doubles([X|L], R):-
  member(X,L), !, no_doubles(L, R).
no_doubles([X|L], [X|T]):-
  no_doubles(L,T).

/*получить множество уже существующих классов*/
get_set_of_classes(L, S):- get_set_of_classes1(L, R), no_doubles(R, S).
get_set_of_classes1([],[]):- !.
get_set_of_classes1([cell(0, _, _) | L], R):- get_set_of_classes1(L, R), !.
get_set_of_classes1([cell(C, _, _) | L], [C|R1]):- get_set_of_classes1(L, R1).


/* получить минимальное число для номера нового класса, не входящее в список номеров существующих классов*/
get_new_class_number(S, N):- get_new_class_number1(S, N, 1).
get_new_class_number1(S, Curr, Curr):- not(member(Curr, S)), !.
get_new_class_number1(S, N, Curr):- K is Curr + 1, get_new_class_number1(S, N, K).


/*Присоединим все ячейки не принадлежащие классам к своим новым классам*/
add_new_classes(L, R):- get_set_of_classes(L, S), add_new_classes1(L, S, [], R).
add_new_classes1([], _, R, R):- !.
add_new_classes1([cell(0, F1, F2) | L], S, R, [cell(NewClass, F1, F2) | K]):-
  get_new_class_number(S, NewClass), T = [NewClass|S], add_new_classes1(L, T, R, K), !.
add_new_classes1([X|L], S, R, [X|K]):-
   add_new_classes1(L, S, R, K).


/*создание пустого ряда клеток шириной Width*/
make_empty_row(Width, Row):- make_empty_row1(Width, 0, [], Row).
make_empty_row1(Width, Curr, Row, [cell(0, 0, 0) | P]):-
  (Curr < Width), !, K is Curr + 1,  make_empty_row1(Width, K, Row, P).
make_empty_row1(Width, Curr, Row, Row):-
  (Curr >= Width).



/*создаём правые границы*/
make_right_boards(L, R):- make_right_boards1(L, [], R).

make_right_boards1([], R, R):- !.

make_right_boards1([X], R, [X|K]):- make_right_boards1([], R, K), !.

/*если клетки из одинаковых классов то всегда создаём стену*/
make_right_boards1([cell(C, _, D1), cell(C, R2, D2) | L], R, [cell(C, 1, D1) | K]):-
  make_right_boards1([cell(C, R2, D2) | L], R, K), !.

/*если из разных классов и выпало 1 то ставим стену*/
make_right_boards1([cell(C1, _, D1), cell(C2, R2, D2) | L], R, [cell(C1, 1, D1) | K]):-
  right_wall_prob(P), make_wall(P, B), B =:= 1, !, make_right_boards1([cell(C2, R2, D2) | L], R, K).

/*если из разных классов и выпало 0 то объединяем в один класс*/
make_right_boards1([cell(C1, _, D1), cell(_, R2, D2) | L], R, [cell(C1, 0, D1) | K]):-
  make_right_boards1([cell(C1, R2, D2) | L], R, K).


/*проверяем, единственная ли ячейка в своём классе или последняя без нижней границы */
check_single_cell(L, C):- check_single_cell1(L, C, Num), Num =:= 1.
check_single_cell1([], _, 0):- !.
check_single_cell1([cell(C, _, 0) | L], C, Num):-
  check_single_cell1(L, C, T), !,  Num is T + 1.
check_single_cell1([cell(_, _, _) | L], C, T):-
  check_single_cell1(L, C, T).



/*создаём нижние границы для ряда ячеек*/
make_bottom_boards(L, R):- make_bottom_boards1(L, 0, R).

/*если вышли за границу массива то возвращаем полученный результат*/
make_bottom_boards1(L, Curr, L):-
  length(L, Len), (Curr >= Len), !.

/*не создаём границу*/
make_bottom_boards1(L, Curr, R):-
  get_obj_at_index(L, Curr, Cell), get_cell_class(Cell, C), check_single_cell(L, C), !,
  T is Curr + 1, make_bottom_boards1(L, T, R).

/*создаём если выпадет 1 по вероятности*/
make_bottom_boards1(L, Curr, R):-
  bottom_wall_prob(P), make_wall(P, B), B =:= 1, !,
  get_obj_at_index(L, Curr, Cell),  add_bottom_board_to_cell(Cell, NewCell),change_value_at_index(L, Curr, NewCell, Res),
  T is Curr + 1, make_bottom_boards1(Res, T, R).

/*не создаём если выпало 0*/
make_bottom_boards1(L, Curr, R):-
  T is Curr + 1, make_bottom_boards1(L, T, R).

add_bottom_board_to_cell(cell(C, R, _), cell(C, R, 1)).
get_cell_class(cell(C, _, _), C).




/*изменить значение ячейки по индексу*/
change_value_at_index(L, Index, NewValue, R):-
  change_value_at_index1(L, Index, NewValue, 0, R).

change_value_at_index1([], _, _, _, []).
change_value_at_index1([_|L], Index, NewValue, Curr, [NewValue|L]):- (Curr =:= Index), !.
change_value_at_index1([X|L], Index, NewValue, Curr, [X|K]):-
  T is Curr + 1, change_value_at_index1(L, Index, NewValue, T, K).


/*получить ячейку из ряда по индексу*/
get_obj_at_index(L, Index, Obj):- get_obj_at_index1(L, Index, 0, Obj).
get_obj_at_index1([], _, _, _):- fail, !.
get_obj_at_index1([X|_], Index, Curr, X):- (Curr =:= Index), !.
get_obj_at_index1([_|L], Index, Curr, R):-
  T is Curr + 1,  get_obj_at_index1(L, Index, T, R).



/*удаление правых границ в ряду из ячеек*/
delete_right_boards([], []).
delete_right_boards([cell(C, _, D) | L], [cell(C, 0, D) | K]):-
  delete_right_boards(L, K).

/*если у ячейки есть нижняя граница обнулим её класс, также попутно удаляем нижние границы*/
delete_classes([], []).
delete_classes([cell(_, R, 1) | L], [cell(0, R, 0) | K]):-
  delete_classes(L, K), !.
delete_classes([X|L], [X|K]):-
  delete_classes(L, K).

/*добавление нового ряда ячеек, не последнего */
make_new_row(L, R):-
  delete_right_boards(L, R1), delete_classes(R1, R2), add_new_classes(R2, R3),
  make_right_boards(R3, R4), make_bottom_boards(R4, R).

/*добавлние последнего ряда ячеек*/
make_last_row(L, R):- make_new_row(L, R1), add_bottom_boards(R1, R2), make_strong_boards(R2, R3), join_cells(R3, R).



/*оставляем только границы между ячейками, принадлежащими одному классу*/
join_cells([X], [X]).
join_cells([cell(C1, 2, D1), cell(_, R2, D2) | L], [cell(C1, 1, D1) | K]):-
  join_cells([cell(C1, R2, D2) | L], K), !.
join_cells([cell(C1, _, D1), cell(_, R2, D2) | L], [cell(C1, 0, D1) | K]):-
  join_cells([cell(C1, R2, D2) | L], K), !.

/*находим пары ячеек, принадлежащих одному классу, между которыми есть стена и укрепляем её*/
make_strong_boards([X], [X]).
make_strong_boards([cell(C, 1, D1), cell(C, R2, D2) | L], [cell(C, 2, D1) | K]):-
  make_strong_boards([cell(C, R2, D2) | L], K).
make_strong_boards([X,Y|L], [X|K]):-
  make_strong_boards([Y|L], K).


/*добавляем нижние границы к каждой ячейке последнего ряда*/
add_bottom_boards([], []).
add_bottom_boards([cell(C, R, _) | L], [cell(C, R, 1) | K]):-
  add_bottom_boards(L, K).


print_row(L):- write("|"), print_row1(L).
print_row1([]):- write("|"), write("\n"),!.
print_row1([cell(_, 0, 0) | L]):- write("   "),print_row1(L).
print_row1([cell(_, 1, 0) | L]):- write("  |"), print_row1(L).
print_row1([cell(_, 0, 1) | L]):- write("___"), print_row1(L).
print_row1([cell(_, 1, 1) | L]):- write("__|"), print_row1(L).



make_lab(Height, Width, [Row|K]):- make_empty_row(Width, TopRow), make_new_row(TopRow, Row), write(Row), write("\n"),
  make_lab1(Height, 1, Row, [],K).
make_lab1(Height, Curr, _, R, R):-
  (Curr =:= Height), !.
make_lab1(Height, Curr, L, R, [NewRow|K]):-
  (Curr < Height - 1), !, T is Curr + 1, make_new_row(L, NewRow), write(NewRow), write("\n"), make_lab1(Height, T, NewRow, R, K).
make_lab1(Height, Curr, L, R, [LastRow|K]):-
  (Curr =:= Height - 1), T is Curr + 1, make_last_row(L, LastRow), write(LastRow), write("\n"), make_lab1(Height, T, LastRow, R, K).


/*печать лабиринта в терминале*/
print_lab(Height, Width):-
  make_lab(Height,Width,Lab), print_line(Width), print_lab1(Lab).
print_lab1([]):- !.
print_lab1([Row|L]):- print_row(Row), print_lab1(L).

/*напечатать границу длиной */
print_line(Width):- print_line1(Width, 0).
print_line1(Width, Curr):-
  (Curr < Width), !, write("___"), T is Curr + 1, print_line1(Width, T).
print_line1(_, _):- write("\n").

start :- /* создаём диалоговое окно */
         new(DW, dialog('Окно моей программы')),
/* создаём поле Picture для вывода графических
фигур и задаём его размеры */
         new(Picture, picture),
         send(Picture, width(350)),
         send(Picture, height(350)),
         send(Picture, open),
         send(DW, open).
