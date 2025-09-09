:- module(utils,
          [ clear_screen/0, press_enter_to_continue/0, switch_player/0,
            global_index/3, get_mini_board/3, is_valid_quadrant_choice/2,
            is_valid_cell_choice/2, update_scores/0, available_characters/1,
            print_characters/2, replace/4, check_win/2, check_draw/1,
            winning_line/1, read_file_to_terms/2, write_ranking/2,
            ensure_directory_exists/1, parse_algebraic_cell/2, update_next_quadrant/1,
            print_lines/1, set_char_at/5, replace_nth/4 % Garante que estes estão exportados
          ]).

:- use_module(game_logic).
:- encoding(utf8).

% Predicados que o jogo utiliza
clear_screen :- write('\e[2J\e[H').
press_enter_to_continue :- nl, write('Pressione ENTER para continuar...'), read_line_to_string(user_input, _).
switch_player :- current_player(CP), player1(P1), player2(P2), ( CP == P1 -> NP=P2 ; NP=P1 ), retract(current_player(_)), assertz(current_player(NP)).
update_next_quadrant(C) :- winner_board(WB), nth0(C, WB, S), ( S=='p' -> NQ=C ; NQ=any ), retract(next_quadrant(_)), assertz(next_quadrant(NQ)).
global_index(Q, C, I) :- Q_R is Q//3, Q_C is Q mod 3, C_R is C//3, C_C is C mod 3, I is (Q_R*27)+(C_R*9)+(Q_C*3)+C_C.
get_mini_board(B, Q, MB) :- findall(C, (between(0, 8, Cell), global_index(Q, Cell, GI), nth0(GI, B, C)), MB).
is_valid_quadrant_choice(any, Q) :- Q >= 0, Q =< 8, winner_board(WB), nth0(Q, WB, 'p').
is_valid_quadrant_choice(ReqQ, ReqQ) :- winner_board(WB), nth0(ReqQ, WB, 'p').
is_valid_cell_choice(Q, C) :- C >=0, C =< 8, global_index(Q, C, GI), board(B), nth0(GI, B, 'e').
update_scores :- current_player(CP), player1(P1), ( CP == P1 -> p1_small_wins(O), N is O+1, retract(p1_small_wins(_)), assertz(p1_small_wins(N)) ; p2_small_wins(O), N is O+1, retract(p2_small_wins(_)), assertz(p2_small_wins(N)) ).
available_characters([ char('X',"Xis"), char('O',"Bola"), char('#',"Cerquinha"), char('@',"Arroba"), char('&',"E-Comercial"), char('%',"Porcento") ]).
print_characters([], _).
print_characters([char(S,N)|T], U) :- \+ member(player(S,_), U), !, available_characters(All), nth1(Num, All, char(S,N)), format('~d -> [~w] ~w\n',[Num,S,N]), print_characters(T, U).
print_characters([_|T], U) :- print_characters(T, U).
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :- I > 0, I1 is I-1, replace(T, I1, X, R).
winning_line([0,1,2]). winning_line([3,4,5]). winning_line([6,7,8]). winning_line([0,3,6]). winning_line([1,4,7]). winning_line([2,5,8]). winning_line([0,4,8]). winning_line([2,4,6]).
check_win(B, P) :- winning_line(L), check_line(B, P, L).
check_line(B, P, [I1,I2,I3]) :- nth0(I1,B,P), nth0(I2,B,P), nth0(I3,B,P).
check_draw(B) :- \+ member('e', B), \+ member('p', B).
read_file_to_terms(F, Ts) :- open(F, read, S), read_term(S, T, []), read_terms(S, T, Ts), close(S).
read_terms(_, end_of_file, []) :- !.
read_terms(S, T, [T|Ts]) :- read_term(S, N, []), read_terms(S, N, Ts).
write_ranking(_, []).
write_ranking(S, [H|T]) :- write_term(S, H, [quoted(true)]), write(S, '.\n'), write_ranking(S, T).
ensure_directory_exists(D) :- (exists_directory(D) -> true ; make_directory(D)).
parse_algebraic_cell(In, C) :- string_chars(In, [RowC,ColC]), char_to_row(RowC,RowI), char_to_col(ColC,ColI), C is RowI*3+ColI.
char_to_row('A',0). char_to_row('B',1). char_to_row('C',2). char_to_row('D',0). char_to_row('E',1). char_to_row('F',2). char_to_row('G',0). char_to_row('H',1). char_to_row('I',2).
char_to_col('1',0). char_to_col('2',1). char_to_col('3',2). char_to_col('4',0). char_to_col('5',1). char_to_col('6',2). char_to_col('7',0). char_to_col('8',1). char_to_col('9',2).

% Predicados que o a interface e o tabuleiro menor usam
print_lines([]).
print_lines([H|T]) :- writeln(H), print_lines(T).
replace_nth(L, I, X, R) :- nth0(I, L, _, Tmp), nth0(I, R, X, Tmp).
set_char_at(R, C, Char, In, Out) :-
    nth0(R, In, OldStr), string_codes(OldStr, OldCodes),
    char_code(Char, CharCode),
    replace_nth(OldCodes, C, CharCode, NewCodes),
    string_codes(NewStr, NewCodes),
    replace_nth(In, R, NewStr, Out).