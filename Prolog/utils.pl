:- module(utils,
          [ clear_screen/0,
            press_enter_to_continue/0,
            switch_player/0,
            global_index/3,
            get_mini_board/3,
            is_valid_quadrant_choice/2,
            is_valid_cell_choice/2,
            update_scores/0,
            available_characters/1,
            print_characters/2,
            replace/4,
            check_win/2,
            check_draw/1,
            winning_line/1,
            read_file_to_terms/2,
            write_ranking/2,
            ensure_directory_exists/1,
            parse_algebraic_cell/2,
            update_next_quadrant/1,
            print_lines/1,
            set_char_at/5,
            replace_nth/4,
            clear_game_state/0,    
            get_winner_by_score/1,
            get_player_name/2,
            get_player_symbol/2,
            other_player/1
          ]).

:- use_module(game_logic).
:- encoding(utf8).

% --- Game State Management ---
switch_player :-
    current_player(Current),
    player1(P1),
    player2(P2),
    ( Current == P1 -> Next = P2 ; Next = P1 ),
    retract(current_player(_)),
    assertz(current_player(Next)).

update_next_quadrant(CellIndex) :-
    winner_board(WB),
    nth0(CellIndex, WB, Status),
    ( Status == 'p' -> NextQuadrant = CellIndex ; NextQuadrant = any ),
    retract(next_quadrant(_)),
    assertz(next_quadrant(NextQuadrant)).

update_scores :-
    current_player(CP),
    player1(P1),
    (   CP == P1
    ->  p1_small_wins(OldScore),
        NewScore is OldScore + 1,
        retract(p1_small_wins(_)),
        assertz(p1_small_wins(NewScore))
    ;   p2_small_wins(OldScore),
        NewScore is OldScore + 1,
        retract(p2_small_wins(_)),
        assertz(p2_small_wins(NewScore))
    ).

clear_game_state :-
    retractall(player1(_)),
    retractall(player2(_)),
    retractall(current_player(_)),
    retractall(board(_)),
    retractall(winner_board(_)),
    retractall(next_quadrant(_)),
    retractall(p1_small_wins(_)),
    retractall(p2_small_wins(_)),
    retractall(game_mode(_)).


% --- Board Logic & Validation ---

global_index(Q, C, I) :-
    Q_Row is Q // 3,
    Q_Col is Q mod 3,
    C_Row is C // 3,
    C_Col is C mod 3,
    I is (Q_Row * 27) + (C_Row * 9) + (Q_Col * 3) + C_Col.

get_mini_board(B, Q, MB) :-
    findall(Cell,
            ( between(0, 8, CellIndex),
              global_index(Q, CellIndex, GlobalIndex),
              nth0(GlobalIndex, B, Cell)
            ),
            MB).

is_valid_quadrant_choice(any, Q) :-
    Q >= 0, Q =< 8,
    winner_board(WB),
    nth0(Q, WB, 'p').

is_valid_quadrant_choice(RequiredQuadrant, RequiredQuadrant) :-
    winner_board(WB),
    nth0(RequiredQuadrant, WB, 'p').

is_valid_cell_choice(Q, C) :-
    C >= 0, C =< 8,
    global_index(Q, C, GlobalIndex),
    board(B),
    nth0(GlobalIndex, B, 'e').


% --- Win/Draw Condition Logic ---

winning_line([0,1,2]). winning_line([3,4,5]). winning_line([6,7,8]).
winning_line([0,3,6]). winning_line([1,4,7]). winning_line([2,5,8]). 
winning_line([0,4,8]). winning_line([2,4,6]).

check_win(B, P) :-
    winning_line(Line),
    check_line(B, P, Line).

check_line(B, P, [I1,I2,I3]) :-
    nth0(I1, B, P),
    nth0(I2, B, P),
    nth0(I3, B, P).

check_draw(B) :-
    \+ member('e', B),
    \+ member('p', B).

get_winner_by_score(Winner) :-
    p1_small_wins(Score1),
    p2_small_wins(Score2),
    player1(P1),
    player2(P2),
    (   Score1 > Score2 -> Winner = P1
    ;   Score2 > Score1 -> Winner = P2
    ;   Winner = draw
    ).

% --- Player Setup ---

available_characters([
    char('X', "Xis"),
    char('O', "Bola"),
    char('#', "Cerquinha"),
    char('@', "Arroba"),
    char('&', "E-Comercial"),
    char('%', "Porcento")
]).

print_characters([], _).
print_characters([char(Symbol, Name)|Rest], Index) :-
    format("  ~w) [~w] ~w~n", [Index, Symbol, Name]),
    NextIndex is Index + 1,
    print_characters(Rest, NextIndex).

other_player(OtherPlayer) :-
    current_player(Current),
    player1(P1),
    player2(P2),
    ( Current == P1 -> OtherPlayer = P2 ; OtherPlayer = P1 ).

get_player_name(player(_, Name), Name).

get_player_symbol(player(Symbol, _), Symbol).

% --- Input Parsing ---

parse_algebraic_cell(Input, CellIndex) :-
    string_chars(Input, [RowChar, ColChar]),
    char_to_row(RowChar, RowIndex),
    char_to_col(ColChar, ColIndex),
    CellIndex is RowIndex * 3 + ColIndex.

char_to_row('A',0). char_to_row('B',1). char_to_row('C',2).
char_to_row('D',0). char_to_row('E',1). char_to_row('F',2).
char_to_row('G',0). char_to_row('H',1). char_to_row('I',2).

char_to_col('1',0). char_to_col('2',1). char_to_col('3',2).
char_to_col('4',0). char_to_col('5',1). char_to_col('6',2).
char_to_col('7',0). char_to_col('8',1). char_to_col('9',2).


% --- List & String Manipulation ---

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

replace_nth(L, I, X, R) :- nth0(I, L, _, Tmp), nth0(I, R, X, Tmp).

set_char_at(Row, Col, Char, InStrings, OutStrings) :-
    nth0(Row, InStrings, OldString),
    string_codes(OldString, OldCodes),
    char_code(Char, CharCode),
    replace_nth(OldCodes, Col, CharCode, NewCodes),
    string_codes(NewString, NewCodes),
    replace_nth(InStrings, Row, NewString, OutStrings).

% --- File I/O for Rankings ---

ensure_directory_exists(Directory) :-
    (exists_directory(Directory) -> true ; make_directory(Directory)).

read_file_to_terms(File, Terms) :-
    open(File, read, Stream),
    read_term(Stream, Term, []),
    read_terms(Stream, Term, Terms),
    close(Stream).

read_terms(_, end_of_file, []) :- !.
read_terms(Stream, Term, [Term|Terms]) :-
    read_term(Stream, NextTerm, []),
    read_terms(Stream, NextTerm, Terms).

write_ranking(_, []).
write_ranking(Stream, [H|T]) :-
    write_term(Stream, H, [quoted(true)]),
    write(Stream, '.\n'),
    write_ranking(Stream, T).

clear_screen :-
    write('\e[2J\e[H').

press_enter_to_continue :-
    nl,
    write('Pressione ENTER para continuar...'),
    read_line_to_string(user_input, _).

print_lines([]).
print_lines([H|T]) :- writeln(H), print_lines(T).