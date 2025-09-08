:- module(game_logic,
          [ game_loop/0, is_game_over/0, player1/1, player2/1,
            p1_small_wins/1, p2_small_wins/1, board/1, winner_board/1,
            current_player/1, next_quadrant/1, game_mode/1,
            start_new_game/1, initialize_game_state/2
          ]).

:- use_module(library(random)).
:- use_module(ui).
:- use_module(persistence).
:- use_module(utils).
:- use_module(small_board).

:- dynamic
    board/1, winner_board/1, current_player/1, player1/1, player2/1,
    next_quadrant/1, p1_small_wins/1, p2_small_wins/1, game_mode/1.

% Loop principal do jogo
game_loop :-
    display_game_state,
    (   get_move(Quadrant, Cell) ->
        apply_move(Quadrant, Cell),
        ( is_game_over -> true ; (switch_player, game_loop) )
    ;   switch_player, game_loop ).

get_move(Quadrant, Cell) :-
    current_player(player(Symbol, Name)),
    ( Name == 'Bot' ->
        get_bot_move(Quadrant, Cell)
    ;   get_player_move(Symbol, Name, Quadrant, Cell)
    ).

get_player_move(Symbol, Name, Quadrant, Cell) :-
    next_quadrant(NextQ),
    format('\nTurno de ~w [~w].\n', [Name, Symbol]),
    ( NextQ == any -> writeln('Você pode jogar em qualquer quadrante.')
    ; Q_Display is NextQ + 1, format('Você deve jogar no quadrante ~d.\n', [Q_Display]) ),
    prompt_for_quadrant(NextQ, Quadrant),
    small_board:play_on_small_board(Quadrant, Cell).

prompt_for_quadrant(NextQ, Quadrant) :-
    write('Digite o quadrante (1-9) ou "salvar": '),
    read_line_to_string(user_input, Input),
    string_upper(Input, UpperInput),
    ( UpperInput == "SALVAR" -> save_game, fail ; true ),
    atom_number(UpperInput, Q_Num),
    Quadrant is Q_Num - 1,
    is_valid_quadrant_choice(NextQ, Quadrant), !.
prompt_for_quadrant(NextQ, Quadrant) :-
    writeln('Quadrante inválido ou não permitido. Tente novamente.'),
    prompt_for_quadrant(NextQ, Quadrant).

apply_move(Quadrant, Cell) :-
    current_player(player(Symbol, _)),
    board(OldBoard),
    global_index(Quadrant, Cell, GlobalIndex),
    replace(OldBoard, GlobalIndex, Symbol, NewBoard),
    retract(board(_)), assertz(board(NewBoard)),
    check_mini_board_winner(NewBoard, Quadrant, Symbol),
    update_next_quadrant(Cell).

check_mini_board_winner(Board, Quadrant, Symbol) :-
    winner_board(OldWinnerBoard), nth0(Quadrant, OldWinnerBoard, 'p'), !,
    get_mini_board(Board, Quadrant, MiniBoard),
    (   check_win(MiniBoard, Symbol) ->
        replace(OldWinnerBoard, Quadrant, Symbol, NewWinnerBoard),
        update_scores, writeln('** Você conquistou um quadrante! **'), sleep(1)
    ;   check_draw(MiniBoard) ->
        replace(OldWinnerBoard, Quadrant, 'd', NewWinnerBoard),
        writeln('** Quadrante empatado! **'), sleep(1)
    ;   NewWinnerBoard = OldWinnerBoard ),
    retract(winner_board(_)), assertz(winner_board(NewWinnerBoard)).
check_mini_board_winner(_, _, _).

is_game_over :-
    winner_board(WB), current_player(player(Symbol, Name)),
    (   check_win(WB, Symbol) ->
        ui:show_winner_art(Name), update_ranking(Name), press_enter_to_continue, !, true
    ;   check_draw(WB) ->
        ui:show_draw_art, press_enter_to_continue, !, true ).

% Lógica para jogada com o bot
get_bot_move(Quadrant, Cell) :-
    writeln('\nTurno do Bot. Pensando...'), sleep(2),
    next_quadrant(NextQ), winner_board(WB), board(B),
    current_player(player(BotSymbol, _)), player1(player(P1Symbol, _)),
    ( BotSymbol == P1Symbol -> opponent_symbol(P2Symbol), OpponentSymbol = P2Symbol ; OpponentSymbol = P1Symbol ),
    choose_best_quadrant(NextQ, WB, BotSymbol, OpponentSymbol, Quadrant),
    get_mini_board(B, Quadrant, MiniBoard),
    choose_best_cell(MiniBoard, BotSymbol, OpponentSymbol, Cell),
    Q_Display is Quadrant + 1, C_Display is Cell + 1,
    sleep(2).

choose_best_quadrant(any, WB, Bot, Opp, BestQ) :-
    findall(Q, (between(0, 8, Q), nth0(Q, WB, 'p')), Quads),
    find_best_quad_from_list(Quads, WB, Bot, Opp, -1, -1, BestQ), !.
choose_best_quadrant(Q, _, _, _, Q).

find_best_quad_from_list([Q|Qs], WB, Bot, Opp, CurrentBestScore, _, BestQ) :-
    score_quadrant(Q, WB, Bot, Opp, Score),
    Score > CurrentBestScore, !,
    find_best_quad_from_list(Qs, WB, Bot, Opp, Score, Q, BestQ).
find_best_quad_from_list([_|Qs], WB, Bot, Opp, BestS, BestQSoFar, BestQ) :-
    find_best_quad_from_list(Qs, WB, Bot, Opp, BestS, BestQSoFar, BestQ).
find_best_quad_from_list([], _, _, _, _, FinalBestQ, FinalBestQ).

score_quadrant(Q, WB, Bot, _, 10) :- would_win_game(Q, WB, Bot), !.
score_quadrant(Q, WB, _, Opp, 5) :- would_win_game(Q, WB, Opp), !.
score_quadrant(_, _, _, _, 1).

would_win_game(Q, WB, Symbol) :- replace(WB, Q, Symbol, TempWB), check_win(TempWB, Symbol).

choose_best_cell(MiniBoard, Bot, _, Cell) :- find_winning_move(MiniBoard, Bot, Cell), !.
choose_best_cell(MiniBoard, _, Opp, Cell) :- find_winning_move(MiniBoard, Opp, Cell), !.
choose_best_cell(MiniBoard, _, _, Cell) :- preferred_cell(Cell), nth0(Cell, MiniBoard, 'e'), !.
choose_best_cell(MiniBoard, _, _, Cell) :- findall(C, nth0(C, MiniBoard, 'e'), Cs), random_member(Cell, Cs).

find_winning_move(Board, Symbol, Cell) :- winning_line(Line), check_movable_line(Board, Symbol, Line, Cell).
check_movable_line(B, S, [I1,I2,I3], I1) :- nth0(I1,B,'e'), nth0(I2,B,S), nth0(I3,B,S).
check_movable_line(B, S, [I1,I2,I3], I2) :- nth0(I1,B,S), nth0(I2,B,'e'), nth0(I3,B,S).
check_movable_line(B, S, [I1,I2,I3], I3) :- nth0(I1,B,S), nth0(I2,B,S), nth0(I3,B,'e').

preferred_cell(4). preferred_cell(0). preferred_cell(2). preferred_cell(6). preferred_cell(8).
preferred_cell(1). preferred_cell(3). preferred_cell(5). preferred_cell(7).

% Inicialização do jogo
start_new_game(Mode) :-
    retractall(game_mode(_)), assertz(game_mode(Mode)),
    clear_screen,
    writeln('--- JOGADOR(A) 1 ---'),
    choose_player(1, [], P1),
    assertz(player1(P1)),
    ( Mode == pve ->
        opponent_symbol(BotSymbol),
        P2 = player(BotSymbol, 'Bot')
    ;
        writeln('--- JOGADOR(A) 2 ---'),
        choose_player(2, [P1], P2)
    ),
    initialize_game_state(P1, P2).

opponent_symbol(Symbol) :-
    player1(player(S1,_)), ( S1 == 'X' -> Symbol = 'O' ; Symbol = 'X' ).

initialize_game_state(P1, P2) :-
    retractall(board(_)), retractall(winner_board(_)), retractall(current_player(_)),
    retractall(player2(_)), retractall(next_quadrant(_)),
    retractall(p1_small_wins(_)), retractall(p2_small_wins(_)),
    length(IB, 81), maplist(=('e'), IB),
    length(IWB, 9), maplist(=('p'), IWB),
    assertz(board(IB)), assertz(winner_board(IWB)),
    assertz(player2(P2)),
    assertz(current_player(P1)), assertz(next_quadrant(any)),
    assertz(p1_small_wins(0)), assertz(p2_small_wins(0)).