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
:- encoding(utf8).

:- dynamic
    board/1, winner_board/1, current_player/1, player1/1, player2/1,
    next_quadrant/1, p1_small_wins/1, p2_small_wins/1, game_mode/1.

game_loop :-
    display_game_state,
    (   get_move(Quadrant, Cell) ->
        handle_move(Quadrant, Cell)
    ;   % Este 'else' trata o caso de o jogador ficar sem tempo.
        writeln('Tempo esgotado! Passando a vez...'),
        sleep(2),
        switch_player,
        game_loop
    ).

handle_move(quit, _) :- % Caso 1: O jogador saiu na tela de quadrantes.
    writeln('Partida encerrada. Retornando ao menu principal...'),
    sleep(2). 

handle_move(_, quit) :- % Caso 2: O jogador saiu na tela do tabuleiro menor.
    writeln('Partida encerrada. Retornando ao menu principal...'),
    sleep(2). 

handle_move(Quadrant, Cell) :- % Caso 3: Jogada normal.
    Quadrant \== quit, Cell \== quit, % Garante que não é um sinal de saída
    apply_move(Quadrant, Cell),
    (   is_game_over ->
        true % Fim de jogo, para o loop.
    ;
        switch_player,
        game_loop % Continua o jogo.
    ).

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
    write('Digite o quadrante (1–9), "salvar" para salvar ou "sair" para sair: '), 
    read_line_to_string(user_input, Input),
    string_upper(Input, UpperInput),
    handle_quadrant_input(UpperInput, NextQ, Quadrant).

handle_quadrant_input("SAIR", _, _) :-
    writeln('\nVocê escolheu sair da partida. Retornando ao menu principal...'),
    sleep(2),
    throw(user_quit). % Lança um sinal para encerrar o jogo

handle_quadrant_input("SALVAR", NextQ, Quadrant) :-
    save_game,
    writeln('...'),
    sleep(1),
    prompt_for_quadrant(NextQ, Quadrant).

handle_quadrant_input("", NextQ, Quadrant) :-
    writeln('! Entrada vazia. Digite um número de 1–9 ou "salvar".'),
    sleep(1),
    prompt_for_quadrant(NextQ, Quadrant).

handle_quadrant_input(Input, NextQ, Quadrant) :-
    catch(atom_number(Input, Q_Num), _, fail),
    Quadrant is Q_Num - 1,
    is_valid_quadrant_choice(NextQ, Quadrant), !.

handle_quadrant_input(_, NextQ, Quadrant) :-
    writeln('! Quadrante inválido ou não permitido. Tente novamente.'),
    sleep(1),
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
    current_player(player(_, Name)),
    (   check_win(MiniBoard, Symbol) ->
        replace(OldWinnerBoard, Quadrant, Symbol, NewWinnerBoard),
        update_scores,
        ( Name == 'Bot' -> 
            writeln('** Bot conquistou um quadrante! **')
        ; 
            writeln('** Você conquistou um quadrante! **')
        ),
        sleep(1)
    ;   check_draw(MiniBoard) ->
        replace(OldWinnerBoard, Quadrant, 'd', NewWinnerBoard),
        writeln('** Quadrante empatado! **'), sleep(1)
    ;   NewWinnerBoard = OldWinnerBoard ),
    retract(winner_board(_)), assertz(winner_board(NewWinnerBoard)).

is_game_over :-
    winner_board(WB), 
    current_player(CurrentPlayer),
    other_player(OtherPlayer),
    get_player_symbol(CurrentPlayer, CurrentSymbol),
    get_player_symbol(OtherPlayer, OtherSymbol),
    
    (   check_win(WB, CurrentSymbol) ->
        get_player_name(CurrentPlayer, WinnerName),
        (   WinnerName \= 'Bot' ->
            ui:show_winner_art(WinnerName)  % Humano venceu - arte normal
        ;   get_player_name(OtherPlayer, HumanName),
            ui:show_loser_art(HumanName)    % Bot venceu - arte de derrota para humano
        ),
        ( WinnerName \= 'Bot' -> update_ranking(WinnerName) ; true ),
        press_enter_to_continue, !, true
    ;   check_win(WB, OtherSymbol) ->
        get_player_name(OtherPlayer, WinnerName),
        (   WinnerName \= 'Bot' ->
            ui:show_winner_art(WinnerName)  % Humano venceu - arte normal
        ;   get_player_name(CurrentPlayer, HumanName),
            ui:show_loser_art(HumanName)    % Bot venceu - arte de derrota para humano
        ),
        ( WinnerName \= 'Bot' -> update_ranking(WinnerName) ; true ),
        press_enter_to_continue, !, true
    ;   check_draw(WB) ->
        get_winner_by_score(Winner),
        (   Winner == draw ->
            ui:show_draw_art,               % Empate real
            press_enter_to_continue, !, true
        ;   get_player_name(Winner, WinnerName),
            (   WinnerName \= 'Bot' ->
                ui:show_winner_art(WinnerName)  % Humano vence por pontos
            ;   other_player(HumanPlayer),
                get_player_name(HumanPlayer, HumanName),
                ui:show_loser_art(HumanName)    % Bot vence por pontos - derrota humano
            ),
            ( WinnerName \= 'Bot' -> update_ranking(WinnerName) ; true ),
            press_enter_to_continue, !, true
        )
    ).

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

score_quadrant(Q, WB, Bot, _, 10) :- 
    would_win_game(Q, WB, Bot), !.     % Bot pode ganhar

score_quadrant(Q, WB, _, Opp, 5) :- 
    would_win_game(Q, WB, Opp), !.     % Oponente pode ganhar

score_quadrant(Q, _, _, Opp, Score) :-
    board(B),
    get_mini_board(B, Q, MiniBoard),
    count_symbol(MiniBoard, Opp, CountOpp),
    Score is 3 - CountOpp.             % quanto menos peças do adversário, maior score

count_symbol(Board, Symbol, Count) :-
    include(==(Symbol), Board, Filtered),
    length(Filtered, Count).

would_win_game(Q, WB, Symbol) :-
    replace(WB, Q, Symbol, TempWB),
    check_win(TempWB, Symbol).

choose_best_cell(MiniBoard, Bot, _, Cell) :- 
    find_winning_move(MiniBoard, Bot, Cell), !.   % Jogada vencedora

choose_best_cell(MiniBoard, _, Opp, Cell) :- 
    find_winning_move(MiniBoard, Opp, Cell), !.   % Bloqueio

choose_best_cell(MiniBoard, Bot, Opp, Cell) :-
    % Pega número de símbolos do adversário por posição
    findall(I, nth0(I, MiniBoard, 'e'), EmptyPositions),
    ( EmptyPositions \= [] ->
        weighted_random_cell(MiniBoard, EmptyPositions, Cell)
    ; 
      findall(I, nth0(I, MiniBoard, 'e'), AllFree),
      random_member(Cell, AllFree)
    ).      % Probabilidade ponderada


weighted_random_cell(MiniBoard, CandidateCells, Cell) :-
    include({MiniBoard}/[I]>>nth0(I, MiniBoard, 'e'), CandidateCells, FreeCells),
    FreeCells \= [], !,
    random(0.0, 1.0, R),
    (   R < 0.40 -> Preferred = [4]            % 40% meio
    ;   R < 0.75 -> Preferred = [1,3,5,7]     % 35% laterais (0.40 -> 0.75)
    ;   Preferred = [0,2,6,8]                  % 25% cantos (0.75 -> 1.0)
    ),
    include({FreeCells}/[I]>>member(I, FreeCells), Preferred, Valid),
    ( Valid \= [] -> random_member(Cell, Valid)
    ; random_member(Cell, FreeCells)
    ).

find_winning_move(Board, Symbol, Cell) :-
    winning_line(Line),
    check_movable_line(Board, Symbol, Line, Cell).

check_movable_line(B, S, [I1,I2,I3], I1) :- nth0(I1,B,'e'), nth0(I2,B,S), nth0(I3,B,S).
check_movable_line(B, S, [I1,I2,I3], I2) :- nth0(I1,B,S), nth0(I2,B,'e'), nth0(I3,B,S).
check_movable_line(B, S, [I1,I2,I3], I3) :- nth0(I1,B,S), nth0(I2,B,S), nth0(I3,B,'e').


start_new_game(Mode) :-
    retractall(game_mode(_)), assertz(game_mode(Mode)),
    clear_screen,
    writeln('--- JOGADOR(A) 1 ---'),
    choose_player(1, [], P1),
    assertz(player1(P1)),
    ( Mode == pve ->
        opponent_symbol(BotSymbol),
        P2 = player(BotSymbol, 'Bot'),
        assertz(player2(P2))
    ;
        writeln('--- JOGADOR(A) 2 ---'),
        choose_player(2, [P1], P2),
        assertz(player2(P2))
    ),

    % Mostrar resumo antes do tabuleiro
    P1 = player(S1, N1),
    player2(P2A), P2A = player(S2, N2),

    writeln(''),
    writeln('======= Configuração do Jogo ======='),
    format('Jogador(a) 1: ~w [~w]~n', [N1, S1]),
    format('Jogador(a) 2: ~w [~w]~n', [N2, S2]),
    writeln('===================================='),
    sleep(2),

    initialize_game_state(P1, P2A).

opponent_symbol(Symbol) :-
    player1(player(S1,_)),
    ( S1 == 'X' -> Symbol = 'O' ; Symbol = 'X' ).

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
