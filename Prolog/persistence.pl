:- module(persistence,
          [ save_game/0,
            load_game/0,
            show_ranking/0,
            update_ranking/1,
            register_player/2,
            clear_players/0
          ]).

:- use_module(game_logic).
:- use_module(utils).
:- encoding(utf8).

% ================================
% Persistência do jogo
% ================================
save_game :-
    ensure_directory_exists('dados/'),
    open('dados/salvo.txt', write, Stream),
    player1(P1), write_term(Stream, player1(P1), [quoted(true)]), write(Stream, '.\n'),
    player2(P2), write_term(Stream, player2(P2), [quoted(true)]), write(Stream, '.\n'),
    current_player(CP), write_term(Stream, current_player(CP), [quoted(true)]), write(Stream, '.\n'),
    board(B), write_term(Stream, board(B), [quoted(true)]), write(Stream, '.\n'),
    winner_board(WB), write_term(Stream, winner_board(WB), [quoted(true)]), write(Stream, '.\n'),
    next_quadrant(NQ), write_term(Stream, next_quadrant(NQ), [quoted(true)]), write(Stream, '.\n'),
    p1_small_wins(W1), write_term(Stream, p1_small_wins(W1), [quoted(true)]), write(Stream, '.\n'),
    p2_small_wins(W2), write_term(Stream, p2_small_wins(W2), [quoted(true)]), write(Stream, '.\n'),
    game_mode(M), write_term(Stream, game_mode(M), [quoted(true)]), write(Stream, '.\n'),
    close(Stream),
    writeln('Jogo salvo com sucesso! Retornando ao menu...'), sleep(2).

load_game :-
    exists_file('dados/salvo.txt'), !,
    open('dados/salvo.txt', read, Stream),
    read_file_to_state(Stream),
    close(Stream),
    writeln('Jogo carregado com sucesso!'), sleep(2).
load_game :-
    writeln('Nenhum jogo salvo encontrado.'), sleep(2), fail.

read_file_to_state(Stream) :-
    read_term(Stream, Term, []),
    ( Term == end_of_file -> true
    ; retractall(Term), assertz(Term), read_file_to_state(Stream) ).

% ================================
% Ranking
% ================================
update_ranking(WinnerName) :-
    ( WinnerName == 'Bot' -> true
    ; ensure_directory_exists('dados/'),
      ( exists_file('dados/ranking.txt') -> read_file_to_terms('dados/ranking.txt', R) ; R = [] ),
      ( member(player(WinnerName, W), R) ->
          NewW is W + 1,
          select(player(WinnerName, W), R, TR),
          NR = [player(WinnerName, NewW) | TR]
      ; NR = [player(WinnerName, 1) | R] ),
      open('dados/ranking.txt', write, S), write_ranking(S, NR), close(S)
    ).

show_ranking :-
    clear_screen,
    writeln("==========================="),
    writeln("      RANKING TOP 5"),
    writeln("===========================\n"),
    ( exists_file('dados/ranking.txt') ->
        read_file_to_terms('dados/ranking.txt', R),
        predsort(compare_scores, R, SR),
        print_top_5(SR, 1)
    ; writeln("Nenhum(a) jogador(a) registrado(a) ainda.\n") ).

compare_scores(Delta, player(_, W1), player(_, W2)) :-
    compare(Delta, W2, W1).  

print_top_5([], _).
print_top_5(_, 6).
print_top_5([player(Name, Wins) | T], N) :-
    format('~d. ~w - ~d vitórias\n', [N, Name, Wins]),
    N1 is N + 1,
    print_top_5(T, N1).

% ================================
% Registro de jogadores
% ================================
ensure_players_file :-
    ensure_directory_exists('dados/'),
    ( exists_file('dados/jogadores.txt') -> true
    ; open('dados/jogadores.txt', write, S), close(S) ).

load_players(Players) :-
    ensure_players_file,
    read_file_to_terms('dados/jogadores.txt', Players).

save_players(Players) :-
    open('dados/jogadores.txt', write, S),
    forall(member(player(Name), Players),
           ( write_term(S, player(Name), [quoted(true)]),
             write(S, '.\n'))),
    close(S).

clear_players :-
    ensure_players_file,
    open('dados/jogadores.txt', write, S), close(S).

% Registra jogador garantindo unicidade
register_player(BaseName, UniqueName) :-
    ( string_upper(BaseName, "BOT") ->
        writeln('⚠ O nome "Bot" é reservado e não pode ser escolhido por jogadores humanos.'),
        fail
    ; true ),
    load_players(Players),
    generate_unique_name(BaseName, Players, UniqueName),
    ( BaseName == UniqueName ->
        true
    ; format('⚠ O nome "~w" já existe. Seu nome será ajustado para "~w".~n', [BaseName, UniqueName]) ),
    save_players([player(UniqueName) | Players]).

generate_unique_name(Name, Players, UniqueName) :-
    ( member(player(Name), Players) ->
        find_unique_suffix(Name, 2, Players, UniqueName)
    ; UniqueName = Name ).

find_unique_suffix(Base, N, Players, Unique) :-
    atomic_list_concat([Base, N], Unique),
    ( member(player(Unique), Players) ->
        N1 is N + 1,
        find_unique_suffix(Base, N1, Players, Unique)
    ; true ).
