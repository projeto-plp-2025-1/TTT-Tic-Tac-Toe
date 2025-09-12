:- module(persistence,
          [ save_game/0,
            load_game/0,
            register_player/1,
            clear_players/0,
            update_ranking/1,
            show_ranking/0
          ]).

:- use_module(game_logic).
:- use_module(utils).
:- encoding(utf8).

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
    writeln('✅ Jogo salvo com sucesso!').

load_game :-
    exists_file('dados/salvo.txt'), !,
    open('dados/salvo.txt', read, Stream),
    read_file_to_state(Stream),
    close(Stream),
    writeln('✅ Jogo carregado com sucesso!').
load_game :-
    writeln('⚠️ Nenhum jogo salvo encontrado.'), fail.

read_file_to_state(Stream) :-
    read_term(Stream, Term, []),
    ( Term == end_of_file -> true
    ; retractall(Term), assertz(Term), read_file_to_state(Stream)
    ).

% arquivo do jogo

ensure_players_file :-
    ensure_directory_exists('dados/'),
    ( exists_file('dados/jogadores.txt') -> true
    ; open('dados/jogadores.txt', write, S), close(S)
    ).

load_players(Players) :-
    ensure_players_file,
    read_file_to_terms('dados/jogadores.txt', Players).

save_players(Players) :-
    open('dados/jogadores.txt', write, S),
    forall(member(player(Name, Score), Players),
           ( write_term(S, player(Name, Score), [quoted(true)]),
             write(S, '.\n'))),
    close(S).

clear_players :-
    ensure_players_file,
    open('dados/jogadores.txt', write, S), close(S).

register_player(Name) :-
    string_upper(Name, Upper), Upper == "BOT", !,
    writeln('⚠ O nome "Bot" é reservado e não pode ser usado por jogadores humanos.'),
    fail.
register_player(Name) :-
    load_players(Players),
    ( member(player(Name, _), Players) -> true  % já registrado
    ; save_players([player(Name, 0) | Players])
    ).

update_ranking('Bot') :- !.  % bot não recebe pontos
update_ranking(WinnerName) :-
    load_players(Players),
    ( select(player(WinnerName, OldScore), Players, Rest) ->
        NewScore is OldScore + 1,
        NewPlayers = [player(WinnerName, NewScore) | Rest]
    ; NewPlayers = [player(WinnerName, 1) | Players]
    ),
    save_players(NewPlayers).

show_ranking :-
    clear_screen,
    writeln("==========================="),
    writeln("      RANKING TOP 5"),
    writeln("===========================\n"),
    ( load_players(Players),
      Players \= [] ->
        predsort(compare_scores, Players, Sorted),
        print_top_5(Sorted, 1)
    ; writeln("Nenhum(a) jogador(a) registrado(a) ainda.\n")
    ).

compare_scores(Delta, player(_, S1), player(_, S2)) :-
    compare(Delta, S2, S1).  % ordena decrescente

print_top_5([], _).
print_top_5(_, 6).
print_top_5([player(Name, Score) | T], N) :-
    format('~d. ~w - ~d pontos\n', [N, Name, Score]),
    N1 is N + 1,
    print_top_5(T, N1).
