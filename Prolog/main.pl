:- use_module(game_logic).
:- use_module(ui).
:- use_module(persistence).
:- use_module(utils).
:- use_module(small_board).
:- encoding(utf8).

main :-
    repeat,
    clear_screen,
    show_title_art,
    show_menu_options,
    read_line_to_string(user_input, Option),
    process_menu_option(Option),
    ( Option == "6" ; Option == "1" ; Option == "2" ; Option == "3" ; Option == "4" ; Option == "5" ), !,
    ( Option == "6" -> true ; main).

% Processa a escolha do usuário.
process_menu_option("1") :- start_new_game(pvp), game_loop.
process_menu_option("2") :- start_new_game(pve), game_loop.
process_menu_option("3") :- (load_game -> game_loop ; true).
process_menu_option("4") :- show_rules, press_enter_to_continue.
process_menu_option("5") :- show_ranking, press_enter_to_continue.
process_menu_option("6") :- writeln('Saindo do jogo. Até mais!').
process_menu_option(_)   :- writeln('Opção inválida!'), sleep(1).