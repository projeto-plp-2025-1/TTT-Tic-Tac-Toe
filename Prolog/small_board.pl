:- module(small_board,
          [ play_on_small_board/2,
            display_small_board/1
          ]).

:- use_module(ui).
:- use_module(utils).
:- use_module(game_logic).
:- use_module(persistence).
:- use_module(library(time)).
:- encoding(utf8).

small_template(0, [ "    1    2    3      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ A ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ B ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ C ",
                    "  ║  QUADRANTE 1 ║   ",
                    "  ╚══════════════╝   " ]).
small_template(1, [ "    4    5    6      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ A ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ B ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ C ",
                    "  ║  QUADRANTE 2 ║   ",
                    "  ╚══════════════╝   " ]).
small_template(2, [ "    7    8    9      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ A ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ B ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ C ",
                    "  ║  QUADRANTE 3 ║   ",
                    "  ╚══════════════╝   " ]).
small_template(3, [ "    1    2    3      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ D ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ E ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ F ",
                    "  ║  QUADRANTE 4 ║   ",
                    "  ╚══════════════╝   " ]).
small_template(4, [ "    4    5    6      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ D ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ E ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ F ",
                    "  ║  QUADRANTE 5 ║   ",
                    "  ╚══════════════╝   " ]).
small_template(5, [ "    7    8    9      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ D ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ E ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ F ",
                    "  ║  QUADRANTE 6 ║   ",
                    "  ╚══════════════╝   " ]).
small_template(6, [ "    1    2    3      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ G ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ H ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ I ",
                    "  ║  QUADRANTE 7 ║   ",
                    "  ╚══════════════╝   " ]).
small_template(7, [ "    4    5    6      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ G ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ H ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ I ",
                    "  ║  QUADRANTE 8 ║   ",
                    "  ╚══════════════╝   " ]).
small_template(8, [ "    7    8    9      ",
                    "  ╔══════════════╗   ",
                    "  ║    │    │    ║ G ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ H ",
                    "  ║────┼────┼────╢   ",
                    "  ║    │    │    ║ I ",
                    "  ║  QUADRANTE 9 ║   ",
                    "  ╚══════════════╝   " ]).

small_cell_to_coords(0, 2, 4).
small_cell_to_coords(1, 2, 9).
small_cell_to_coords(2, 2, 14).
small_cell_to_coords(3, 4, 4).
small_cell_to_coords(4, 4, 9).
small_cell_to_coords(5, 4, 14).
small_cell_to_coords(6, 6, 4).
small_cell_to_coords(7, 6, 9).
small_cell_to_coords(8, 6, 14).

play_on_small_board(Quadrant, Cell) :-
    writeln('\n--- Acessando o quadrante... ---'), sleep(1),
    catch(
        call_with_time_limit(120, get_small_board_move(Quadrant, Cell)),
        time_limit_exceeded,
        ( writeln('\n\x23F0 Tempo esgotado! Passando a vez...'), sleep(2), fail )
    ).

get_small_board_move(Quadrant, FinalCell) :-
    get_time(StartTime),
    repeat,
      clear_screen,
      display_small_board(Quadrant),
      get_time(CurrentTime),
      Elapsed   is floor(CurrentTime - StartTime),
      Remaining is 120 - Elapsed,
      ( Remaining >= 0
        -> format('\n⏳ Tempo restante: ~w segundos\n', [Remaining])
        ;  writeln('\n⏳ Tempo restante: 0 segundos')
      ),
      current_player(player(Symbol, _)),
      format('Turno do Jogador(a): [~w] (Tabuleiro Menor)\n\n', [Symbol]),

      write('Digite "salvar" para salvar, "V" para ver o tabuleiro maior, ou pressione ENTER para jogar: '),
      read_line_to_string(user_input, OpInput0),
      string_upper(OpInput0, OpInput),

      ( OpInput == "V" ->
          process_view_option, fail
      ; OpInput == "SALVAR" ->
          save_game, fail
      ; OpInput == "" ->
          write('Linha (A–I): '),
          read_line_to_string(user_input, LinhaInput0),
          string_upper(LinhaInput0, LinhaUpper),
          ( LinhaUpper == "" ->
              writeln('! Entrada vazia para a LINHA. Tente novamente.'), sleep(1), fail
          ; \+ valid_row(LinhaUpper) ->
              writeln('! Linha inválida. Use letras de A a I.'), sleep(1), fail
          ; write('Coluna (1–9): '),
            read_line_to_string(user_input, ColunaInput0),
            ( ColunaInput0 == "" ->
                writeln('! Entrada vazia para a COLUNA. Tente novamente.'), sleep(1), fail
            ; \+ valid_col(ColunaInput0) ->
                writeln('! Coluna inválida. Use números de 1 a 9.'), sleep(1), fail
            ; process_small_board_input(LinhaUpper, ColunaInput0, Quadrant, FinalCell)
            )
          )
      ; writeln('! Opção inválida. Digite "salvar", "V" ou pressione ENTER para jogar.'), sleep(1), fail
      ),
    !.

process_view_option :-
    clear_screen, writeln('Visualizando tabuleiro maior:'), nl,
    ui:display_game_state, nl,
    press_enter_to_continue.

process_small_board_input(Linha, Coluna, Quadrant, Cell) :-
    string_length(Linha, 1),
    string_length(Coluna, 1),
    atom_concat(Linha, Coluna, Jogada),
    ( parse_algebraic_cell(Jogada, Cell)
    ; ( atom_number(Jogada, Num), Cell is Num - 1 )
    ),
    is_valid_cell_choice(Quadrant, Cell), !.
process_small_board_input(_, _, _, _) :-
    writeln('--- JOGADA INVÁLIDA! Tente novamente. ---'), sleep(1.5), fail.

display_small_board(Quadrant) :-
    board(BigBoard),
    get_mini_board(BigBoard, Quadrant, MiniBoard),
    small_template(Quadrant, Template),
    build_small_display(0, MiniBoard, Template, FinalDisplay),
    print_lines(FinalDisplay).

build_small_display(9, _, Display, Display) :- !.
build_small_display(Cell, MiniBoard, InDisplay, OutDisplay) :-
    nth0(Cell, MiniBoard, Symbol),
    ( Symbol == 'e' ->
        TempDisplay = InDisplay
    ; small_cell_to_coords(Cell, R, C),
      set_char_at(R, C, Symbol, InDisplay, TempDisplay)
    ),
    NextCell is Cell + 1,
    build_small_display(NextCell, MiniBoard, TempDisplay, OutDisplay).

valid_row(S) :-
    string_length(S, 1),
    string_chars(S, [C]),
    member(C, ['A','B','C','D','E','F','G','H','I']).

valid_col(S) :-
    string_length(S, 1),
    string_chars(S, [C]),
    member(C, ['1','2','3','4','5','6','7','8','9']).
