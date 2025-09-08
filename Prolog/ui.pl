:- module(ui,
          [ display_game_state/0,
            show_rules/0,
            show_title_art/0,
            choose_player/3,
            show_winner_art/1,
            show_draw_art/0,
            show_menu_options/0
          ]).

:- use_module(utils).
:- use_module(game_logic).

print_lines([]).
print_lines([H|T]) :- writeln(H), print_lines(T).

replace_nth(L, I, X, R) :- nth0(I, L, _, Tmp), nth0(I, R, X, Tmp).

set_char_at(R, C, Char, In, Out) :-
    nth0(R, In, OldStr), string_codes(OldStr, OldCodes),
    char_code(Char, CharCode),
    replace_nth(OldCodes, C, CharCode, NewCodes),
    string_codes(NewStr, NewCodes),
    replace_nth(In, R, NewStr, Out).

% Template do tabuleiro principal
template_line(0,  "    1    2    3     4    5    6     7    8    9    ").
template_line(1,  "                                                   ").
template_line(2,  "       │    │    ║    │    |    ║    │    │      A ").
template_line(3,  "   ────┼────┼────╢────┼────┼────╢────┼────┼────    ").
template_line(4,  "       │    │    ║    │    │    ║    │    │      B ").
template_line(5,  "   ────┼────┼────╢────┼────┼────╢────┼────┼────    ").
template_line(6,  "       │    │    ║    │    │    ║    │    │      C ").
template_line(7,  "     QUADRANTE 1 ║  QUADRANTE 2 ║  QUADRANTE 3     ").
template_line(8,  "   ══════════════╬══════════════╬══════════════    ").
template_line(9,  "       │    │    ║    │    │    ║    │    │      D ").
template_line(10, "   ────┼────┼────╢────┼────┼────╢────┼────┼────    ").
template_line(11, "       │    │    ║    │    │    ║    │    │      E ").
template_line(12, "   ────┼────┼────╢────┼────┼────╢────┼────┼────    ").
template_line(13, "       │    │    ║    │    │    ║    │    │      F ").
template_line(14, "     QUADRANTE 4 ║  QUADRANTE 5 ║  QUADRANTE 6     ").
template_line(15, "   ══════════════╬══════════════╬══════════════    ").
template_line(16, "       │    │    ║    │    │    ║    │    │      G ").
template_line(17, "   ────┼────┼────╢────┼────┼────╢────┼────┼────    ").
template_line(18, "       │    │    ║    │    │    ║    │    │      H ").
template_line(19, "   ────┼────┼────╢────┼────┼────╢────┼────┼────    ").
template_line(20, "       │    │    ║    │    │    ║    │    │      I ").
template_line(21, "     QUADRANTE 7 ║  QUADRANTE 8 ║  QUADRANTE 9     ").
template_line(22, "                                                   ").

% Mapeamento das coordenadas
cell_to_coords(Quadrant, Cell, R, C) :-
    QuadRow is Quadrant // 3, 
    QuadCol is Quadrant mod 3,

    CellRow is Cell // 3,    
    CellCol is Cell mod 3,

    R_Base is QuadRow * 7, 
    R_Offset is CellRow * 2, 
    R is R_Base + R_Offset + 2,

    C_Base is QuadCol * 15, 
    C_Offset is CellCol * 5,
    C is C_Base + C_Offset + 5. 

% Lógica de exibição
display_game_state :-
    clear_screen,
    board(Board), winner_board(WinnerBoard),
    findall(Line, template_line(_, Line), Template),
    build_display(Board, WinnerBoard, Template, FinalDisplay),
    print_lines(FinalDisplay),
    show_scoreboard.

build_display(Board, WinnerBoard, InDisplay, OutDisplay) :- build_display_quads(0, Board, WinnerBoard, InDisplay, OutDisplay).
build_display_quads(9, _, _, Display, Display) :- !.
build_display_quads(Quad, Board, WB, InDisplay, OutDisplay) :-
    nth0(Quad, WB, State),
    ( State == 'p' -> build_display_cells(Quad, 0, Board, InDisplay, TempDisplay)
    ; paint_quadrant(Quad, State, InDisplay, TempDisplay) ),
    NextQuad is Quad + 1,
    build_display_quads(NextQuad, Board, WB, TempDisplay, OutDisplay).

build_display_cells(_Quad, 9, _, Display, Display) :- !.
build_display_cells(Quad, Cell, Board, InDisplay, OutDisplay) :-
    global_index(Quad, Cell, GlobalIndex), nth0(GlobalIndex, Board, Symbol),
    ( Symbol == 'e' -> TempDisplay = InDisplay
    ; cell_to_coords(Quad, Cell, R, C), set_char_at(R, C, Symbol, InDisplay, TempDisplay) ),
    NextCell is Cell + 1,
    build_display_cells(Quad, NextCell, Board, TempDisplay, OutDisplay).

paint_quadrant(Quad, State, InDisplay, OutDisplay) :-
    ( State == 'd' -> Symbol = '#' ; Symbol = State ),
    paint_quadrant_cells(Quad, 0, Symbol, InDisplay, OutDisplay).

paint_quadrant_cells(_Quad, 9, _, Display, Display) :- !.
paint_quadrant_cells(Quad, Cell, Symbol, InDisplay, OutDisplay) :-
    cell_to_coords(Quad, Cell, R, C), set_char_at(R, C, Symbol, InDisplay, TempDisplay),
    NextCell is Cell + 1,
    paint_quadrant_cells(Quad, NextCell, Symbol, TempDisplay, OutDisplay).

show_scoreboard :-
    player1(player(S1, N1)), player2(player(S2, N2)),
    p1_small_wins(W1), p2_small_wins(W2), nl,
    writeln('------- PLACAR (Quadrantes) -------'),
    format('~w [~w]: ~d\n', [N1, S1, W1]),
    format('~w [~w]: ~d\n', [N2, S2, W2]),
    writeln('----------------------------------').

choose_player(PlayerNum, UsedPlayers, player(Symbol, Name)) :-
    available_characters(Chars),
    format('Jogador(a) ~d, escolha seu personagem:\n', [PlayerNum]),
    print_characters(Chars, UsedPlayers),
    write('Digite o número do personagem: '),
    read_line_to_string(user_input, NumStr),
    atom_number(NumStr, Num),
    nth1(Num, Chars, char(Symbol, DefaultName)),
    \+ member(player(Symbol, _), UsedPlayers), !,
    format('Personagem escolhido: [~w] ~w\n', [Symbol, DefaultName]),
    write('Digite um nome (ou Enter para usar o padrão): '),
    read_line_to_string(user_input, NameInput),
    ( NameInput == "" -> Name = DefaultName ; Name = NameInput ),
    format('Jogador(a) ~d é ~w [~w]\n', [PlayerNum, Name, Symbol]).
choose_player(PlayerNum, UsedPlayers, Player) :-
    writeln('Seleção inválida ou personagem já escolhido. Tente novamente.'),
    choose_player(PlayerNum, UsedPlayers, Player).

show_menu_options :-
    writeln('  1. Novo Jogo (2 Jogadores)'),
    writeln('  2. Novo Jogo (vs. Bot)'),
    writeln('  3. Continuar Jogo'),
    writeln('  4. Ver Regras'),
    writeln('  5. Ver Ranking'),
    writeln('  6. Sair'),
    nl,
    write('Escolha uma opção: ').

% Regras do jogo
show_rules :-
    clear_screen,
    writeln("--- REGRAS DO SUPER JOGO DA VELHA ---"),
    writeln("1. O jogo é um tabuleiro 3x3, onde cada célula é um tabuleiro 3x3 menor."),
    writeln("2. O objetivo é vencer 3 tabuleiros menores em linha, coluna ou diagonal."),
    writeln("3. A sua jogada em uma célula de um tabuleiro menor determina em qual"),
    writeln("   tabuleiro maior o seu oponente deverá jogar."),
    writeln("4. Se o quadrante de destino já foi vencido ou está cheio, o oponente"),
    writeln("   pode escolher jogar em qualquer outro quadrante livre."),
    writeln("5. Vence quem primeiro formar uma linha, coluna ou diagonal com os"),
    writeln("   quadrantes conquistados.").

% Arte do TÍTULO do jogo.
show_title_art :-
    writeln("     ╔════════════════════════════════════════════════════════════════════════════════════════════════╗    "),
    writeln("     ║                                                                                                ║    "),
    writeln("     ║                ██╗   ██╗██╗   ████████╗██╗███╗   ███╗ █████╗████████╗███████╗                  ║    "),
    writeln("     ║                ██║   ██║██║   ╚══██╔══╝██║████╗ ████║██╔══██╚══██╔══╝██╔════╝                  ║    "),
    writeln("     ║                ██║   ██║██║      ██║   ██║██╔████╔██║███████║  ██║   █████╗                    ║    "),
    writeln("     ║                ██║   ██║██║      ██║   ██║██║╚██╔╝██║██╔══██║  ██║   ██╔══╝                    ║    "),
    writeln("     ║                ████████║███████╗ ██║   ██║██║ ╚═╝ ██║██║  ██║  ██║   ███████╗                  ║    "),
    writeln("     ║                ╚═══════╝╚══════╝ ╚═╝   ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝  ╚═╝   ╚══════╝                  ║    "),
    writeln("     ║                                                                                                ║    "),
    writeln("     ║      ████████╗██╗ ██████╗     ████████╗ █████╗  ██████╗     ████████╗ ██████╗ ███████╗         ║    "),
    writeln("     ║      ╚══██╔══╝██║██╔════╝     ╚══██╔══╝██╔══██╗██╔════╝     ╚══██╔══╝██╔══ ██╗██╔════╝         ║    "),
    writeln("     ║         ██║   ██║██║     █████╗  ██║   ███████║██║     █████╗  ██║   ██║   ██║█████╗           ║    "),
    writeln("     ║         ██║   ██║██║     ╚════╝  ██║   ██╔══██║██║     ╚════╝  ██║   ██║   ██║██╔══╝           ║    "),
    writeln("     ║         ██║   ██║╚██████╗        ██║   ██║  ██║╚██████╗        ██║   ╚██████╔╝███████╗         ║    "),
    writeln("     ║         ╚═╝   ╚═╝ ╚═════╝        ╚═╝   ╚═╝  ╚═╝ ╚═════╝        ╚═╝    ╚═════╝ ╚══════╝         ║    "),
    writeln("     ║                                                                                                ║    "), 
    writeln("     ╚════════════════════════════════════════════════════════════════════════════════════════════════╝    "), nl.


% Exibe a tela de FIM DE JOGO e o nome do vencedor.
show_winner_art(WinnerName) :-
    clear_screen,
    writeln("  ╔═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗   "),
    writeln("  ║                                                                                                                                                     ║   "),
    writeln("  ║    ████████╗███████╗███╗   ███╗ ██████╗ ███████╗    ██╗   ██╗███╗   ███╗    ██╗   ██╗███████╗███╗   ██╗ ██████╗███████╗██████╗  ██████╗ ██████╗     ║   "),
    writeln("  ║    ╚══██╔══╝██╔════╝████╗ ████║██╔═══██╗██╔════╝    ██║   ██║████╗ ████║    ██║   ██║██╔════╝████╗  ██║██╔════╝██╔════╝██╔══██╗██╔═══██╗██╔══██╗    ║   "),
    writeln("  ║       ██║   █████╗  ██╔████╔██║██║   ██║███████╗    ██║   ██║██╔████╔██║    ██║   ██║█████╗  ██╔██╗ ██║██║     █████╗  ██║  ██║██║   ██║██████╔╝    ║   "),
    writeln("  ║       ██║   ██╔══╝  ██║╚██╔╝██║██║   ██║╚════██║    ██║   ██║██║╚██╔╝██║    ╚██╗ ██╔╝██╔══╝  ██║╚██╗██║██║     ██╔══╝  ██║  ██║██║   ██║██╔══██╗    ║   "),
    writeln("  ║       ██║   ███████╗██║ ╚═╝ ██║╚██████╔╝███████║    ╚██████╔╝██║ ╚═╝ ██║     ╚████╔╝ ███████╗██║ ╚████║╚██████╗███████╗██████╔╝╚██████╔╝██║  ██║    ║   "),
    writeln("  ║       ╚═╝   ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚══════╝     ╚═════╝ ╚═╝     ╚═╝      ╚═══╝  ╚══════╝╚═╝  ╚═══╝ ╚═════╝╚══════╝╚═════╝  ╚═════╝ ╚═╝  ╚═╝    ║   "),
    writeln("  ║                                                                                                                                                     ║   "),
    writeln("  ╚═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝   "),
    format("                                                🏆 Parabéns, ~w! Você venceu a partida.                                                        ", [WinnerName]),
    nl.

show_draw_art :-
    clear_screen,
    writeln('     ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════════╗'),
    writeln('     ║                                                                                                               ║'),
    writeln('     ║     █████╗     ██╗   ██╗███████╗██╗     ██╗  ██╗ █████╗      ██████╗  █████╗ ███╗   ██╗██╗  ██╗ █████╗ ██╗    ║'),
    writeln('     ║    ██╔══██╗    ██║   ██║██╔════╝██║     ██║  ██║██╔══██╗    ██╔════╝ ██╔══██╗████╗  ██║██║  ██║██╔══██╗██║    ║'),
    writeln('     ║    ███████║    ██║   ██║█████╗  ██║     ███████║███████║    ██║  ███╗███████║██╔██╗ ██║███████║███████║██║    ║'),
    writeln('     ║    ██╔══██║    ╚██╗ ██╔╝██╔══╝  ██║     ██╔══██║██╔══██║    ██║   ██║██╔══██║██║╚██╗██║██╔══██║██╔══██║╚═╝    ║'),
    writeln('     ║    ██║  ██║     ╚████╔╝ ███████╗███████╗██║  ██║██║  ██║    ╚██████╔╝██║  ██║██║ ╚████║██║  ██║██║  ██║██╗    ║'),
    writeln('     ║    ╚═╝  ╚═╝      ╚═══╝  ╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝     ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝    ║'),
    writeln('     ║                                                                                                               ║'),
    writeln('     ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════════╝'), nl.