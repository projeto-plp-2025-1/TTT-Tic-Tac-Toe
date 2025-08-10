# TTT - Tic Tac Toe

**TTT - Tic Tac Toe** é uma implementação do Super Jogo da Velha (Ultimate Tic Tac Toe), desenvolvida para explorar e comparar dois paradigmas de programação: **funcional (Haskell)** e **lógico (Prolog)**.

Este projeto foi criado como parte da disciplina de **Paradigmas de Linguagens de Programação (PLP)**, com o intuito de aplicar conceitos teóricos em um desafio prático, estratégico e interativo.

## Sobre o Jogo

O jogo segue a lógica do Super Jogo da Velha:

* O tabuleiro principal é 3x3, onde cada célula contém outro tabuleiro 3x3.
* Cada tabuleiro menor funciona como um jogo da velha comum.
* O jogador que vence um tabuleiro menor conquista o quadrante correspondente no tabuleiro principal.
* Vence quem formar uma linha com três quadrantes conquistados (horizontal, vertical ou diagonal).
* Se todos os quadrantes forem completados sem um vencedor, ganha quem tiver mais quadrantes conquistados. Em caso de empate, não há vencedor.

## Objetivos do Projeto

* Explorar e comparar dois paradigmas de programação: funcional (Haskell) e lógico (Prolog).
* Aplicar conceitos de lógica de jogo, controle de fluxo e manipulação de estado.
* Criar uma experiência de jogo acessível via terminal.
* Estimular a resolução criativa de problemas utilizando abordagens distintas.

## Funcionalidades

* Modo jogador contra jogador ou jogador contra bot.
* Escolha de nomes e personalização de símbolos.
* Interface no terminal com navegação por opções.
* Temporizador de 1 minuto por jogada.
* Encaminhamento automático: a posição escolhida define o próximo quadrante onde o adversário deve jogar.
* Escolha livre de quadrante em caso de tabuleiro finalizado.
* Sistema de salvamento e carregamento de partidas.
* Encerramento automático de jogada por tempo ou manual pelos jogadores.

## Tecnologias Utilizadas

* **[Haskell](https://www.haskell.org/):** lógica funcional, imutabilidade, recursividade.
* **[Prolog](https://www.swi-prolog.org/):** lógica declarativa, inferência, backtracking.
* Interface baseada em terminal (sem uso de interfaces gráficas).

## Como Executar

### Haskell:

Entrar na pasta "haskell"
Instalar todas as dependências necessárias ao projeto utilizando os comandos: cabal update e cabal install
Por fim, rodar o projeto utilizando o comando cabal run all

### Prolog: 

-----------

Certifique-se de ter o GHC (para Haskell) e o SWI-Prolog instalados na sua máquina.

## Equipe

* Carlos Artur Santana Sales
* Débora Sabrina de Oliveira Pereira
* Nicolas Wesley Correia Paz
* Nicole Brito Maracajá

Projeto de uso acadêmico, livre para fins educacionais.

## Referência

https://tictactoefree.com/pt/super-jogo-da-velha/regras
