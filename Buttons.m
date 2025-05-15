(* ::Package:: *)

(* Inizio del pacchetto Buttons, che dipende da altri tre pacchetti: Board, Euclide e Dice *)
BeginPackage["Buttons`", {"Board`", "Euclide`", "Dice`"}]

(* Dichiarazione dell'uso pubblico delle funzioni esportate dal pacchetto *)
Restart::usage = "Restart[position_, dice_, gameOver_, seed_, showEuclide_, diceButtonEnabled_] crea il bottone per ricominciare il gioco.";
RollDice::usage = "RollDice[diceButtonEnabled_, diceValue_, num1_, num2_, euclideComponent_, playerPosition_, obstaclesList_, totalBoardCells_, isGameOver_, showEuclide_] crea il bottone per tirare il dado.";
ClearFields::usage = "ClearFields[quotient_, remainder_, message_] restituisce un bottone che ripulisce i campi.";

(* Inizio del contesto privato dove vengono definite le funzioni *)
Begin["`Private`"]

(* Imposta gli attributi per evitare la valutazione immediata degli argomenti *)
SetAttributes[ClearFields, HoldAll]

(* Definizione del bottone che pulisce i campi del quoziente, resto e messaggio *)
ClearFields[quotient_, remainder_, message_] := Button[
  "Pulisci Campi",  (* Etichetta del bottone *)
  (
    quotient = "";    (* Pulisce il quoziente *)
    remainder = "";   (* Pulisce il resto *)
    message = "";     (* Pulisce il messaggio *)
  ),
  BaseStyle -> {FontWeight -> Bold}
]

(* Imposta HoldAll per impedire la valutazione prematura degli argomenti *)
SetAttributes[Restart, HoldAll]

(* Bottone per ricominciare il gioco *)
Restart[position_, dice_, gameOver_, seed_, showEuclide_, diceButtonEnabled_] := Button[
  TextCell["Ricomincia", "Text", FontColor -> White],  (* Testo del bottone *)
  (
    SeedRandom[seed];         (* Reimposta il seed del generatore di numeri casuali *)
    position = 1;             (* Riporta il giocatore alla posizione iniziale *)
    dice = 0;                 (* Reimposta il valore del dado *)
    gameOver = False;         (* Imposta che il gioco non è finito *)
    showEuclide = False;      (* Nasconde la componente Euclidea *)
    diceButtonEnabled = True; (* Riabilita il pulsante del dado *)
  ),
  Background -> RGBColor[0.2, 0.6, 0.8],  (* Colore di sfondo *)
  FrameMargins -> 10,                    (* Margini interni *)
  Appearance -> None,                    (* Aspetto piatto *)
  BaseStyle -> {
    FontSize -> 14,                      (* Dimensione del font *)
    FontColor -> White,                 (* Colore del font *)
    FontWeight -> "Bold",               (* Grassetto *)
    FontFamily -> "Arial"               (* Tipo di carattere *)
  },
  ImageSize -> {120, Automatic},        (* Dimensione del bottone *)
  Method -> "Queued",                   (* Esecuzione asincrona *)
  ContentPadding -> 10                  (* Padding interno *)
]

(* Evita la valutazione prematura *)
SetAttributes[RollDice, HoldAll]

(* Bottone per tirare il dado *)
RollDice[diceButtonEnabled_, diceValue_, num1_, num2_, euclideComponent_, playerPosition_, obstaclesList_, totalBoardCells_, isGameOver_, showEuclide_] := 
  Button[
    TextCell["Tira il dado", "Text", FontColor -> White],  (* Testo del bottone *)
    (
      diceButtonEnabled = False;  (* Disabilita temporaneamente il pulsante *)
      diceValue = RandomInteger[{1, 6}];         (* Estrae un numero da 1 a 6 *)
      num1 = RandomInteger[{10, 99}];            (* Primo numero casuale per Euclide *)
      num2 = RandomInteger[{1, num1 - 1}];       (* Secondo numero, più piccolo *)

      (* Crea il componente Euclide *)
      euclideComponent = EuclideComponent[num1, num2, diceValue,
        Function[gcdResult,
          Module[{newPosition},
            newPosition = GetNextPosition[
              playerPosition, diceValue, obstaclesList, totalBoardCells
            ];  (* Calcola la nuova posizione del giocatore *)
            playerPosition = newPosition;  (* Aggiorna la posizione *)
            If[playerPosition >= totalBoardCells, isGameOver = True];  (* Controlla se il gioco è finito *)
            showEuclide = False;           (* Nasconde EuclideComponent dopo il movimento *)
            diceButtonEnabled = True;      (* Riabilita il pulsante *)
          ]
        ]
      ];
      showEuclide = True;  (* Mostra il componente Euclide *)
    ),
    ImageSize -> {150, Automatic},              (* Dimensione del bottone *)
    Enabled -> Dynamic[diceButtonEnabled && !isGameOver],  (* Abilitazione condizionale *)
    Appearance -> "Frameless",                  (* Aspetto senza cornice *)
    Background -> RGBColor[0.3, 0.6, 0.3],      (* Colore di sfondo verde *)
    BaseStyle -> {
      FontSize -> 14,                           (* Font size *)
      FontColor -> White,                       (* Colore testo *)
      FontWeight -> "Bold"                      (* Testo in grassetto *)
    }
  ]

(* Fine del contesto privato *)
End[]

(* Fine del pacchetto *)
EndPackage[]
