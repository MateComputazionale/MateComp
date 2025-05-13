(* ::Package:: *)
(* :Title: Gioco dell'Anatra con Algoritmo di Euclide *)
(* :Context: IDivisoriPackage*)
(* :Author: Gruppo 8: Sara Casadio, Enrico Ferraiolo, Luca Orlandello, Federica Santisi *)
(* :Summary: Questo pacchetto implementa il Gioco dell'Anatra con l'Algoritmo di Euclide.
   Il giocatore tira un dado e avanza sul tabellone. In ogni turno dovrà risolvere
   un problema di calcolo del MCD (Massimo Comun Divisore) usando l'algoritmo di Euclide.*)
(* :Copyright: 
   MIT License

Copyright (c) 2025 Sara Casadio, Enrico Ferraiolo, Luca Orlandello, Federica Santisi

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)
(* :Package Version: 1.0*)
(* :Mathematica Version: 14.2*)
(* :History: 16/05/2025*)
(* :Keywords: Gioco dell'Anatra, Euclide, MCD, GCD, didattica, matematica *)
(* :Limitations: For educational purposes only.*)
(* :Target: Ragazzi delle medie *)
(* Definizione del pacchetto "IDivisori" *)
BeginPackage["IDivisori`"]

(* Dichiarazione dei simboli esportati dal pacchetto *)
StartGame::usage = "StartGame[] starts the Game of the Goose with Euclid's Algorithm.";

(* Inizio della sezione privata del pacchetto *)
Begin["`Private`"]

(* Importazione dei pacchetti necessari per il funzionamento del gioco *)
Get["Buttons.m"];    (* Pacchetto per la gestione dei pulsanti nell'interfaccia *)
(* Dichiarazione dei simboli esportati dal pacchetto Buttons.m *)
(*
Restart::usage = "Restart[position_, dice_, gameOver_, seed_, showEuclide_, diceButtonEnabled_] crea il bottone per ricominciare il gioco.";
RollDice::usage = "RollDice[diceButtonEnabled_, diceValue_, num1_, num2_, euclideComponent_, playerPosition_, obstaclesList_, totalBoardCells_, isGameOver_, showEuclide_] crea il bottone per tirare il dado.";
ClearFields::usage = "ClearFields[quotient_, remainder_, message_] restituisce un bottone che ripulisce i campi.";
*)
Get["Board.m"];      (* Pacchetto per la gestione del tabellone di gioco *)
(* Dichiarazione dei simboli esportati dal pacchetto Board.m *)
(*
BoardPrimitives::usage = 
  "BoardPrimitives[cols, rows] genera il tabellone e restituisce \
{primitives, obstacles, totalCells, cols, rows}.";

GetNextPosition::usage = 
  "GetNextPosition[startPosition, diceRoll, obstaclesList, totalCells] \
calcola la nuova posizione tenendo conto degli ostacoli.";

DrawPlayer::usage =
  "DrawPlayer[position, cols] disegna la pedina del giocatore.";
*)
Get["Euclide.m"];    (* Pacchetto per l'implementazione dell'algoritmo di Euclide *)
(* Dichiarazione dei simboli esportati dal pacchetto Euclide.m *)
(*
EuclideComponent::usage = 
  "EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] \
crea un componente per calcolare il GCD usando l'algoritmo di Euclide, mostrando il calcolo passo per passo.";
*)

Get["Dice.m"];       (* Pacchetto per la gestione del dado *)
(* Dichiarazione dei simboli esportati dal pacchetto  Dice.m*)
(*
DrawDice::usage = "DrawDice[value] disegna un dado con il valore specificato.";
*)

(* ResetGame: Resetta lo stato del gioco ai valori iniziali
   Output:
   - Una lista con i valori iniziali: {posizione=1, valore_dado=0, partita_finita=False}
   
   Questa funzione viene chiamata all'inizio del gioco e quando il giocatore
   vuole ricominciare una nuova partita.
*)
ResetGame[] := {1, 0, False};

(* StartGame: Funzione principale che avvia il gioco
   Input:
   - ___ i tre trattini bassi rappresentano un pattern che fa match con zero, uno o più argomenti.
    (corrisponde a qualsiasi parametro opzionale che viene ignorato)
   Output:
   - Crea e visualizza l'interfaccia grafica del gioco
   
   La funzione gestisce l'intera logica del gioco, dalla richiesta del
   seed iniziale alla creazione dell'interfaccia interattiva.
*)


(* Module è un costrutto usato per creare variabili locali e incapsulare il codice. Permette di definire variabili con una visibilità locale
    al corpo del costrutto Module, cioè variabili che non interferisco con altre variabili dichiarate fuori dal modulo con lo stesso nome
    o con gli argomenti della funzione con lo stesso nome. *)
StartGame[___] := Module[
  {seed,                  (* Valore seed fornito dall'utente per la generazione pseudocasuale *)
   boardElements,         (* Elementi grafici del tabellone *)
   obstacles,             (* Lista delle posizioni degli ostacoli *)
   totalCells,            (* Numero totale di celle nel tabellone *)
   columns,               (* Numero di colonne del tabellone *)
   rows,                  (* Numero di righe del tabellone *)
   gameNotebook},         (* Notebook contenente l'interfaccia di gioco *)
  
  (* Richiedi all'utente di inserire un seed attraverso una finestra di dialogo *)
  seed = Module[{seedInput}, DialogInput[
    (* Questa funzione posiziona i suoi argomenti in un layout verticale *)
    Column[{
      "Inserisci il numero seed per il gioco:",
      (* Dynamic[seedInput] serve per fare in modo che il valore inserito nel campo di input sia collegato dinamicamente 
         alla variabile seedInput. Ciò significa che qualsiasi modifica apportata nel campo di input aggiornerà seedInput in tempo reale. *)
      InputField[Dynamic[seedInput], Number],  (* Campo di input per il seed *)
      DefaultButton["OK", DialogReturn[seedInput] ]  (* Pulsante OK che conferma il valore inserito *)
    }, Alignment -> Center],  (* Aggiungo centramento della colonna della finestra di dialogo *)
    WindowTitle -> "Seed del Gioco"  (* Titolo della finestra di dialogo *)
  ] ];
  
  (* Gestione del risultato della finestra di dialogo *)
  If[seed === "cancel" || seed === $Canceled,
    (* L'utente ha annullato l'operazione - non fare nulla *)
    Return[]
  ];
  (* IntegerPart prendi la parte intera se il seed è un numero reale *)
  seed = IntegerPart[seed];
  (* Imposta il generatore di numeri casuali con il seed fornito *)
  SeedRandom[seed];

  {boardElements, obstacles, totalCells, columns, rows} = BoardPrimitives[];
      
  (* Crea il notebook contenente l'interfaccia di gioco *)
  gameNotebook = CreateDocument[
    (* DynamicModule consente di definire variabili locali che mantengono il loro stato,
    abilitando aggiornamenti dinamici e interazioni senza influenzare l'ambiente globale. *)
    DynamicModule[
      {
        boardPrimitives = boardElements,   (* Elementi grafici del tabellone *)
        boardColumns = columns,            (* Numero di colonne *)
        boardRows = rows,                  (* Numero di righe *)
        diceValue = 0,                     (* Valore del dado, inizialmente 0 *)
        playerPosition = 1,                (* Posizione iniziale del giocatore *)
        isGameOver = False,                (* Indica se il gioco è finito *)
        obstaclesList = obstacles,         (* Lista degli ostacoli *)
        totalBoardCells = totalCells,      (* Numero totale di celle *)
        originalSeed = seed,               (* Salva il seed originale per poter ricominciare *)
        num1 = 0,                          (* Primo numero per l'algoritmo di Euclide *)
        num2 = 0,                          (* Secondo numero per l'algoritmo di Euclide *)
        showEuclide = False,               (* Indica se mostrare il componente di Euclide *)
        euclideComponent = Null,           (* Componente per l'algoritmo di Euclide *)
        diceButtonEnabled = True           (* Controlla se il pulsante è abilitato *)
      },
      
      (* Crea l'interfaccia utente *)
      Pane[  (* Aggiungo un Pane per garantire il centramento corretto *)
        Column[{
          (* Titolo del gioco *)
          TextCell[
            "Gioco dell'Anatra con calcolo del MCD",
            "Text",
            FontSize -> 20,
            FontWeight -> "Bold",
            FontColor -> Black,
            TextAlignment -> Center  (* Centra il testo del titolo *)
          ],
          (* Layout orizzontale con tabellone a sinistra e componente di Euclide a destra *)
          Row[{
            (* Colonna sinistra con tabellone e pulsanti di gioco *)
            Column[{
              (* Visualizzazione dinamica del tabellone di gioco *)
              (* Crea la grafica del tabellone e aggiorna automaticamente la grafica quando le sue variabili cambiano. *)
              Dynamic@Graphics[
                Join[
                  boardPrimitives,                              (* Disegna il tabellone *)
                  DrawPlayer[playerPosition, boardColumns] (* Disegna il giocatore nella posizione corrente *)
                ],
                PlotRange -> {{0, boardColumns}, {0, boardRows}}, (* Imposta l'area di visualizzazione *)
                ImageSize -> 400                                  (* Dimensione dell'immagine *)
              ],
              
              (* Pulsante per tirare il dado - CENTRATO *)
              (* Utilizzo di un contenitore Row per centrare il pulsante *)
              Row[{
                (* Utilizzo della funzione RollDice dal pacchetto Buttons *)
                RollDice[
                  diceButtonEnabled, diceValue, num1, num2, euclideComponent, 
                  playerPosition, obstaclesList, totalBoardCells, isGameOver, showEuclide
                ]
              }, Alignment -> Center],  (* Imposta l'allineamento centrale per la riga *)
              
              (* Visualizzazione dinamica dello stato del gioco *)
              (* La funzione Dynamic valuta l'espressione passata come argomento ogni volta che le variabili 
                da cui dipende l'espressione vengono modificate e ritorna il valore dell'espressione, 
                in questo il valore di ritorno viene ingnorato *)
              Dynamic[
                If[isGameOver,
                  (* Se il gioco è finito, mostra il messaggio di vittoria e il pulsante per riavviare *)
                  Column[{
                    Style["Hai vinto!", Bold, 16, TextAlignment -> Center],  (* Stilizzazione del messaggio di vittoria *)
                    Button["Nuova Partita", 
                      (* Reset dello stato del gioco *)
                      {playerPosition, diceValue, isGameOver} = ResetGame[];
                      showEuclide = False;
                    ]
                  }, Alignment -> Center],  (* Centramento della colonna *)
                  (* Altrimenti, mostra il valore dell'ultimo lancio del dado *)
                  Column[{
                    If[diceValue > 0, 
                      DrawDice[diceValue]
                    ]
                  }, Alignment -> Center]  (* Centramento della colonna *)
                ]
              ],
              
              (* Pulsanti di controllo del gioco *)
              (* Pulsanti di controllo del gioco migliorati con sfondi colorati *)
              Row[{
                (* Utilizzo della funzione Restart dal pacchetto Buttons *)
                Restart[
                  playerPosition, diceValue, isGameOver, 
                  originalSeed, showEuclide, diceButtonEnabled
                ],
                
                Spacer[20],
                
                Button[
                  TextCell["Chiudi il gioco", "Text", FontColor -> White],
                  NotebookClose[EvaluationNotebook[] ],
                  Background -> RGBColor[0.8, 0.2, 0.2],
                  FrameMargins -> 10,
                  Appearance -> None,
                  BaseStyle -> {
                    FontSize -> 14,
                    FontColor -> White,
                    FontWeight -> "Bold",
                    FontFamily -> "Arial"
                  },
                  ImageSize -> {120, Automatic},
                  Method -> "Queued",
                  ContentPadding -> 10
                ]
              }, Alignment -> Center]  (* Centramento della riga dei pulsanti *)
            }, Alignment -> Center],  (* Centramento della colonna sinistra *)
            
            Spacer[20], (* Aggiunta di spazio tra il tabellone e il componente di Euclide *)
            
            (* Colonna destra con il componente di Euclide *)
            Dynamic[
              If[showEuclide, 
                euclideComponent,
                (* Mostra un messaggio quando il componente di Euclide non è visibile *)
                Panel[
                  Column[{
                    Style["Tira il dado per cominciare a giocare!", Bold, 14, TextAlignment -> Center]
                  }, Alignment -> Center],  (* Centramento del contenuto del pannello *)
                  ImageMargins -> 10,
                  ImageSize -> {400, 300}
                ]
              ]
            ]
          }, Alignment -> Center]  (* Centramento della riga principale *)
        },
        Alignment -> Center,  (* Allineamento al centro degli elementi nella colonna principale *)
        Spacings -> 10        (* Aumentato lo spazio tra gli elementi per una migliore leggibilità *)
        ],
        ImageSize -> Full,    (* Imposta il Pane per occupare tutta la larghezza disponibile *)
        Alignment -> Center   (* Centra il contenuto del Pane *)
      ]
    ],
    WindowTitle -> "Gioco dell'Anatra con calcolo del MCD",  (* Titolo della finestra del gioco *)
    WindowMargins -> {{Automatic, Automatic}, {Automatic, Automatic}},  (* Centratura automatica della finestra *)
    WindowElements -> {"VerticalScrollBar"}  (* Aggiunge barra di scorrimento verticale se necessario *)
  ];
];

(* Fine della sezione privata del pacchetto *)
End[]

(* Fine della definizione del pacchetto *)
EndPackage[]
