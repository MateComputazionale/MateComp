(* ::Package:: *)
(* :Title: GCD Euclide - Gioco dell'Oca con Algoritmo di Euclide *)
(* :Context: TODO*)
(* :Author: Gruppo 8: Sara Casadio, Enrico Ferraiolo, Federica Santisi, Luca Orlandello*)
(* :Summary: Questo pacchetto implementa il Gioco dell'Oca con l'Algoritmo di Euclide.
   Il giocatore tira un dado e avanza sul tabellone. In ogni turno dovrà risolvere
   un problema di calcolo del MCD (Massimo Comun Divisore) usando l'algoritmo di Euclide.*)
(* :Copyright: TODO*)
(* :Package Version: 1.0*)
(* :Mathematica Version: TODO*)
(* :History: last modified TODO*)
(* :Keywords: Gioco dell'Oca, Euclide, MCD, GCD, didattica, matematica *)
(* :Sources: TODO*)
(* :Limitations: this is a preliminary version, for educational purposes only.*)
(* :Discussion: TODO*)
(* :Requirements: Richiede i pacchetti Buttons.m, Board.m, Euclide.m e Dice.m *)
(* :Warnings: TODO*)

(* Definizione del pacchetto "IDivisori" *)
BeginPackage["IDivisori`"]

(* Dichiarazione dei simboli esportati dal pacchetto *)
StartGame::usage = "StartGame[] starts the Game of the Goose with Euclid's Algorithm.";

(* Inizio della sezione privata del pacchetto *)
Begin["`Private`"]

(* Importazione dei pacchetti necessari per il funzionamento del gioco *)
Get["Buttons.m"];    (* Pacchetto per la gestione dei pulsanti nell'interfaccia *)
Get["Board.m"];      (* Pacchetto per la gestione del tabellone di gioco *)
Get["Euclide.m"];    (* Pacchetto per l'implementazione dell'algoritmo di Euclide *)
Get["Dice.m"];       (* Pacchetto per la gestione del dado *)

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
    }],
    WindowTitle -> "Seed del Gioco"  (* Titolo della finestra di dialogo *)
  ] ];
  
  (* Gestione del risultato della finestra di dialogo *)
  If[seed === "cancel" || seed === $Canceled,
    (* L'utente ha annullato l'operazione - non fare nulla *)
    Return[]
  ];

  (* Controlla se il seed è un numero valido *)
  If[! NumericQ[seed],
    (* Gestione dell'input non valido *)
      MessageDialog["Il valore inserito non è valido. Inserire un numero."];
      Return[]
  ];
  (* Imposta il generatore di numeri casuali con il seed fornito *)
  SeedRandom[seed];

  (* Board`BoardPrimitives[]; chiama la funzione BoardPrimitives dal pacchetto Board. L'uso del backtick indica che BoardPrimitives 
    è una funzione definita nel pacchetto Board`. Questa riga utilizza un destructuring assignment per estrarre i valori restituiti 
    dalla funzione BoardPrimitives in più variabili. La funzione restituisce una struttura contenente cinque elementi che vengono assegnati 
    rispettivamente boardElements, obstacles, totalCells, columns, rows *)
  {boardElements, obstacles, totalCells, columns, rows} = Board`BoardPrimitives[];
      
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
      Column[{
        (* Titolo del gioco *)
        TextCell[
          "Gioco dell'Anatra con calcolo del MCD",
          "Text",
          FontSize -> 20,
          FontWeight -> "Bold",
          FontColor -> Black
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
                Board`DrawPlayer[playerPosition, boardColumns] (* Disegna il giocatore nella posizione corrente *)
              ],
              PlotRange -> {{0, boardColumns}, {0, boardRows}}, (* Imposta l'area di visualizzazione *)
              ImageSize -> 400                                  (* Dimensione dell'immagine *)
            ],
            
            (* Pulsante per tirare il dado - CENTRATO *)
            (* Utilizzo di un contenitore Row per centrare il pulsante *)
            Row[{
              Spacer[125],  (* Aggiunge spazio a sinistra per centrare il pulsante *)
              (* Utilizzo della funzione RollDice dal pacchetto Buttons *)
              Buttons`RollDice[
                diceButtonEnabled, diceValue, num1, num2, euclideComponent, 
                playerPosition, obstaclesList, totalBoardCells, isGameOver, showEuclide
              ],
              Spacer[125]  (* Aggiunge spazio a destra per mantenere simmetria *)
            }, Alignment -> Center],  (* Imposta l'allineamento centrale per la riga *)
            
            (* Visualizzazione dinamica dello stato del gioco *)
            (* La funzione Dynamic valuta l'espressione passata come argomento ogni volta che le variabili 
              da cui dipende l'espressione vengono modificate e ritorna il valore dell'espressione, 
              in questo il valore di ritorno viene ingnorato *)
            Dynamic[
              If[isGameOver,
                (* Se il gioco è finito, mostra il messaggio di vittoria e il pulsante per riavviare *)
                Column[{
                  "Hai vinto!",
                  Button["Nuova Partita", 
                    (* Reset dello stato del gioco *)
                    {playerPosition, diceValue, isGameOver} = ResetGame[];
                    showEuclide = False;
                  ]
                }],
                (* Altrimenti, mostra il valore dell'ultimo lancio del dado *)
                Column[{
                  If[diceValue > 0, 
                    DrawDice[diceValue]
                  ]
                }]
              ]
            ],
            
            (* Pulsanti di controllo del gioco *)
            (* Pulsanti di controllo del gioco migliorati con sfondi colorati *)
            Row[{
            (* Utilizzo della funzione Restart dal pacchetto Buttons *)
            Buttons`Restart[
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
              ContentPadding -> 10,
              RoundingRadius -> 8,
              BoxShadow -> {0, 2, 4, GrayLevel[0.5]}
            ]
          }]
          }],
          
          Spacer[20], (* Aggiunta di spazio tra il tabellone e il componente di Euclide *)
          
          (* Colonna destra con il componente di Euclide *)
          Dynamic[
            If[showEuclide, 
              euclideComponent,
              (* Mostra un messaggio quando il componente di Euclide non è visibile *)
              Panel[
                Column[{
                  Style["Tira il dado per cominciare a giocare!", Bold, 14]
                }],
                ImageMargins -> 10,
                ImageSize -> {400, 300}
              ]
            ]
          ]
        }]
      },
      Alignment -> Center,  (* Allineamento al centro degli elementi *)
      Spacings -> 2         (* Spaziatura tra gli elementi *)
      ]
    ],
    WindowTitle -> "Gioco dell'Anatra con calcolo del MCD"  (* Titolo della finestra del gioco *)
  ];
];

(* Fine della sezione privata del pacchetto *)
End[]

(* Fine della definizione del pacchetto *)
EndPackage[]