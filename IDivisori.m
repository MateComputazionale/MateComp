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
(* :Requirements: Richiede i pacchetti Buttons.m, Board.m e Euclide.m *)
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

(* ResetGame: Resetta lo stato del gioco ai valori iniziali
   Output:
   - Una lista con i valori iniziali: {posizione=1, valore_dado=0, partita_finita=False}
   
   Questa funzione viene chiamata all'inizio del gioco e quando il giocatore
   vuole ricominciare una nuova partita.
*)
ResetGame[] := {1, 0, False};

(* StartGame: Funzione principale che avvia il gioco
   Input:
   - ___ (corrisponde a qualsiasi parametro opzionale che viene ignorato)
   Output:
   - Crea e visualizza l'interfaccia grafica del gioco
   
   La funzione gestisce l'intera logica del gioco, dalla richiesta del
   seed iniziale alla creazione dell'interfaccia interattiva.
*)
StartGame[___] := Module[
  {seed,                  (* Valore seed fornito dall'utente per la generazione pseudocasuale *)
   boardElements,         (* Elementi grafici del tabellone *)
   obstacles,             (* Lista delle posizioni degli ostacoli *)
   totalCells,            (* Numero totale di celle nel tabellone *)
   columns,               (* Numero di colonne del tabellone *)
   rows,                  (* Numero di righe del tabellone *)
   gameNotebook},         (* Notebook contenente l'interfaccia di gioco *)
  
  (* Richiedi all'utente di inserire un seed attraverso una finestra di dialogo *)
  seed = DialogInput[
    Column[{
      "Inserisci il numero seed per il gioco:",
      InputField[Dynamic[seedInput], Number],  (* Campo di input per il seed *)
      Row[{
        DefaultButton["OK", DialogReturn[seedInput]]  (* Pulsante OK che conferma il valore inserito *)
      }]
    }],
    WindowTitle -> "Seed del Gioco"  (* Titolo della finestra di dialogo *)
  ];
  
  (* Gestione del risultato della finestra di dialogo *)
  If[seed === "cancel" || seed === $Canceled,
    (* L'utente ha annullato l'operazione - non fare nulla *)
    Return[],
    
    (* Controlla se il seed è un numero valido *)
    If[NumericQ[seed],
      (* Configura il gioco con il seed valido *)
      SeedRandom[seed];  (* Imposta il generatore di numeri casuali con il seed fornito *)
      (* Genera il tabellone e ottieni gli elementi necessari *)
      {boardElements, obstacles, totalCells, columns, rows} = Board`BoardPrimitives[];
      
      (* Crea il notebook contenente l'interfaccia di gioco *)
      gameNotebook = CreateDocument[
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
            originalSeed = seed                (* Salva il seed originale per poter ricominciare *)
          },
          
          (* Crea l'interfaccia utente *)
          Column[{
            (* Titolo del gioco *)
            Style["Gioco dell'Oca con Algoritmo di Euclide", Bold, 16],
            
            (* Visualizzazione dinamica del tabellone di gioco *)
            Dynamic@Graphics[
              Join[
                boardPrimitives,                              (* Disegna il tabellone *)
                Board`DrawPlayer[playerPosition, boardColumns] (* Disegna il giocatore nella posizione corrente *)
              ],
              PlotRange -> {{0, boardColumns}, {0, boardRows}}, (* Imposta l'area di visualizzazione *)
              ImageSize -> 400                                  (* Dimensione dell'immagine *)
            ],
            
            (* Pulsante per tirare il dado *)
            Button["Tira il dado", 
              (* Genera un numero casuale da 1 a 6 *)
              diceValue = RandomInteger[{1, 6}];
              
              (* Genera due numeri casuali per il calcolo del MCD *)
              Module[{num1, num2},
                num1 = RandomInteger[{10, 99}];          (* Genera un numero tra 10 e 99 *)
                num2 = RandomInteger[{1, num1 - 1}];     (* Genera un numero minore di num1 *)
                
                (* Apre la finestra di dialogo per l'algoritmo di Euclide *)
                Euclide`EuclideDialog[num1, num2, diceValue, 
                  (* Callback che viene chiamata quando l'utente completa l'algoritmo *)
                  Function[gcdResult, 
                    Module[{newPosition},
                      (* Calcola la nuova posizione considerando gli ostacoli *)
                      newPosition = Board`GetNextPosition[
                        playerPosition, diceValue, obstaclesList, totalBoardCells
                      ];
                      (* Aggiorna la posizione del giocatore *)
                      playerPosition = newPosition;
                      (* Controlla se il giocatore ha raggiunto o superato l'ultima cella *)
                      If[playerPosition >= totalBoardCells, isGameOver = True];
                    ]
                  ]
                ];
              ],
              (* Disabilita il pulsante se il gioco è finito *)
              Enabled -> Dynamic[!isGameOver]
            ],
            
            (* Visualizzazione dinamica dello stato del gioco *)
            Dynamic[
              If[isGameOver,
                (* Se il gioco è finito, mostra il messaggio di vittoria e il pulsante per riavviare *)
                Column[{
                  "Hai vinto!",
                  Button["Nuova Partita", 
                    (* Reset dello stato del gioco *)
                    {playerPosition, diceValue, isGameOver} = ResetGame[]
                  ]
                }],
                (* Altrimenti, mostra il valore dell'ultimo lancio del dado *)
                "Ultimo lancio: " <> ToString[diceValue]
              ]
            ],
            
            (* Pulsanti di controllo del gioco *)
            Row[{
              Button["Ricomincia da capo", 
                (* Reset del gioco mantenendo lo stesso seed *)
                SeedRandom[originalSeed];
                {playerPosition, diceValue, isGameOver} = ResetGame[]
              ],
              Spacer[20],  (* Spaziatore tra i pulsanti *)
              Button["Chiudi schermata", 
                (* Chiudi il notebook corrente *)
                NotebookClose[EvaluationNotebook[]]
              ]
            }]
          },
          Alignment -> Center,  (* Allineamento al centro degli elementi *)
          Spacings -> 2         (* Spaziatura tra gli elementi *)
          ]
        ],
        WindowTitle -> "Gioco dell'Oca con Algoritmo di Euclide"  (* Titolo della finestra del gioco *)
      ],
      
      (* Gestione dell'input non valido *)
      MessageDialog["Il valore inserito non è valido. Inserire un numero."]
    ]
  ];
];

(* Fine della sezione privata del pacchetto *)
End[]

(* Fine della definizione del pacchetto *)
EndPackage[]