(* ::Package:: *)
(* :Title: GCD Euclide*)
(* :Context: TODO*)
(* :Author: Gruppo 8: Sara Casadio, Enrico Ferraiolo, Federica Santisi, Luca Orlandello*)
(* :Summary: TODO*)
(* :Copyright: TODO*)
(* :Package Version: 1.0*)
(* :Mathematica Version: TODO*)
(* :History: last modified TODO*)
(* :Keywords: TODO*)
(* :Sources: TODO*)
(* :Limitations: this is a preliminary version, for educational purposes only.*)
(* :Discussion: TODO*)
(* :Requirements: TODO*)
(* :Warnings: TODO*)

BeginPackage["IDivisori`"]

StartGame::usage = "StartGame[] avvia il Gioco dell'Oca con l'Algoritmo di Euclide.";

Begin["`Private`"]

(* Includo i package necessari *)
Get["Buttons.m"];
Get["Board.m"];
Get["Euclide.m"];

(* Funzione per resettare il gioco *)
Restart[position_, dice_, gameOver_] := (
  position = 1;
  dice = 0;
  gameOver = False;
);

StartGame[___] := Module[
  {seed, finalPos, obstacles, cols, rows, boardPrimitives, totalCells},
  
  (* 1. Prompt per il seed *)
  seed = DialogInput[
    Column[{
      "Inserisci il numero seed per il gioco:",
      InputField[Dynamic[seedInput], Number],
      DefaultButton[DialogReturn[seedInput]]
    }],
    WindowTitle -> "Seed del Gioco"
  ];
  
  If[NumericQ[seed],
    (* Se il seed è numerico, impostalo e configura il tabellone *)
    SeedRandom[seed];
    (* Chiamata alla funzione che genera la board *)
    {boardPrimitives, obstacles, totalCells, cols, rows} = Board`BoardPrimitives[];
    finalPos = totalCells;
    
    (* 2. Creazione della finestra del gioco *)
    CreateDocument[
      DynamicModule[
      {boardPrims = boardPrimitives, nCols = cols, nRows = rows, dice = 0, position = 1, gameOver = False, ostacoli = obstacles, totalCellsDM = totalCells},
        Column[{
          Style["Gioco dell'Oca con Algoritmo di Euclide", Bold, 16],
          
          (* Tabellone di gioco *)
          Dynamic@Graphics[
            Join[
              boardPrims,
              {Red, Disk[Board`snakeCoordinates[position, nCols] + {0.5,0.5}, 0.25]}
            ],
            PlotRange -> {{0, nCols}, {0, nRows}},
            ImageSize -> 400
          ],
          
          (* Pulsante per tirare il dado *)
          (* Pulsante per tirare il dado *)
          Button["Tira il dado", 
            dice = RandomInteger[{1, 6}];
            Module[{a, b},
              a = RandomInteger[{10, 99}];
              b = RandomInteger[{1, a - 1}];
              Euclide`EuclideDialog[a, b, dice, 
                Function[result, 
                  Module[{newPos},
                    newPos = Board`getNextPosition[position, dice, ostacoli, totalCellsDM, nCols];
                    position = newPos;
                    If[position >= totalCells, gameOver = True];
                  ]
                ]
              ];
            ],
            Enabled -> Dynamic[!gameOver]
          ],
          
          (* Informazioni di gioco *)
          Dynamic[
            If[gameOver,
              Column[{
                "Hai vinto!",
                Button["Nuova Partita", Restart[position, dice, gameOver]]
              }],
              "Ultimo lancio: " <> ToString[dice]
            ]
          ]
        },
        Alignment -> Center,
        Spacings -> 2
        ]
      ],
      WindowTitle -> "Gioco dell'Oca con Algoritmo di Euclide"
    ],
    
    (* Se il valore inserito non è numerico *)
    MessageDialog["Il valore inserito non è valido. Inserire un numero."]
  ]
];

End[]

EndPackage[]