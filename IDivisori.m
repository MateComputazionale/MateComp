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

StartGame::usage = "StartGame[] starts the Game of the Goose with Euclid's Algorithm.";

Begin["`Private`"]

(* Import required packages *)
Get["Buttons.m"];
Get["Board.m"];
Get["Euclide.m"];

(* Reset game state *)
ResetGame[] := {1, 0, False};

(* Main game function *)
StartGame[___] := Module[
  {seed, boardElements, obstacles, totalCells, columns, rows},
  
  (* Get seed from user *)
  seed = DialogInput[
    Column[{
      "Inserisci il numero seed per il gioco:",
      InputField[Dynamic[seedInput], Number],
      DefaultButton[DialogReturn[seedInput] ]
    }],
    WindowTitle -> "Seed del Gioco"
  ];
  
  If[NumericQ[seed],
    (* Configure game with valid seed *)
    SeedRandom[seed];
    {boardElements, obstacles, totalCells, columns, rows} = Board`BoardPrimitives[];
    
    CreateDocument[
      DynamicModule[
        {
          boardPrimitives = boardElements, 
          boardColumns = columns, 
          boardRows = rows, 
          diceValue = 0, 
          playerPosition = 1, 
          isGameOver = False, 
          obstaclesList = obstacles, 
          totalBoardCells = totalCells
        },
        
        Column[{
          Style["Gioco dell'Oca con Algoritmo di Euclide", Bold, 16],
          
          Dynamic@Graphics[
            Join[
              boardPrimitives,
              Board`DrawPlayer[playerPosition, boardColumns]
            ],
            PlotRange -> {{0, boardColumns}, {0, boardRows}},
            ImageSize -> 400
          ],
          
          Button["Tira il dado", 
            diceValue = RandomInteger[{1, 6}];
            
            Module[{num1, num2},
              num1 = RandomInteger[{10, 99}];
              num2 = RandomInteger[{1, num1 - 1}];
              
              Euclide`EuclideDialog[num1, num2, diceValue, 
                Function[gcdResult, 
                  Module[{newPosition},
                    newPosition = Board`GetNextPosition[
                      playerPosition, diceValue, obstaclesList, totalBoardCells, boardColumns
                    ];
                    playerPosition = newPosition;
                    If[playerPosition >= totalBoardCells, isGameOver = True];
                  ]
                ]
              ];
            ],
            Enabled -> Dynamic[!isGameOver]
          ],
          
          Dynamic[
            If[isGameOver,
              Column[{
                "Hai vinto!",
                Button["Nuova Partita", 
                  {playerPosition, diceValue, isGameOver} = ResetGame[]
                ]
              }],
              "Ultimo lancio: " <> ToString[diceValue]
            ]
          ]
        },
        Alignment -> Center,
        Spacings -> 2
        ]
      ],
      WindowTitle -> "Gioco dell'Oca con Algoritmo di Euclide"
    ],
    
    (* Handle invalid input *)
    MessageDialog["Il valore inserito non è valido. Inserire un numero."]
  ];
];

End[]
EndPackage[]
