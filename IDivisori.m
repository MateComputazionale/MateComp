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
ResetGame[OptionsPattern[]] := Module[
  {position = 1, dice = 0, gameOver = False},
  {position, dice, gameOver}
];

(* Main game function *)
StartGame[___] := Module[
  {seed, finalPosition, obstacles, columns, rows, boardElements, totalCells},
  
  (* Get seed from user *)
  seed = DialogInput[
    Column[{
      "Inserisci il numero seed per il gioco:",
      InputField[Dynamic[seedInput], Number],
      DefaultButton[DialogReturn[seedInput]]
    }],
    WindowTitle -> "Seed del Gioco"
  ];
  
  If[NumericQ[seed],
    (* Configure game with valid seed *)
    SeedRandom[seed];
    
    (* Generate board *)
    {boardElements, obstacles, totalCells, columns, rows} = Board`BoardPrimitives[];
    finalPosition = totalCells;
    
    (* Create game interface *)
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
          (* Game title *)
          Style["Gioco dell'Oca con Algoritmo di Euclide", Bold, 16],
          
          (* Game board visualization *)
          Dynamic@Graphics[
            Join[
              boardPrimitives,
              Board`DrawPlayer[playerPosition, boardColumns]
            ],
            PlotRange -> {{0, boardColumns}, {0, boardRows}},
            ImageSize -> 400
          ],
          (* Game controls *)
          Button["Tira il dado", 
            (* Roll dice and trigger Euclid's algorithm dialog *)
            diceValue = RandomInteger[{1, 6}];
            
            Module[{num1, num2},
              (* Generate random numbers for GCD calculation *)
              num1 = RandomInteger[{10, 99}];
              num2 = RandomInteger[{1, num1 - 1}];
              
              (* Launch Euclid's algorithm dialog *)
              Euclide`EuclideDialog[num1, num2, diceValue, 
                (* Callback function when algorithm is completed *)
                Function[gcdResult, 
                  Module[{newPosition},
                    (* Calculate new position *)
                    newPosition = Board`GetNextPosition[
                      playerPosition, diceValue, obstaclesList, totalBoardCells, boardColumns
                    ];
                    
                    (* Update player position *)
                    playerPosition = newPosition;
                    
                    (* Check for game end *)
                    If[playerPosition >= totalBoardCells, isGameOver = True];
                  ]
                ]
              ];
            ],
            Enabled -> Dynamic[!isGameOver]
          ],
          
          (* Game status information *)
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
    
    (* Handle invalid seed input *)
    MessageDialog["Il valore inserito non è valido. Inserire un numero."]
  ]
];

End[]
EndPackage[]