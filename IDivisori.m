(* ::Package:: *)

(* :Title: GCD Euclide*)
(* :Context: TODO*)
(* :Author: Gruppo 8: Sara Casadio, Enrico Ferraiolo, \
Federica Santisi, Luca Orlandello*)
(* :Summary: TODO*)
(* :Copyright: TODO*)
(* :Package Version: 1.0*)
(* :Mathematica Version: TODO*)
(* :History: last modified TODO*)
(* :Keywords: TODO*)
(* :Sources: TODO*)
(* :Limitations: \
this is a preliminary version,for educational purposes only.*)
(* :Discussion: TODO*)
(* :Requirements: TODO*)
(* :Warnings: TODO*)



BeginPackage["IDivisori`"]

StartGame::usage = 
  "StartGame[] avvia il Gioco dell'Oca con l'Algoritmo di Euclide.";

Begin["`Private`"]

(* Includo il package dei bottoni e quello del board *)
Get["Buttons.m"];
Get["Board.m"];
Get["Euclide.m"];


StartGame[___] := Module[
  {seed, finalPos, obstacles},
  
  (* 1. Prompt per il seed *)
  seed = DialogInput[
    Column[{"Inserisci il numero seed per il gioco:", 
      InputField[Dynamic[seedInput], Number], 
      DefaultButton[DialogReturn[seedInput]]}],
    WindowTitle -> "Seed del Gioco"];
  
  If[NumericQ[seed],
    (* Se il seed \[EGrave] numerico, impostalo e configura il tabellone *)
    SeedRandom[seed];
    
    (* Chiamata alla funzione che genera la board *)
    {boardPrimitives, obstacles, totalCells, cols, rows} = Board`BoardPrimitives[];
    finalPos = totalCells;

    (* 2. Creazione della finestra del gioco *)
    CreateDocument[
      DynamicModule[
        {dice = 0, position = 1, gameOver = False, ostacoli = obstacles},
        Column[{
          Style["Gioco dell'Oca", Bold, 16],
          Dynamic@Graphics[
            Join[
              boardPrimitives,
              If[position <= totalCells,
                { Red,
                  Disk[
                    { Mod[position - 1, cols] + 0.5,
                      Quotient[position - 1, cols] + 0.5
                    },
                    0.3
                  ]
                },
                {}
              ]
            ],
            PlotRange -> {{0, cols}, {0, rows}},
            ImageSize  -> 400
          ],

          Restart[position, dice, gameOver]


            Button["Tira il dado",
            dice = RandomInteger[{1, 6}];
            Module[{a, b},
              a = RandomInteger[{10, 99}];
              b = RandomInteger[{1, a - 1}];
              
              Euclide`EuclideDialog[a, b, dice,
                Function[Null,
                  Module[{ostacoliSuperati},
                    position = Board`getNextPosition[position, dice, ostacoli];
                  ]
                ]
              ];

            ],
            Enabled -> Dynamic[! gameOver]
          ]

          Dynamic[If[gameOver, "Hai vinto!",
            "Ultimo lancio: " <> ToString[dice]]],
          Dynamic[If[gameOver,
          Restart[position, dice, gameOver]
            Button["Nuova Partita", position = 1; gameOver = False;],
            ""
          ]]
        },
        Alignment -> Center,
        Spacings -> 2
        ]
      ],
      WindowTitle -> "Gioco dell'Oca"
    ],
    (* Se il valore inserito non \[EGrave] numerico *)
    Print["Il valore inserito non \[EGrave] valido."]
  ];
];

End[]
EndPackage[]
