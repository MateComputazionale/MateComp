(* ::Package:: *)
(* Board.m *)
BeginPackage["Board`"]

BoardPrimitives::usage = 
  "BoardPrimitives[cols, rows, obstaclesPercent] genera la griglia del board. \
Restituisce una lista {boardPrimitives, obstacles, totalCells}.";

Begin["`Private`"]

BoardPrimitives[cols_Integer, rows_Integer, obstaclesPercent_: 0.15] := Module[
  {totalCells, boardColors, numObstacles, obstacles, isBlocked, boardPrimitives},
  
  totalCells = cols * rows;
  boardColors = ColorData["Rainbow"] /@ Rescale[Range[totalCells]];
  numObstacles = Round[totalCells * obstaclesPercent];
  obstacles = RandomSample[Range[2, totalCells - 1], numObstacles];
  
  (* Funzione interna per verificare se una casella è bloccata *)
  isBlocked[pos_] := MemberQ[obstacles, pos];
  
  boardPrimitives = Flatten[
    Table[
      Module[{i, col, row, x, y},
        i = cell;
        row = Quotient[i - 1, cols] + 1;
        col = Mod[i - 1, cols] + 1;
        x = col - 1;
        y = row - 1;
        {EdgeForm[Black], 
         FaceForm[If[isBlocked[i], Gray, boardColors[[i]]]], 
         Rectangle[{x, y}, {x + 1, y + 1}], 
         If[isBlocked[i],
           Text[Style["X", 14, Bold, Red], {x + 0.5, y + 0.5}],
           Text[Style[ToString[i], 8], {x + 0.5, y + 0.5}]
         ]
        }
      ],
      {cell, 1, totalCells}
    ],
    1
  ];
  
  (* Restituisco le primitive del board, la lista degli ostacoli e il numero totale di celle *)
  {boardPrimitives, obstacles, totalCells}
]

End[]
EndPackage[]
