(* ::Package:: *)
(* Board.m *)
BeginPackage["Board`"]

BoardPrimitives::usage = 
  "BoardPrimitives[cols, rows, obstaclesPercent] genera la griglia del board. \
Restituisce una lista {boardPrimitives, obstacles, totalCells}.";

Begin["`Private`"]

BoardPrimitives[cols_Integer: 3, rows_Integer: 3, obstaclesPercent_: 0.15] := Module[
  {totalCells, boardColors, numObstacles, obstacles, isBlocked, boardPrimitives},
  
  totalCells = cols * rows;
  boardColors = ColorData["Rainbow"] /@ Rescale[Range[totalCells]]; (* Colori celle *)
  numObstacles = Round[totalCells * obstaclesPercent];
  obstacles = RandomSample[Range[2, totalCells - 1], numObstacles]; 
  
  (* Funzione interna per verificare se una casella è bloccata *)
  isBlocked[pos_] := MemberQ[obstacles, pos]; (* Restituisco True se la cella è bloccata *)
  
  (* Genero le primitive del board *)
  
  boardPrimitives = Flatten[
    Table[
      Module[{i, col, row, x, y},
        i = cell;
        row = Quotient[i - 1, cols] + 1;
        col = Mod[i - 1, cols] + 1;
        x = col - 1;
        y = row - 1;
        {EdgeForm[Black], 
         FaceForm[If[isBlocked[i], Gray, boardColors[[i]]]], (* Colore cella, grigia se è un ostacolo *)
         Rectangle[{x, y}, {x + 1, y + 1}], (* Disegno cella *)
         If[isBlocked[i],
           Text[Style["X", 14, Bold, Red], {x + 0.5, y + 0.5}], (* Ostacolo *)
           Text[Style[ToString[i], 8], {x + 0.5, y + 0.5}] (* Cella senza ostacolo *)
         ]
        }
      ],
      {cell, 1, totalCells}
    ],
    1
  ];
  
  (* Restituisco le primitive del board, la lista degli ostacoli e il numero totale di celle *)
  {boardPrimitives, obstacles, totalCells, cols, rows}
]

End[]
EndPackage[]
