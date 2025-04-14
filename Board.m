(* ::Package:: *)
(* Board.m *)
BeginPackage["Board`"]

BoardPrimitives::usage = 
  "BoardPrimitives[cols, rows, obstaclesPercent] genera la griglia del board. \
Restituisce una lista {boardPrimitives, obstacles, totalCells}.";

Begin["`Private`"]

(* Funzione per generare un percorso casuale dalla cella 1 alla cella finale, attraverso mosse U/R *)
Clear[generatePath];
generatePath[cols_Integer, rows_Integer] := Module[
  {moves, start = {1, 1}, current = {1, 1}, path, newPos},
  (* In totale sono necessarie (cols-1) mosse a destra e (rows-1) mosse in alto *)
  moves = Join[Table["R", {cols - 1}], Table["U", {rows - 1}]];
  moves = RandomSample[moves]; (* mescola le mosse per variare il percorso *)
  path = {current};
  Do[
    newPos = current;
    Switch[move,
      "R", newPos = {current[[1]] + 1, current[[2]]},
      "U", newPos = {current[[1]], current[[2]] + 1}
    ];
    AppendTo[path, newPos];
    current = newPos;
    ,
    {move, moves}
  ];
  (* Convertiamo le coordinate (col, row) in indice di cella, dove l'indice si calcola come (row-1)*cols + col *)
  Map[ (#[[2]] - 1)*cols + #[[1]] &, path ]
];


BoardPrimitives[cols_Integer: 6, rows_Integer: 6, obstaclesPercent_: 0.35] := Module[
  {totalCells, boardColors, numObstacles, obstacles, isBlocked, boardPrimitives, guaranteedPath, availableCells},
  
  totalCells = cols * rows;
  guaranteedPath = generatePath[cols, rows]; (* Genero il percorso garantito *)
  availableCells = Complement[Range[2, totalCells - 1], guaranteedPath]; (* Celle disponibili *)
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
