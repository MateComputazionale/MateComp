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
  
  (* 
    La funzione snakeCoordinates converte l'indice (1,...,totalCells) 
    in coordinate {x, y} in modo che:
      - La prima riga (row 1) viene letta da sinistra a destra
      - La seconda riga (row 2) da destra a sinistra, ecc.
    Le coordinate y sono calcolate come (row - 1) e x dipende dal verso della riga.
  *)
  snakeCoordinates[i_] := Module[{row, col, x},
    row = Quotient[i - 1, cols] + 1;
    (* col iniziale come posizione in senso "naturale" *)
    col = Mod[i - 1, cols];
    (* Se la riga è pari, invertiamo l'ordine delle colonne *)
    If[EvenQ[row], x = cols - 1 - col, x = col];
    {x, row - 1}
  ];
  
    boardPrimitives = Table[
      Module[{pos, x, y},
        pos = snakeCoordinates[cell];
        {x, y} = pos;
        {EdgeForm[Black],
        FaceForm[If[isBlocked[cell], Gray, boardColors[[cell]]]],
        Rectangle[{x, y}, {x + 1, y + 1}],
        If[isBlocked[cell],
          Text[Style["X", 14, Bold, Red], {x + 0.5, y + 0.5}],
          (* Se vuoi evidenziare il percorso garantito puoi inserire una logica condizionale,
              per esempio colorando in modo particolare le celle appartenenti a guaranteedPath *)
          Text[Style[ToString[cell], 8], {x + 0.5, y + 0.5}]
        ]
        }
      ],
      {cell, 1, totalCells}
    ];
  
  (* Restituisco le primitive del board, la lista degli ostacoli e il numero totale di celle *)
  {boardPrimitives, obstacles, totalCells, cols, rows}
]

End[]
EndPackage[]
