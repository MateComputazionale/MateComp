(* ::Package:: *)

BeginPackage["Board`"]

BoardPrimitives::usage = 
  "BoardPrimitives[cols, rows, obstaclesPercent] genera la griglia del board. \
Restituisce una lista {boardPrimitives, obstacles, totalCells, cols, rows}.";
getNextPosition::usage = 
  "getNextPosition[posizioneIniziale, tiro, listaOstacoli, totalCells, cols] calcola la nuova posizione \
  in base alla posizione iniziale, al tiro e alla lista di ostacoli. Restituisce la nuova posizione.";

Begin["Private`"]

(* Funzione iterativa per contare ostacoli superati *)
ContaOstacoliSuperati[posizioneIniziale_, tiro_, listaOstacoli_] := Module[
  {total = 0, currentStart, currentEnd, count},
  currentStart = posizioneIniziale + 1;
  currentEnd = posizioneIniziale + tiro;
  While[True,
    count = Count[listaOstacoli, _?(Between[{currentStart, currentEnd}])];
    If[count == 0, Break[]];
    total += count;
    currentStart = currentEnd + 1;
    currentEnd += count;
  ];
  total
];

(* Funzione per calcolare la nuova posizione con controllo dei limiti *)
getNextPosition[posizioneIniziale_, tiro_, listaOstacoli_, totalCells_, cols_] := Module[
  {ostacoliSuperati, nuovaPosizione, maxPosition},
  ostacoliSuperati = ContaOstacoliSuperati[posizioneIniziale, tiro, listaOstacoli];
  nuovaPosizione = posizioneIniziale + tiro + ostacoliSuperati;
  maxPosition = Min[nuovaPosizione, totalCells];
  maxPosition 
];

(* Funzione per convertire l'indice in coordinate a serpentina *)
snakeCoordinates[position_, cols_] := Module[
  {row, col},
  row = Quotient[position - 1, cols];
  col = Mod[position - 1, cols];
  (* If row is odd, we need to reverse the column order for snake pattern *)
  If[OddQ[row],
    col = cols - 1 - col
  ];
  (* Return center coordinates of the cell *)
  {col, row}
]

(* Genera il percorso garantito senza ostacoli *)
generatePath[cols_, rows_] := Module[
  {moves, start = {1, 1}, current = {1, 1}, path, newPos},
  moves = Join[Table["R", {cols - 1}], Table["U", {rows - 1}]];
  moves = RandomSample[moves];
  path = {current};
  Do[
    newPos = Switch[move,
      "R", {current[[1]] + 1, current[[2]]},
      "U", {current[[1]], current[[2]] + 1}
    ];
    AppendTo[path, newPos];
    current = newPos,
    {move, moves}
  ];
  Map[(#[[2]] - 1)*cols + #[[1]] &, path]
];

(* Genera la griglia con ostacoli *)
BoardPrimitives[cols_Integer:6, rows_Integer:6, obstaclesPercent_:0.35] := Module[
  {totalCells, boardColors, numObstacles, obstacles, boardPrimitives, guaranteedPath, availableCells},
  totalCells = cols * rows;
  guaranteedPath = generatePath[cols, rows];
  availableCells = Complement[Range[2, totalCells - 1], guaranteedPath];
  numObstacles = Round[Length[availableCells] * obstaclesPercent];
  obstacles = RandomSample[availableCells, numObstacles];
  
  boardPrimitives = Table[
    Module[{pos = snakeCoordinates[cell, cols], x, y},
      {x, y} = pos;
      {
        EdgeForm[Black],
        FaceForm[If[MemberQ[obstacles, cell], Gray, ColorData["Rainbow"][cell/totalCells]]],
        Rectangle[{x, y}, {x + 1, y + 1}],
        Text[Style[If[MemberQ[obstacles, cell], "", ToString[cell]], 8], {x + 0.5, y + 0.5}]
      }
    ],
    {cell, 1, totalCells}
  ];
  
  {boardPrimitives, obstacles, totalCells, cols, rows}
];

End[]
EndPackage[]