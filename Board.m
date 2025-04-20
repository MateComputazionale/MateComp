(* ::Package:: *)

BeginPackage["Board`"]

BoardPrimitives::usage = 
  "BoardPrimitives[cols, rows, obstaclesPercent] generates the game board grid. \
Returns {boardPrimitives, obstacles, totalCells, cols, rows}.";

GetNextPosition::usage = 
  "GetNextPosition[startPosition, diceRoll, obstaclesList, totalCells, cols] calculates the new position \
based on initial position, dice roll and list of obstacles. Returns the new position.";

SnakeCoordinates::usage =
  "SnakeCoordinates[position, cols] converts linear position to snake pattern coordinates.";

Begin["`Private`"]

(* Calculate how many obstacles are encountered in a move *)
CountObstaclesPassed[startPosition_, diceRoll_, obstaclesList_] := Module[
  {totalObstacles = 0, currentStart, currentEnd, obstaclesInRange},
  
  currentStart = startPosition + 1;
  currentEnd = startPosition + diceRoll;
  
  While[True,
    obstaclesInRange = Count[obstaclesList, _?(Between[{currentStart, currentEnd}])];
    
    If[obstaclesInRange == 0, Break[]];
    
    totalObstacles += obstaclesInRange;
    currentStart = currentEnd + 1;
    currentEnd += obstaclesInRange;
  ];
  
  totalObstacles
];

(* Calculate new position with bounds checking *)
GetNextPosition[startPosition_, diceRoll_, obstaclesList_, totalCells_, cols_] := Module[
  {obstaclesPassed, newPosition},
  
  obstaclesPassed = CountObstaclesPassed[startPosition, diceRoll, obstaclesList];
  newPosition = startPosition + diceRoll + obstaclesPassed;
  
  (* Don't exceed board limits *)
  Min[newPosition, totalCells]
];

(* Convert linear position to snake pattern coordinates *)
SnakeCoordinates[position_, cols_] := Module[
  {row, col},
  
  row = Quotient[position - 1, cols];
  col = Mod[position - 1, cols];
  
  (* Reverse column order for odd rows (snake pattern) *)
  If[OddQ[row],
    col = cols - 1 - col
  ];
  
  {col, row}
];

(* Randomly select obstacle positions based on percentage *)
PlaceObstacles[cols_Integer, rows_Integer, obstaclesPercent_:0.35] := Module[
  {totalCells, numObstacles, allCells},
  totalCells = cols * rows;
  allCells = Range[2, totalCells];
  numObstacles = Round[totalCells * obstaclesPercent];
  RandomSample[allCells, numObstacles]
];

(* Generate board primitives with obstacles marked *)
BoardPrimitives[cols_Integer:6, rows_Integer:6, obstaclesPercent_:0.15] := Module[
  {totalCells, obstacles, primitives},
  totalCells = cols * rows;
  obstacles = PlaceObstacles[cols, rows, obstaclesPercent];
  primitives = Table[
    Module[{coord = SnakeCoordinates[cell, cols], x, y},
      {x, y} = coord;
      {
        EdgeForm[Black],
        FaceForm[If[MemberQ[obstacles, cell], Gray, ColorData["Rainbow"][cell/totalCells]]],
        Rectangle[{x, y}, {x + 1, y + 1}],
        Text[Style[If[MemberQ[obstacles, cell], "", ToString[cell]], 8], {x + 0.5, y + 0.5}]
      }
    ],
    {cell, 1, totalCells}
  ];
  {primitives, obstacles, totalCells, cols, rows}
];

End[]
EndPackage[]