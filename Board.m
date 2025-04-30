(* ::Package:: *)

BeginPackage["Board`"]

BoardPrimitives::usage = 
  "BoardPrimitives[cols, rows] genera il tabellone e restituisce \
{primitives, obstacles, totalCells, cols, rows}.";

GetNextPosition::usage = 
  "GetNextPosition[startPosition, diceRoll, obstaclesList, totalCells] \
calcola la nuova posizione tenendo conto degli ostacoli.";

DrawPlayer::usage =
  "DrawPlayer[position, cols] disegna la pedina del giocatore.";

Begin["`Private`"]

(* Conta quanti ostacoli si incontrano in un movimento *)
CountObstaclesPassed[start_, roll_, obs_List] := Module[
  {passed = 0, a = start + 1, b = start + roll, inRange},
  While[True,
    inRange = Count[obs, _?(Between[{a, b}])];
    If[inRange == 0, Break[]];
    passed += inRange;
    a = b + 1;
    b += inRange;
  ];
  passed
];

(* Calcola la nuova posizione e non supera totalCells *)
GetNextPosition[start_, roll_, obs_List, totalCells_] := Module[
  {extra, newPos},
  extra = CountObstaclesPassed[start, roll, obs];
  newPos = start + roll + extra;
  Min[newPos, totalCells]
];

(* Costruisce un percorso pseudocasuale dalla 1 a cols*rows *)
GeneratePath[cols_, rows_] := Module[
  {total = cols*rows, cur = 1, path = {1}, moves, nxt},
  While[cur < total,
    moves = Select[{cur + 1, cur + cols}, # <= total &];
    nxt = RandomChoice[moves];
    AppendTo[path, nxt];
    cur = nxt;
  ];
  DeleteDuplicates[path]
];

(* Mapping semplice linea -> coordinate (x,y) su griglia *)
LinearCoordinates[pos_, cols_] := {
  Mod[pos - 1, cols],         (* x = colonna da 0 *)
  Quotient[pos - 1, cols]     (* y = riga da 0 *)
};

(* Genera i primitivi grafici del tabellone *)
BoardPrimitives[cols_Integer:6, rows_Integer:6] := Module[
  {total, path, obstacles, prims},
  total     = cols*rows;
  path      = GeneratePath[cols, rows];
  obstacles = Complement[Range[1, total], path];

  prims = Table[
    Module[{coord = LinearCoordinates[cell, cols], x, y},
      {x, y} = coord;
      {
        EdgeForm[Black],
        FaceForm[
          If[MemberQ[obstacles, cell],
            Gray,                        (* ostacolo *)
            ColorData["Rainbow"][cell/total]  (* percorso *)
          ]
        ],
        Rectangle[{x, y}, {x + 1, y + 1}],
        (* Numero solo sulle celle del percorso *)
        If[MemberQ[path, cell],
          Text[Style[ToString[cell], 8], {x + 0.5, y + 0.5}],
          {}
        ]
      }
    ],
    {cell, 1, total}
  ];

  {prims, obstacles, total, cols, rows}
];

(* Disegna la pedina rossa con bordo nero e cella evidenziata *)
DrawPlayer[position_, cols_] := Module[
  {coord = LinearCoordinates[position, cols]},
  {
    (* bordo verde intorno alla casella *)
    {
      EdgeForm[{Green, Thick}],
      FaceForm[None],
      Rectangle[coord, coord + {1, 1}]
    },
    (* pedina rossa *)
    {
      EdgeForm[Black],
      FaceForm[Red],
      Disk[coord + {0.5, 0.5}, 0.25]
    }
  }
];

End[] 
EndPackage[]