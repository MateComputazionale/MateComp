(* ::Package:: *)

(* Board package - Implementa un tabellone di gioco con ostacoli e movimento *)
BeginPackage["Board`"]

(* Funzioni pubbliche *)
BoardPrimitives::usage = 
  "BoardPrimitives[cols, rows] genera il tabellone e restituisce \
{primitives, obstacles, totalCells, cols, rows}.";

GetNextPosition::usage = 
  "GetNextPosition[startPosition, diceRoll, obstaclesList, totalCells] \
calcola la nuova posizione tenendo conto degli ostacoli.";

DrawPlayer::usage =
  "DrawPlayer[position, cols] disegna la pedina del giocatore.";

Begin["`Private`"]

(* CountObstaclesPassed: Conta quanti ostacoli si incontrano in un movimento
   Input:
   - start: posizione di partenza
   - roll: valore del lancio dei dadi
   - obs: lista delle posizioni degli ostacoli
   Output:
   - numero di ostacoli incontrati
   
   La funzione conta quanti ostacoli si incontrano tra la posizione start+1 
   e start+roll. Se si trovano ostacoli, il conteggio viene aggiornato e 
   l'intervallo di controllo viene spostato in avanti.
*)
CountObstaclesPassed[start_, roll_, obs_List] := Module[
  {passed = 0,              (* contatore degli ostacoli superati *)
   a = start + 1,           (* inizio dell'intervallo corrente *)
   b = start + roll,        (* fine dell'intervallo corrente *)
   inRange},                (* ostacoli nell'intervallo corrente *)
  While[True,
    (* Conta quanti ostacoli sono nell'intervallo [a,b] *)
    inRange = Count[obs, _?(Between[{a, b}])];
    (* Se non ci sono ostacoli nell'intervallo, esci dal ciclo *)
    If[inRange == 0, Break[]];
    (* Aggiorna il contatore degli ostacoli *)
    passed += inRange;
    (* Sposta l'intervallo oltre gli ostacoli trovati *)
    a = b + 1;
    b += inRange;
  ];
  passed  (* Ritorna il numero totale di ostacoli superati *)
];

(* GetNextPosition: Calcola la nuova posizione dopo un lancio di dadi
   Input:
   - start: posizione di partenza
   - roll: valore del lancio dei dadi
   - obs: lista delle posizioni degli ostacoli
   - totalCells: numero totale di celle nel tabellone
   Output:
   - nuova posizione, che non supera totalCells
   
   La funzione calcola la nuova posizione considerando il lancio dei dadi
   e gli eventuali ostacoli che si incontrano nel tragitto.
*)
GetNextPosition[start_, roll_, obs_List, totalCells_] := Module[
  {extra,                   (* numero di caselle extra dovute agli ostacoli *)
   newPos},                 (* nuova posizione calcolata *)
  (* Calcola quanti ostacoli si incontrano *)
  extra = CountObstaclesPassed[start, roll, obs];
  (* La nuova posizione è start + roll + extra *)
  newPos = start + roll + extra;
  (* Assicura che la nuova posizione non superi il totale delle celle *)
  Min[newPos, totalCells]
];

(* GeneratePath: Costruisce un percorso pseudocasuale dalla cella 1 alla cella cols*rows
   Input:
   - cols: numero di colonne del tabellone
   - rows: numero di righe del tabellone
   Output:
   - lista delle celle che formano il percorso valido
   
   La funzione genera un percorso valido che parte dalla cella 1 e arriva
   alla cella finale (cols*rows), muovendosi solo a destra o in basso.
*)
GeneratePath[cols_, rows_] := Module[
  {total = cols*rows,       (* numero totale di celle *)
   cur = 1,                 (* posizione corrente *)
   path = {1},              (* percorso, inizia dalla cella 1 *)
   moves,                   (* mosse possibili *)
   nxt},                    (* prossima mossa scelta *)
  While[cur < total,
    (* Le mosse possibili sono: cella a destra o cella sotto *)
    moves = Select[{cur + 1, cur + cols}, # <= total &];
    (* Scegli casualmente una delle mosse possibili *)
    nxt = RandomChoice[moves];
    (* Aggiungi la nuova cella al percorso *)
    AppendTo[path, nxt];
    (* Aggiorna la posizione corrente *)
    cur = nxt;
  ];
  (* Rimuovi eventuali duplicati dal percorso *)
  DeleteDuplicates[path]
];

(* LinearCoordinates: Converte una posizione lineare in coordinate (x,y) su griglia
   Input:
   - pos: posizione lineare (da 1 a cols*rows)
   - cols: numero di colonne del tabellone
   Output:
   - {x,y}: coordinate nella griglia (iniziando da {0,0})
   
   La funzione mappa un indice sequenziale (1,2,3,...) alle coordinate
   corrispondenti in una griglia bidimensionale.
*)
LinearCoordinates[pos_, cols_] := {
  Mod[pos - 1, cols],       (* x = colonna (da 0 a cols-1) *)
  Quotient[pos - 1, cols]   (* y = riga (da 0 a rows-1) *)
};

(* BoardPrimitives: Genera i primitivi grafici del tabellone
   Input:
   - cols: numero di colonne (default: 6)
   - rows: numero di righe (default: 6)
   Output:
   - {primitivi grafici, lista degli ostacoli, numero totale di celle, cols, rows}
   
   La funzione genera tutte le primitive grafiche necessarie per disegnare
   il tabellone, con celle colorate diversamente a seconda che facciano
   parte del percorso o siano ostacoli.
*)
BoardPrimitives[cols_Integer:6, rows_Integer:6] := Module[
  {total,                   (* numero totale di celle *)
   path,                    (* percorso valido *)
   obstacles,               (* lista degli ostacoli *)
   prims},                  (* primitive grafiche *)
  
  (* Calcola il numero totale di celle *)
  total = cols*rows;
  (* Genera un percorso valido *)
  path = GeneratePath[cols, rows];
  (* Gli ostacoli sono tutte le celle che non fanno parte del percorso *)
  obstacles = Complement[Range[1, total], path];

  (* Crea le primitive grafiche per ogni cella *)
  prims = Table[
    Module[{coord = LinearCoordinates[cell, cols], x, y},
      {x, y} = coord;  (* Estrai le coordinate x,y *)
      {
        EdgeForm[Black],  (* Bordo nero per tutte le celle *)
        FaceForm[
          If[MemberQ[obstacles, cell],
            Gray,                           (* Colore grigio per gli ostacoli *)
            ColorData["Rainbow"][cell/total] (* Colore arcobaleno per il percorso *)
          ]
        ],
        (* Disegna il rettangolo della cella *)
        Rectangle[{x, y}, {x + 1, y + 1}],
        (* Aggiungi il numero solo sulle celle del percorso *)
        If[MemberQ[path, cell],
          Text[Style[ToString[cell], 8], {x + 0.5, y + 0.5}],
          {}
        ]
      }
    ],
    {cell, 1, total}  (* Per ogni cella da 1 a total *)
  ];

  (* Ritorna le primitive grafiche, gli ostacoli, e le informazioni sul tabellone *)
  {prims, obstacles, total, cols, rows}
];

(* DrawPlayer: Disegna la pedina del giocatore sulla cella corrente
   Input:
   - position: posizione lineare del giocatore
   - cols: numero di colonne del tabellone
   Output:
   - primitive grafiche per rappresentare il giocatore
   
   La funzione crea le primitive grafiche per evidenziare la cella
   corrente con un bordo verde e disegnare la pedina del giocatore
   come un disco rosso.
*)
DrawPlayer[position_, cols_] := Module[
  {coord = LinearCoordinates[position, cols]},  (* Converti in coordinate (x,y) *)
  {
    (* Bordo verde spesso intorno alla casella corrente *)
    {
      EdgeForm[{Green, Thick}],
      FaceForm[None],  (* Cella trasparente per vedere il colore sottostante *)
      Rectangle[coord, coord + {1, 1}]
    },
    (* Pedina del giocatore: disco rosso con bordo nero *)
    {
      EdgeForm[Black],
      FaceForm[Red],
      Disk[coord + {0.5, 0.5}, 0.25]  (* Centrato nella cella con raggio 0.25 *)
    }
  }
];

End[] 
EndPackage[]