(* Definizione del pacchetto per la visualizzazione del dado *)
BeginPackage["Dice`"]

(* Dichiarazione dei simboli esportati dal pacchetto *)
DrawDice::usage = "DrawDice[value] disegna un dado con il valore specificato.";

(* Inizio della sezione privata del pacchetto *)
Begin["`Private`"]

(* Funzione per disegnare un dado con un valore specifico *)
DrawDice[value_Integer] := Module[
  {diceSize = 100, dotSize = 16, dotPositions},
  
  (* Definizione delle posizioni dei punti per ogni valore del dado *)
  dotPositions = Switch[value,
    1, {{0, 0}},  (* Centro *)
    
    2, {{-0.3, 0.3}, {0.3, -0.3}},  (* In diagonale *)
    
    3, {{-0.3, 0.3}, {0, 0}, {0.3, -0.3}},  (* Diagonale + centro *)
    
    4, {{-0.3, 0.3}, {0.3, 0.3}, {-0.3, -0.3}, {0.3, -0.3}},  (* Ai quattro angoli *)
    
    5, {{-0.3, 0.3}, {0.3, 0.3}, {0, 0}, {-0.3, -0.3}, {0.3, -0.3}},  (* Quattro angoli + centro *)
    
    6, {{-0.3, 0.3}, {0.3, 0.3}, {-0.3, 0}, {0.3, 0}, {-0.3, -0.3}, {0.3, -0.3}},  (* Due colonne da tre *)
    
    _, {}  (* Per valori non validi, nessun punto *)
  ];
  
  (* Creazione del grafico del dado *)
  Graphics[{
    (* Sfondo bianco del dado con bordo nero *)
    {EdgeForm[{Thick, Black}], White, 
     Rectangle[{-0.5, -0.5}, {0.5, 0.5}, RoundingRadius -> 0.1]},
    
    (* Disegna i punti neri sul dado *)
    {Black, 
     Map[Disk[#, 0.08] &, dotPositions]}
  },
  ImageSize -> diceSize,  (* Dimensione dell'immagine *)
  PlotRange -> {{-0.6, 0.6}, {-0.6, 0.6}}  (* Range di visualizzazione *)
  ]
];

(* Fine della sezione privata del pacchetto *)
End[]

(* Fine della definizione del pacchetto *)
EndPackage[]