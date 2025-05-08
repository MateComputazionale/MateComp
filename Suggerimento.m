(* ::Package:: *)

BeginPackage["Suggerimento`"]

MostraSuggerimento::usage = 
  "MostraSuggerimento[a_Integer, b_Integer] mostra un albero decisionale per capire come vengono scelti i numeri nell'algoritmo di Euclide.";

Begin["`Private`"]

MostraSuggerimento[a_Integer, b_Integer] := 
 Module[{precedente = a, resto = b},
  CreateDialog[
   Column[{
     Style["Suggerimento per il prossimo passo", Bold, 14],
     "Nel prossimo passo dell'algoritmo di Euclide:",
     Row[{"\[Bullet] ", Style["a = ", Bold], precedente}],
     Row[{"\[Bullet] ", Style["b = ", Bold], resto}],
     "Il numero a \[EGrave] il numero precedente, mentre b \[EGrave] il resto del passo precedente.",
     
     Graphics[
      {
       Arrowheads[0.03],
       Text[Style[ToString[precedente], Bold, 16], {0, 0}],
       Arrow[{{0.5, 0}, {2, 0}}],
       Text[Style[ToString[resto], Bold, 16], {2.5, 0}],
       Text["\[LeftArrow] resto del passo precedente", {2.5, -0.5}, Left]
      },
      PlotRange -> {{-1, 4}, {-2, 2}},
      ImageSize -> 400
     ],
     
     Style["Questo suggerimento ti aiuta a capire da dove vengono i numeri nel prossimo passo dell'algoritmo.", Italic, 11],
     Button["Chiudi", DialogReturn[]]
     }],
   WindowTitle -> "Suggerimento Algoritmo di Euclide",
   Modal -> True
   ]
  ]

End[]
EndPackage[]



