(* ::Package:: *)

BeginPackage["Suggerimento`"]

MostraSuggerimento::usage =
  "MostraSuggerimento[a_Integer, b_Integer] mostra un passo dell'algoritmo di Euclide con cerchi colorati, etichette e passaggi successivi."

Begin["`Private`"]

(* Crea un cerchio colorato con label opzionale *)
CreaNodoGrafico[val_, colore_, label_: ""] := 
  Graphics[{
    EdgeForm[Black], FaceForm[colore], 
    Disk[{0, 0}, 0.1],
    Text[Style[ToString[val], 10, Black], {0, 0}],
    If[label =!= "", 
      Text[Style[label, 8, Black], {0, -0.18}], 
      Nothing]
  }, ImageSize -> 40]

MostraSuggerimento[a_Integer, b_Integer] := Module[
  {
    r = Mod[a, b],
    q = Quotient[a, b],
    nodoA, nodoB, nodoR, nodoNextA, nodoNextB
  },
  
  nodoA = CreaNodoGrafico[a, LightGray, "A"];
  nodoB = CreaNodoGrafico[b, Yellow, "B"];
  nodoR = CreaNodoGrafico[r, LightBlue, "A mod B"];
  nodoNextA = CreaNodoGrafico[b, Yellow, "Prossimo A"];
  nodoNextB = CreaNodoGrafico[r, LightBlue, "Prossimo B"];
  
  CreateDialog[
    Column[{
      Style["Suggerimento", Bold, 14],
      Row[{"Il prossimo A \[EGrave] ", Style[b, Bold], , " e il prossimo B \[EGrave] ", Style[r, Bold]}],
      
      Style["Passo Corrente:", Bold, 12],
        Row[{nodoA, Spacer[10], nodoB, Spacer[10], nodoR}],
      
      Style["Prossimo passo:", Bold, 12],
      Row[{"Il prossimo A \[EGrave] il vecchio B", Spacer[10], nodoNextA}],
      Row[{"Il prossimo B \[EGrave] A mod B", Spacer[10], nodoNextB}],
      
      Spacer[10],
      Button["Chiudi", DialogReturn[]]
    }],
    WindowTitle -> "Suggerimento Visivo - Euclide",
    Scrollbars -> True
  ]
];

End[]
EndPackage[]






