BeginPackage["Aiuto`"]

MostraAiuto::usage = 
  "MostraAiuto[a_Integer, b_Integer] mostra la divisione a = q*b + r come gruppi di aiuto colorate in base al valore (blu=10, arancione=1).";

Begin["`Private`"]

(* Funzione per creare aiuto che rappresentano un numero n *)
CreaAiutoValore[n_Integer, radius_: 0.2] := Module[
  {blu, arancioni},
  blu = Quotient[n, 10];
  arancioni = Mod[n, 10];
  
  Join[
    Table[
      Graphics[{EdgeForm[Black], FaceForm[Blue], Disk[{0, 0}, radius]}, ImageSize -> 30],
      {blu}
    ],
    Table[
      Graphics[{EdgeForm[Black], FaceForm[Orange], Disk[{0, 0}, radius]}, ImageSize -> 30],
      {arancioni}
    ]
  ]
];

MostraAiuto[a_Integer, b_Integer] := Module[
  {
    q = Quotient[a, b], r = Mod[a, b],
    gruppi, resto, radius = 0.2
  },
  
gruppi = {
  Row[CreaAiutoValore[b, radius], Spacer[5] ],
  Style["× " <> ToString[q] <> " gruppi", Italic, Gray]
};



  resto = CreaAiutoValore[r, radius];

  CreateDialog[
    Column[{
      Style["MCD visivamente:", Bold, 14],
      "Dividi a = " <> ToString[a] <> " palline in gruppi da b = " <> ToString[b] <> ".",
      Style["Quoziente: " <> ToString[q] <> " gruppi, resto " <> ToString[r], Italic],
      Column[gruppi],
      If[r > 0,
        Row[{"Resto: ", Row[resto]}],
        Nothing
      ],
      Button["Chiudi", DialogReturn[] ]
    }],
    Scrollbars -> True,
    WindowTitle -> "Aiuto Visivo - Divisione"
  ]
];

End[]
EndPackage[]
