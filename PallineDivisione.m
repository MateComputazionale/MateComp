BeginPackage["PallineDivisione`"]

MostraPalline::usage = 
  "MostraPalline[a_Integer, b_Integer] mostra la divisione a = q*b + r come gruppi di palline.";

Begin["`Private`"]

MostraPalline[a_Integer, b_Integer] := Module[
  {
    q = Quotient[a, b], r = Mod[a, b],
    gruppi, resto, radius = 0.2
  },
  
  gruppi = Table[
    Row[
      Table[
        Graphics[{EdgeForm[Black], FaceForm[Orange], Disk[{0, 0}, radius]},
          ImageSize -> 30
        ],
        {b}
      ],
      Spacer[5]
    ],
    {q}
  ];

  resto = Table[
    Graphics[{EdgeForm[Black], FaceForm[Lighter[Orange] ], Disk[{0, 0}, radius]},
      ImageSize -> 30
    ],
    {r}
  ];

  CreateDialog[
    Column[{
      Style["Divisione con le Palline", Bold, 14],
      "Dividi " <> ToString[a] <> " palline in gruppi da " <> ToString[b] <> ".",
      Style["Risultato: " <> ToString[q] <> " gruppi, " <> ToString[r] <> " palline restanti", Italic],
      Column[gruppi],
      If[r > 0, 
        Row[{"Resto: ", Row[resto]}], 
        Nothing
      ],
      Button["Chiudi", DialogReturn[] ]
    }],
    Scrollbars -> True
  ],
  WindowTitle -> "Aiuto Visivo - Divisione"
];

End[]
EndPackage[]
