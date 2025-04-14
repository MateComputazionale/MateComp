BeginPackage["Euclide`"]

EuclideDialog::usage = 
  "EuclideDialog[a_Integer, b_Integer, stepCount_Integer, onSuccess_Function] \
mostra un dialogo interattivo per calcolare il MCD, visualizzando passo a passo il calcolo.";

Begin["`Private`"]

Get["Buttons.m"]; 

EuclideDialog[a_Integer, b_Integer, stepCount_Integer, onSuccess_Function] := 
  CreateDialog[
    DynamicModule[
      {
        currentA = a, currentB = b, steps = {}, currentStep = 1,
        locQuotient = "", locRemainder = "", locMessage = "", completed = False
      },
      Column[{
        Style["Algoritmo di Euclide", Bold, 14],
        Dynamic[
          Column[
            Join[
              (* Visualizza i passaggi completati *)
              Table[
                With[{s = steps[[i]]},
                  Row[{
                    "Passo ", i, ": ",
                    s[[1]], " div ", s[[2]], " = ", s[[3]], ", resto ", s[[4]]
                  }]
                ],
                {i, Length[steps]}
              ],
              (* Visualizza il passo corrente oppure il messaggio finale se completato *)
              If[completed,
                {
                  Style["Algoritmo completato! Il MCD è " <> ToString[currentA], Bold, 14],
                  Button["Avanza di " <> ToString[stepCount] <> " caselle",
                    DialogReturn[];
                    onSuccess[]
                  ]
                },
                {
                  Row[{
                    "Passo ", currentStep, ": ",
                    currentA, " div ", currentB, " = ",
                    InputField[Dynamic[locQuotient], String, FieldSize -> 5],
                    ", resto ",
                    InputField[Dynamic[locRemainder], String, FieldSize -> 5]
                  }],
                  Dynamic[Style[locMessage, Red]],
                  ClearFields[locQuotient, locRemainder, locMessage],
                  Button["Verifica",
                    Module[{q, r},
                      q = ToExpression[locQuotient];
                      r = ToExpression[locRemainder];
                      If[! NumericQ[q] || ! NumericQ[r],
                        locMessage = "Inserisci numeri validi.",
                        If[q === Quotient[currentA, currentB] && r === Mod[currentA, currentB],
                          AppendTo[steps, {currentA, currentB, q, r}];
                          (* Aggiorna i valori per il passo successivo *)
                          currentA = currentB;
                          currentB = r;
                          currentStep++;
                          locQuotient = "";
                          locRemainder = "";
                          locMessage = "";
                          If[currentB === 0 || currentStep > stepCount,
                            completed = True
                          ],
                          locMessage = "Risposta errata. Riprova."
                        ]
                      ]
                    ]
                  ]
                }
              ]
            ]
          ]
        ]
      }],
      WindowTitle -> "Algoritmo di Euclide", Modal -> True
    ]
  ];

End[]
EndPackage[]
