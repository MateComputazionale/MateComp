(* ::Package:: *)

BeginPackage["Euclide`"]

EuclideComponent::usage = 
  "EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] \
crea un componente per calcolare il GCD usando l'algoritmo di Euclide, mostrando il calcolo passo per passo.";

Begin["`Private`"]

Get["Buttons.m"];
Get["Aiuto.m"];

EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
  DynamicModule[{
    currentA = a, currentB = b, steps = {}, currentStep = 1,
    userQuotient = "", userRemainder = "", errorMessage = "", 
    isCompleted = False, nextA = "", nextB = "", userMCD = "", mcdMessage = ""
  },
    
    Panel[
      Column[{
        Style["Algoritmo di Euclide", Bold, 14],
        Dynamic[
          Column[
            Join[
              Table[
                With[{stepData = steps[[i]]},
                  Row[{
                    Style[Row[{"Passo ", i}], Bold],
                    "     a = ", stepData[[1]], 
                    " b = ", stepData[[2]], 
                    "     ", stepData[[1]], " div ", stepData[[2]], 
                    " = ", stepData[[3]], ", resto ", stepData[[4]]
                  }]
                ],
                {i, Length[steps]}
              ],
              
              If[currentB === 0,
                  {
                    Style["Algoritmo completato! Il MCD \[EGrave] " <> ToString[currentA], Bold, 14],
                    Button["Avanza di " <> ToString[stepsToComplete] <> " caselle",
                      onSuccessCallback[currentA];
                    ]
                  },
                  If[isCompleted,
                  {
                    Style["Inserisci i nuovi valori per a e b per continuare:", Bold],
                    Row[{
                      "a = ", InputField[Dynamic[nextA], Number, FieldSize -> 5], "   ",
                      "b = ", InputField[Dynamic[nextB], Number, FieldSize -> 5]
                    }],
                    Dynamic[Style[errorMessage, Red] ],
                    Button["Prosegui",
                      If[nextA === currentB && nextB === Mod[currentA, currentB],
                        currentA = nextA;
                        currentB = nextB;
                        userQuotient = "";
                        userRemainder = "";
                        nextA = "";
                        nextB = "";
                        errorMessage = "";
                        isCompleted = False;
                      ,
                        errorMessage = "Risposta errata! Riprova."
                      ]
                    ]
                  },
                (* isCompleted is False *)
                {
                  (* Visualizzazione del attuale passo dell'algoritmo di Euclide *)
                  Row[{
                    Style[Row[{"Passo ", currentStep}], Bold],
                    "    a = ", currentA,
                    ", b = ", currentB,
                    "     ", currentA, " div ", currentB,
                    " = ", InputField[Dynamic[userQuotient], Number, FieldSize -> 5],
                    ", resto ",
                    InputField[Dynamic[userRemainder], Number, FieldSize -> 5]
                  }],
                  
                  Dynamic[Style[errorMessage, Red] ],
                  
                  ClearFields[userQuotient, userRemainder, errorMessage],
                  
                  Button["Aiuto",
                    Aiuto`MostraAiuto[currentA, currentB]
                  ],
                  
                  Button["Verifica",
                    If[userQuotient === Quotient[currentA, currentB] && 
                       userRemainder === Mod[currentA, currentB],
                      AppendTo[steps, {currentA, currentB, userQuotient, userRemainder}];
                      currentStep++;
                      userQuotient = "";
                      userRemainder = "";
                      errorMessage = "";
                      isCompleted = True;
                    ,
                      errorMessage = "Risposta errata! Riprova."
                    ]
                  ]
                }
              ]
            ]
          ]
        ]
        ]
      }, Spacings -> 1],
      ImageMargins -> 10
    ]
  ];

EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
  CreateDialog[
    EuclideComponent[a, b, stepsToComplete, onSuccessCallback],
    WindowTitle -> "Algoritmo di Euclide",
    Modal -> True
  ];

End[]
EndPackage[]
