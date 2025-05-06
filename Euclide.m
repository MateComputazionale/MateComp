(* ::Package:: *)

BeginPackage["Euclide`"]

EuclideDialog::usage = 
  "EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] \
displays an interactive dialog for calculating GCD using Euclid's algorithm, showing step-by-step calculation.";

Begin["`Private`"]

Get["Buttons.m"];
Get["PallineDivisione.m"];

EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
  CreateDialog[
    DynamicModule[
      {
        currentA = a, 
        currentB = b, 
        steps = {}, 
        currentStep = 1,
        userQuotient = "", 
        userRemainder = "", 
        errorMessage = "", 
        isCompleted = False,
        nextA = "", 
        nextB = ""
      },
      
      Column[{
        Style["Algoritmo di Euclide", Bold, 14],
        
        Dynamic[
          Column[
            Join[
              Table[
                With[{stepData = steps[[i]]},
                  Row[{
                    "Passo ", i, ": ",
                    stepData[[1]], " div ", stepData[[2]], " = ", 
                    stepData[[3]], ", resto ", stepData[[4]]
                  }]
                ],
                {i, Length[steps]}
              ],
              
              If[isCompleted,
                If[currentB === 0 || currentStep > stepsToComplete,
                  {
                    Style["Algoritmo completato! Il MCD \[EGrave] " <> ToString[currentA], Bold, 14],
                    Button["Avanza di " <> ToString[stepsToComplete] <> " caselle",
                      DialogReturn[];
                      onSuccessCallback[currentA];
                    ]
                  },
                  {
                    Style["Inserisci i nuovi valori per a e b per continuare:", Bold],
                    Row[{
                      "a = ", InputField[Dynamic[nextA], String, FieldSize -> 5],
                      "   b = ", InputField[Dynamic[nextB], String, FieldSize -> 5]
                    }],
                    Dynamic[Style[errorMessage, Red]],
                    Button["Prosegui",
                      Module[{newA, newB},
                        If[StringMatchQ[nextA, DigitCharacter ..] && StringMatchQ[nextB, DigitCharacter ..],
                          newA = ToExpression[nextA];
                          newB = ToExpression[nextB];
                          If[NumericQ[newA] && NumericQ[newB] && newB =!= 0,
                            currentA = newA;
                            currentB = newB;
                            userQuotient = "";
                            userRemainder = "";
                            nextA = "";
                            nextB = "";
                            errorMessage = "";
                            isCompleted = False;
                          ,
                            errorMessage = "I valori devono essere numeri validi. b deve essere diverso da zero."
                          ],
                          errorMessage = "Inserisci numeri validi per a e b."
                        ]
                      ]
                    ]
                  }
                ],
                {
                  Row[{
                    "Passo ", currentStep, ": ",
                    currentA, " div ", currentB, " = ",
                    InputField[Dynamic[userQuotient], String, FieldSize -> 5],
                    ", resto ",
                    InputField[Dynamic[userRemainder], String, FieldSize -> 5]
                  }],
                  
                  Dynamic[Style[errorMessage, Red]],
                  
                  ClearFields[userQuotient, userRemainder, errorMessage],
                  
                  Button["Mostra Palline",
                    PallineDivisione`MostraPalline[currentA, currentB]
                  ],
                  
                  Button["Verifica",
                    Module[{quotient, remainder},
                      If[StringMatchQ[userQuotient, DigitCharacter ..] &&
                         StringMatchQ[userRemainder, DigitCharacter ..],
                        quotient = ToExpression[userQuotient];
                        remainder = ToExpression[userRemainder];
                      ];
                      
                      If[! NumericQ[quotient] || ! NumericQ[remainder],
                        errorMessage = "Inserisci numeri validi.",
                        If[quotient === Quotient[currentA, currentB] && 
                           remainder === Mod[currentA, currentB],
                          AppendTo[steps, {currentA, currentB, quotient, remainder}];
                          currentStep++;
                          userQuotient = "";
                          userRemainder = "";
                          errorMessage = "";
                          isCompleted = True;
                        ,
                          errorMessage = "Risposta errata. Riprova."
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
      WindowTitle -> "Algoritmo di Euclide", 
      Modal -> True
    ]
  ];

End[]
EndPackage[]



