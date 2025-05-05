BeginPackage["Euclide`"]

EuclideDialog::usage = 
  "EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] \
displays an interactive dialog for calculating GCD using Euclid's algorithm, showing step-by-step calculation.";

Begin["`Private`"]

Get["Buttons.m"];
Get["PallineDivisione.m"];

(* Main dialog function *)
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
        isCompleted = False
      },
      
      Column[{
        (* Dialog header *)
        Style["Algoritmo di Euclide", Bold, 14],
        
        Dynamic[
          Column[
            Join[
              (* Display completed steps *)
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
              
              (* Display current step or completion message *)
              If[isCompleted,
                {
                  Style["Algoritmo completato! Il MCD e' " <> ToString[currentA], Bold, 14],
                  Button["Avanza di " <> ToString[stepsToComplete] <> " caselle",
                    DialogReturn[];
                    onSuccessCallback[currentA]; (* Pass the GCD result to the callback *)
                  ]
                },
                {
                  (* Current step input form *)
                  Row[{
                    "Passo ", currentStep, ": ",
                    currentA, " div ", currentB, " = ",
                    InputField[Dynamic[userQuotient], String, FieldSize -> 5],
                    ", resto ",
                    InputField[Dynamic[userRemainder], String, FieldSize -> 5]
                  }],
                  
                  (* Error message display *)
                  Dynamic[Style[errorMessage, Red] ],
                  
                  (* Clear fields button *)
                  ClearFields[userQuotient, userRemainder, errorMessage],
                  
                  Button[ "Mostra Palline",
                    PallineDivisione`MostraPalline[currentA, currentB]
                  ],

                  (* Verification button *)
                  Button["Verifica",
                    Module[{quotient, remainder},

                    (* DigitCharacter is a built-in symbol that represents a pattern that matches any single digit character (0 through 9). 
                       The notation .. (double dot) is used to indicate that the preceding pattern can repeat zero or more times. *)
                    If[StringMatchQ[userQuotient, DigitCharacter ..] || 
                        StringMatchQ[userRemainder, DigitCharacter ..],
                        quotient = ToExpression[userQuotient];
                        remainder = ToExpression[userRemainder];
                    ]
                                            
                      If[! NumericQ[quotient] || ! NumericQ[remainder],
                        errorMessage = "Inserisci numeri validi.",
                        If[quotient === Quotient[currentA, currentB] && 
                           remainder === Mod[currentA, currentB],
                          (* Correct answer *)
                          AppendTo[steps, {currentA, currentB, quotient, remainder}];
                          
                          (* Update values for next step *)
                          currentA = currentB;
                          currentB = remainder;
                          currentStep++;
                          userQuotient = "";
                          userRemainder = "";
                          errorMessage = "";
                          
                          (* Check if algorithm is complete *)
                          If[currentB === 0 || currentStep > stepsToComplete,
                            isCompleted = True
                          ],
                          
                          (* Incorrect answer *)
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
