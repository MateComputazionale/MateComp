BeginPackage["Euclide`"]

EuclideComponent::usage = 
  "EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] \
crea un componente per calcolare il GCD usando l'algoritmo di Euclide, mostrando il calcolo passo per passo.";

Begin["`Private`"]

Get["Buttons.m"];
Get["Aiuto.m"];

(* Versione modificata che ritorna un componente invece di creare una finestra di dialogo *)
EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
  DynamicModule[
    {
      currentA = a, 
      currentB = b, 
      steps = {},  (* steps è una lista di liste di questa forma {currentA, currentB, quotient, remainder} *)
      currentStep = 1, (* il passo corrente dell'algoritmo di Euclide *)
      userQuotient = "", 
      userRemainder = "", 
      errorMessage = "", 
      (* True se l'utente ha inserito il quoziente e il resto corretti del corrente 
         passo dell'algoritmo e ha cliccato su Verifica, False altrimenti *)
      isCompleted = False, 
      nextA = "", 
      nextB = ""
    },
    
    Panel[
      Column[{
        Style["Algoritmo di Euclide", Bold, 14],
        (* La funzione Dynamic valuta l'espressione passata come argomento ogni volta che 
           le variabili da cui dipende l'espressione vengono modificate e ritorna il valore 
           dell'espressione, in questo il valore di ritorno viene ingnorato *)
        Dynamic[
          (* La funzione Column posiziona i suoi argomenti in un layout verticale *)
          Column[
            Join[
              (* Visualizzazione dei passi dell'algoritmo di Euclide già completati *)
              (* La funzione Table genera una lista di espressioni iterando su un intervallo. 
                 In questo caso, crea una espressione Row per ogni i da 1 a Length[steps]. *)
              Table[
                (* La funzione With crea una variabile locale stepData che contiene il valore di steps[[i]]. *)
                With[{stepData = steps[[i]]},
                  (* La funzione Row posiziona i suoi argomenti in un layout orizontale *)
                  Row[{
                    Style[Row[{"Passo ", i}], Bold],
                    "     a = ", stepData[[1]], 
                    " b = ", stepData[[2]], 
                    "     ", stepData[[1]], " div ", stepData[[2]],
                    " = ", stepData[[3]], 
                    ", resto ", stepData[[4]]
                  }]
                ],
                {i, Length[steps]}
              ],
              
              If[isCompleted,
                (* isCompleted is True *)
                If[currentB === 0 || currentStep > stepsToComplete,
                  {
                    Style["Algoritmo completato! Il MCD \[EGrave] " <> ToString[currentA], Bold, 14],
                    Button["Avanza di " <> ToString[stepsToComplete] <> " caselle",
                      onSuccessCallback[currentA];
                    ]
                  },
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
                  }
                ],
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
      }, Spacings -> 1], 
      ImageMargins -> 10
    ]
  ];

(* Manteniamo la funzione di dialog originale per compatibilità *)
EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
  CreateDialog[
    EuclideComponent[a, b, stepsToComplete, onSuccessCallback],
    WindowTitle -> "Algoritmo di Euclide", 
    Modal -> True
  ];

End[]
EndPackage[]