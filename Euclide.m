(* ::Package:: *)

BeginPackage["Euclide`"]

EuclideComponent::usage = 
  "EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] \
crea un componente per calcolare il GCD usando l'algoritmo di Euclide, mostrando il calcolo passo per passo.";

Begin["`Private`"]

Get["Buttons.m"];
Get["Aiuto.m"];
Get["Suggerimento.m"];

EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
 DynamicModule[{
   currentA = a, currentB = b, steps = {}, currentStep = 1,
   userQuotient = "", userRemainder = "", errorMessage = "", 
   isCompleted = False, nextA = "", nextB = "", userMCD = "", mcdMessage = "",
   showHelp = False, helpContent = Null
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
           onSuccessCallback[currentA]
          ]
         },
         
         If[isCompleted,
          {
           Style["Inserisci i nuovi valori per a e b per continuare:", Bold],
           Row[{
             "a = ", InputField[Dynamic[nextA], Number, FieldSize -> 5],
             "   ",
             "b = ", InputField[Dynamic[nextB], Number, FieldSize -> 5]
           }],
           Dynamic[Style[errorMessage, Red]],
           Row[{
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
               showHelp = False;
               ,
               errorMessage = "Risposta errata! Riprova."
              ]
             ],
             Button["Pulisci campi",
              (nextA = ""; nextB = ""; errorMessage = ""),
              ImageMargins -> 5
             ]
             Button["Aiuto",
            MostraSuggerimento[currentA, currentB]
           ]
           }]
          },
          
          {
           Row[{
             Style[Row[{"Passo ", currentStep}], Bold],
             "    a = ", currentA,
             ", b = ", currentB,
             "     ", currentA, " div ", currentB,
             " = ", InputField[Dynamic[userQuotient], Number, FieldSize -> 5],
             ", resto ",
             InputField[Dynamic[userRemainder], Number, FieldSize -> 5]
           }],
           
           Dynamic[Style[errorMessage, Red]],
           
           ClearFields[userQuotient, userRemainder, errorMessage],
           
           Button[Dynamic[If[showHelp, "Nascondi aiuto", "Aiuto"]],
            showHelp = !showHelp;
            If[showHelp, helpContent = MostraAiuto[currentA, currentB]];
           ],
           
           Button["Verifica",
            If[userQuotient === Quotient[currentA, currentB] && 
               userRemainder === Mod[currentA, currentB],
             AppendTo[steps, {currentA, currentB, userQuotient, userRemainder}];
             currentStep++;
             If[Mod[currentA, currentB] === 0,
              (* Algoritmo completato *)
              currentB = 0; (* Forza il completamento *)
             ,
              isCompleted = True;
             ];
             userQuotient = "";
             userRemainder = "";
             errorMessage = "";
             showHelp = False;
             ,
             errorMessage = "Risposta errata! Riprova.";
             userQuotient = ""; userRemainder = "";
            ]
           ]
          }
         ]
        ],
        
        (* Mostra l'aiuto se richiesto *)
        If[showHelp && helpContent =!= Null,
          {
           Spacer[5],
           helpContent
          },
          {}
        ]
       ]
      ]
     ]
    }, Spacings -> 1],
   ImageMargins -> 10
  ]
 ]

EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
 CreateDialog[
  EuclideComponent[a, b, stepsToComplete, onSuccessCallback],
  WindowTitle -> "Algoritmo di Euclide",
  Modal -> True
 ]

End[]
EndPackage[]



