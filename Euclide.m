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
   showHelpAiuto = False, helpAiutoContent = Null, 
   showHelpSuggerimento = False, helpSuggerimentoContent = Null
 },
  
  Panel[
   Column[{
     Style["Algoritmo di Euclide", Bold, 14],
     Dynamic[
      Column[
       Join[

        (* Passi gi\[AGrave] completati *)
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
        
        (* Fase: completato *)
        If[currentB === 0,
         {
          Style["Algoritmo completato! Il MCD \[EGrave] " <> ToString[currentA], Bold, 14],
          Button["Avanza di " <> ToString[stepsToComplete] <> " caselle",
           onSuccessCallback[currentA]
          ]
         },
         
         (* Fase: inserimento nuovi a e b *)
         If[isCompleted,
          {
           Style["Inserisci i nuovi valori per a e b per continuare:", Bold],
           Row[{
             "a = ", InputField[Dynamic[nextA], Number, FieldSize -> 5],
             "   ",
             "b = ", InputField[Dynamic[nextB], Number, FieldSize -> 5]
           }],
           Dynamic[Style[errorMessage, Red]],
           
           Button["Pulisci campi", 
             nextA = ""; nextB = ""; errorMessage = "",
             ImageSize -> {Scaled[1], Automatic}
           ],
           
           Button[
             Dynamic[If[showHelpSuggerimento, "Nascondi suggerimento", "Suggerimento"]],
             showHelpSuggerimento = !showHelpSuggerimento;
             If[showHelpSuggerimento,
              helpSuggerimentoContent = Suggerimento`MostraSuggerimento[currentA, currentB]
             ],
             ImageSize -> {Scaled[1], Automatic}
           ],
           
           Button["Verifica",
             If[nextA === currentB && nextB === Mod[currentA, currentB],
              currentA = nextA;
              currentB = nextB;
              userQuotient = "";
              userRemainder = "";
              nextA = "";
              nextB = "";
              errorMessage = "";
              isCompleted = False;
              showHelpSuggerimento = False;
              ,
              errorMessage = "I valori inseriti per a e b non sono corretti! Riprova."
             ],
             ImageSize -> {Scaled[1], Automatic}
           ],
           
           If[showHelpSuggerimento && helpSuggerimentoContent =!= Null,
            helpSuggerimentoContent,
            Nothing
           ]
          },
          
          (* Fase: input quoziente e resto *)
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
           
           Button["Pulisci campi", 
             userQuotient = ""; userRemainder = ""; errorMessage = "",
             ImageSize -> {Scaled[1], Automatic}
           ],
           
           Button[
             Dynamic[If[showHelpAiuto, "Nascondi aiuto", "Aiuto"]],
             showHelpAiuto = !showHelpAiuto;
             If[showHelpAiuto,
              helpAiutoContent = Aiuto`MostraAiuto[currentA, currentB]
             ],
             ImageSize -> {Scaled[1], Automatic}
           ],
           
           Button["Verifica",
             If[userQuotient === Quotient[currentA, currentB] && 
                userRemainder === Mod[currentA, currentB],
              AppendTo[steps, {currentA, currentB, userQuotient, userRemainder}];
              currentStep++;
              If[Mod[currentA, currentB] === 0,
               currentB = 0; (* Algoritmo completato *)
              ,
               isCompleted = True;
              ];
              userQuotient = "";
              userRemainder = "";
              errorMessage = "";
              showHelpAiuto = False;
              ,
              errorMessage = "Risposta errata! Riprova.";
              userQuotient = ""; userRemainder = "";
             ],
             ImageSize -> {Scaled[1], Automatic}
           ],
           
           If[showHelpAiuto && helpAiutoContent =!= Null,
            helpAiutoContent,
            Nothing
           ]
          }
         ]
        ]
       ], Spacings -> 1
      ]
     ]
    }, Spacings -> 1],
   ImageMargins -> 10,
   ImageSize -> {400, Automatic},
   Alignment -> Left
  ]
 ]

EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
 CreateDialog[
  EuclideComponent[a, b, stepsToComplete, onSuccessCallback],
  WindowTitle -> "Algoritmo di Euclide",
  WindowSize -> {500, Automatic},
  Modal -> True
 ]

End[]
EndPackage[]



