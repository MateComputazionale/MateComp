(* ::Package:: *)

(* Inizio del pacchetto Euclide *)
BeginPackage["Euclide`"]

(* Descrizione della funzione EuclideDialog che mostra il dialogo interattivo per calcolare il MCD usando l'algoritmo di Euclide *)
EuclideDialog::usage = 
  "EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] \
displays an interactive dialog for calculating GCD using Euclid's algorithm, showing step-by-step calculation.";

(* Inizio del contesto privato per definire le funzioni interne del pacchetto *)
Begin["`Private`"]

(* Caricamento di file esterni che contengono funzionalit\[AGrave] ausiliarie *)
Get["Buttons.m"];
Get["PallineDivisione.m"];

(* Definizione della funzione principale EuclideDialog *)
EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] := 
  CreateDialog[
    DynamicModule[{
      (* Variabili locali per gestire lo stato dell'interfaccia e dell'algoritmo *)
      currentA = a,           (* Numero corrente a *)
      currentB = b,           (* Numero corrente b *)
      steps = {},             (* Lista che memorizza i passi dell'algoritmo *)
      currentStep = 1,        (* Numero del passo attuale *)
      userQuotient = "",      (* Quoziente inserito dall'utente *)
      userRemainder = "",     (* Resto inserito dall'utente *)
      errorMessage = "",      (* Messaggio di errore *)
      isCompleted = False,    (* Stato di completamento dell'algoritmo *)
      nextA = "",             (* Valore successivo per a *)
      nextB = "",             (* Valore successivo per b *)
      expectedA = a,          (* Valore atteso per a al prossimo passo *)
      expectedB = b           (* Valore atteso per b al prossimo passo *)
    },
    
    (* Interfaccia grafica del dialogo *)
    Column[{
      (* Titolo dell'interfaccia *)
      Style["Algoritmo di Euclide", Bold, 14],
      
      (* Dinamica per aggiornare la lista dei passi dell'algoritmo *)
      Dynamic[
        Column[
          Join[
            (* Mostra i passi gi\[AGrave] completati *)
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
            
            (* Se l'algoritmo \[EGrave] completato, mostra il messaggio di successo *)
            If[isCompleted,
              If[currentB === 0 || currentStep > stepsToComplete,
                {
                  Style["Algoritmo completato! Il MCD \[EGrave] " <> ToString[currentA], Bold, 14],
                  Button["Avanza di " <> ToString[stepsToComplete] <> " caselle",
                    DialogReturn[]; (* Chiude il dialogo e chiama la callback di successo *)
                    onSuccessCallback[currentA];
                  ]
                },
                (* Se l'algoritmo non \[EGrave] completato, permetti di inserire nuovi valori per a e b *)
                {
                  Style["Inserisci i nuovi valori per a e b per continuare:", Bold],
                  Row[{
                    "a = ", InputField[Dynamic[nextA], String, FieldSize -> 5],
                    "   b = ", InputField[Dynamic[nextB], String, FieldSize -> 5]
                  }],
                  Dynamic[Style[errorMessage, Red]],  (* Mostra eventuali errori *)
                  Button["Prosegui",
                    Module[{newA, newB},
                      (* Verifica che i valori inseriti siano numerici e validi *)
                      If[StringMatchQ[nextA, DigitCharacter ..] && StringMatchQ[nextB, DigitCharacter ..],
                        newA = ToExpression[nextA];
                        newB = ToExpression[nextB];
                        If[NumericQ[newA] && NumericQ[newB] && newB =!= 0,
                          (* Verifica che i nuovi valori corrispondano a quelli attesi *)
                          If[newA === expectedA && newB === expectedB,
                            currentA = newA;
                            currentB = newB;
                            userQuotient = "";  (* Resetta i campi dell'utente *)
                            userRemainder = "";
                            nextA = "";         (* Resetta i valori inseriti *)
                            nextB = "";
                            errorMessage = "";
                            isCompleted = False;  (* Rende l'algoritmo incompleto per continuare *)
                          ,
                            (* Se i valori non sono corretti, mostra un messaggio di errore *)
                            nextA = "";
                            nextB = "";
                            errorMessage = "I valori per a e b non sono corretti. Riprova."
                          ],
                          errorMessage = "I valori devono essere numeri validi. b deve essere diverso da zero."
                        ],
                        errorMessage = "Inserisci numeri validi per a e b."
                      ]
                    ]
                  ]
                }
              ],
              (* Altrimenti, mostra il passo corrente con input per quoziente e resto *)
              {
                Row[{
                  "Passo ", currentStep, ": ",
                  currentA, " div ", currentB, " = ",
                  InputField[Dynamic[userQuotient], String, FieldSize -> 5],
                  ", resto ",
                  InputField[Dynamic[userRemainder], String, FieldSize -> 5]
                }],
                
                Dynamic[Style[errorMessage, Red]],  (* Mostra eventuali errori *)
                
                ClearFields[userQuotient, userRemainder, errorMessage],  (* Pulisce i campi dopo l'invio *)
                
                (* Pulsante per mostrare le palline che rappresentano la divisione *)
                Button["Mostra Palline",
                  PallineDivisione`MostraPalline[currentA, currentB]
                ],
                
                (* Pulsante per verificare la risposta dell'utente e aggiornare i passi *)
                Button["Verifica",
                  Module[{quotient, remainder},
                    If[StringMatchQ[userQuotient, DigitCharacter ..] &&
                       StringMatchQ[userRemainder, DigitCharacter ..],
                      quotient = ToExpression[userQuotient];
                      remainder = ToExpression[userRemainder];
                    ];
                    
                    (* Se i valori inseriti sono corretti, aggiorna i passi *)
                    If[! NumericQ[quotient] || ! NumericQ[remainder],
                      errorMessage = "Inserisci numeri validi.",
                      If[quotient === Quotient[currentA, currentB] && 
                         remainder === Mod[currentA, currentB],
                        AppendTo[steps, {currentA, currentB, quotient, remainder}];  (* Aggiungi il passo alla lista *)
                        currentStep++;  (* Passa al passo successivo *)
                        userQuotient = "";
                        userRemainder = "";
                        errorMessage = "";
                        
                        (* Calcola i nuovi valori per il passo successivo *)
                        expectedA = currentB;
                        expectedB = remainder;
                        isCompleted = True;  (* Segna l'algoritmo come completato *)
                      ,
                        errorMessage = "Risposta errata. Riprova."  (* Se la risposta \[EGrave] errata, mostra un errore *)
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
    
    (* Definizioni per la finestra del dialogo *)
    WindowTitle -> "Algoritmo di Euclide",  (* Titolo della finestra *)
    Modal -> True  (* Modalit\[AGrave] del dialogo, obbliga l'utente a interagire prima di chiudere *)
  ]
 ]

(* Fine del contesto privato *)
End[]

(* Fine del pacchetto *)
EndPackage[]



