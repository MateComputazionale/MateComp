(* Pacchetto Euclide per calcolare il MCD tramite l'algoritmo di Euclide con interfaccia utente *)

BeginPackage["Euclide`"]

(* Definizione dell'uso pubblico del simbolo EuclideComponent *)
EuclideComponent::usage =
  "EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] crea un componente per calcolare il GCD usando l'algoritmo di Euclide, mostrando il calcolo passo per passo.";

Begin["`Private`"]

(* Importa il file Buttons.m contenente definizioni di bottoni riutilizzabili *)
Get["Buttons.m"];
(*
Restart::usage = "Restart[position_, dice_, gameOver_, seed_, showEuclide_, diceButtonEnabled_] crea il bottone per ricominciare il gioco.";
RollDice::usage = "RollDice[diceButtonEnabled_, diceValue_, num1_, num2_, euclideComponent_, playerPosition_, obstaclesList_, totalBoardCells_, isGameOver_, showEuclide_] crea il bottone per tirare il dado.";
ClearFields::usage = "ClearFields[quotient_, remainder_, message_] restituisce un bottone che ripulisce i campi.";
*)

(* Importa il file Aiuto.m contenente la funzione di aiuto visuale *)
Get["Aiuto.m"];
(*
MostraAiuto::usage =
  "MostraAiuto[a_Integer, b_Integer] mostra la divisione a = q*b + r come gruppi di aiuto colorate in base al valore (blu=10, arancione=1).";
*)

(* Importa il file Suggerimento.m contenente suggerimenti per ogni passo *)
Get["Suggerimento.m"];
(*
MostraSuggerimento::usage =
  "MostraSuggerimento[a_Integer, b_Integer] mostra un passo dell'algoritmo di Euclide con cerchi colorati, etichette e passaggi successivi."
*)

(* Ridefinizione di ClearFields per uniformità di stile del pacchetto *)
ClearFields[quotient_, remainder_, message_] :=
  Button["Pulisci campi",
    (quotient = ""; remainder = ""; message = ""),
    ImageSize -> {370, 30},         (* Dimensioni del bottone *)
    BaseStyle -> {FontWeight -> Bold} (* Stile di base: grassetto *)
  ]

(* Definizione principale di EuclideComponent, con argomenti:
   a, b: numeri interi iniziali
   stepsToComplete: numero di caselle da avanzare al termine
   onSuccessCallback: funzione da eseguire al completamento *)
EuclideComponent[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] :=
 DynamicModule[{  (* Variabili locali dinamiche *)
   currentA = a,                (* Valore corrente di a *)
   currentB = b,                (* Valore corrente di b *)
   steps = {},                  (* Lista dei passi effettuati *)
   currentStep = 1,             (* Contatore dei passi *)
   userQuotient = "",         (* Quoziente inserito dall'utente *)
   userRemainder = "",        (* Resto inserito dall'utente *)
   errorMessage = "",         (* Messaggio di errore valido *)
   isCompleted = False,         (* Flag di passo completato *)
   nextA = "",                (* Nuovo valore di a dopo completamento di un passo *)
   nextB = "",                (* Nuovo valore di b dopo completamento di un passo *)
   userMCD = "",              (* MCD inserito dall'utente (non usato qui) *)
   mcdMessage = "",           (* Messaggio per il MCD (non usato) *)
   showHelp = False,            (* Se mostrare la sezione di aiuto grafico *)
   helpContent = Null,          (* Contenuto generato da MostraAiuto *)
   showSuggestion = False,      (* Se mostrare il suggerimento testuale *)
   suggestionContent = Null,    (* Contenuto generato da MostraSuggerimento *)
   panelWidth = 400,            (* Larghezza del pannello *)
   buttonWidth = 370            (* Larghezza dei bottoni *)
 },

  (* Struttura del pannello principale *)
  Panel[
   Column[{  (* Disposizione verticale dei contenuti *)
     Style["Algoritmo di Euclide", Bold, 14, TextAlignment -> Center],
     Dynamic[ (* Rende reattivo il contenuto interno *)
      Column[
       Join[
        (* Mostra tutti i passi già effettuati *)
        Table[
         With[{stepData = steps[[i]]},
          Row[{(* Rappresentazione testuale del passo i *)
            Style[Row[{"Passo ", i}], Bold],
            "     a = ", stepData[[1]],
            " b = ", stepData[[2]],
            "     ", stepData[[1]], " div ", stepData[[2]],
            " = ", stepData[[3]], ", resto ", stepData[[4]]
          }]
         ],
         {i, Length[steps]}
        ],

        (* Verifica se l'algoritmo è completato *)
        If[currentB === 0,
         { (* Caso completato: mostra il MCD e il bottone di avanzamento *)
          Style["Algoritmo completato! Il MCD \[EGrave] " <> ToString[currentA], Bold, 14, TextAlignment -> Center],
          Button["Avanza di " <> ToString[stepsToComplete] <> " caselle",
           onSuccessCallback[currentA],  (* Chiamata al callback *)
           ImageSize -> {buttonWidth, 30},
           BaseStyle -> {FontWeight -> Bold}
          ]
         },

         (* Altrimenti, gestisce sia il passo corrente sia la fase di inserimento dei nuovi valori *)
         If[isCompleted,
          { (* Dopo un passo valido, richiede i nuovi a e b *)
           Style["Inserisci i nuovi valori per a e b per continuare:", Bold, TextAlignment -> Center],
           Row[{(* Campi di Input per nextA e nextB *)
             "a = ", InputField[Dynamic[nextA], Number, FieldSize -> 5],
             "   ",
             "b = ", InputField[Dynamic[nextB], Number, FieldSize -> 5]
           }, Alignment -> Center],
           Dynamic[Style[errorMessage, Red, TextAlignment -> Center]],
           Column[{(* Bottoni per pulire, aiuto e verifica nuova coppia *)
             Button["Pulisci campi",
              (nextA = ""; nextB = ""; errorMessage = ""),
              ImageSize -> {buttonWidth, 30},
              BaseStyle -> {FontWeight -> Bold}
             ],
             Dynamic[If[!showHelp,
               Button[Dynamic[If[showSuggestion, "Nascondi aiuto", "Aiuto"]],
                showSuggestion = !showSuggestion;
                If[showSuggestion, suggestionContent = MostraSuggerimento[currentA, currentB]],
                ImageSize -> {buttonWidth, 30},
                BaseStyle -> {FontWeight -> Bold}
               ],
               ""
             ]],
             Button["Verifica",
              If[nextA === currentB && nextB === Mod[currentA, currentB],
               (* Se corretto, aggiorna currentA e currentB, resetta variabili *)
               currentA = nextA;
               currentB = nextB;
               userQuotient = "";
               userRemainder = "";
               nextA = "";
               nextB = "";
               errorMessage = "";
               isCompleted = False;
               showHelp = False;
               showSuggestion = False;
               ,
               (* Altrimenti mostra errore *)
               errorMessage = "Risposta errata! Riprova."
              ],
              ImageSize -> {buttonWidth, 30},
              BaseStyle -> {FontWeight -> Bold}
             ]
           }, Spacings -> 1, Alignment -> Center]
          },

          { (* Fase di inserimento di quoziente e resto per il passo corrente *)
           Row[{(* Mostra dati passo corrente e campi di input *)
             Style[Row[{"Passo ", currentStep}], Bold],
             "    a = ", currentA,
             ", b = ", currentB,
             "     ", currentA, " div ", currentB,
             " = ", InputField[Dynamic[userQuotient], Number, FieldSize -> 5],
             ", resto ",
             InputField[Dynamic[userRemainder], Number, FieldSize -> 5]
           }, Alignment -> Center],

           Dynamic[Style[errorMessage, Red, TextAlignment -> Center]],

           Column[{(* Bottoni di pulizia, aiuto/suggerimento e verifica *)
             ClearFields[userQuotient, userRemainder, errorMessage],

             Dynamic[
              Which[
               showHelp,
                Button["Nascondi aiuto",
                 showHelp = False;
                 helpContent = Null,
                 ImageSize -> {buttonWidth, 30},
                 BaseStyle -> {FontWeight -> Bold}
                ],
               showSuggestion,
                Button["Nascondi suggerimento",
                 showSuggestion = False;
                 suggestionContent = Null,
                 ImageSize -> {buttonWidth, 30},
                 BaseStyle -> {FontWeight -> Bold}
                ],
               True,
                Column[{(* Bottone per richiedere aiuto *)
                 Button["Aiuto",
                  showHelp = True;
                  helpContent = MostraAiuto[currentA, currentB];
                  showSuggestion = False,
                  ImageSize -> {buttonWidth, 30},
                  BaseStyle -> {FontWeight -> Bold}
                 ]
                }, Alignment -> Center]
              ]
             ],

             Button["Verifica",
              If[userQuotient === Quotient[currentA, currentB] &&
                 userRemainder === Mod[currentA, currentB],
               (* Se corretto, memorizza il passo *)
               AppendTo[steps, {currentA, currentB, userQuotient, userRemainder}];
               currentStep++;
               If[Mod[currentA, currentB] === 0,
                (* Se il resto è zero, termina algoritmo *)
                currentB = 0; (* Forza completamento *)
               ,
                isCompleted = True;
               ];
               userQuotient = "";
               userRemainder = "";
               errorMessage = "";
               showHelp = False;
               showSuggestion = False;
               ,
               (* Altrimenti reset input e mostra errore *)
               errorMessage = "Risposta errata! Riprova.";
               userQuotient = ""; userRemainder = "";
              ],
              ImageSize -> {buttonWidth, 30},
              BaseStyle -> {FontWeight -> Bold}
             ]
           }, Spacings -> 1, Alignment -> Center]
          }
         ]
        ],

        (* Se richiesto, visualizza il contenuto di aiuto sotto i bottoni *)
        If[showHelp && helpContent =!= Null,
          {Spacer[5], helpContent},
          {}
        ],

        (* Se richiesto, visualizza il suggerimento nato da Suggerimento.m *)
        If[showSuggestion && suggestionContent =!= Null && !showHelp,
          {Spacer[5], suggestionContent},
          {}
        ]
       ],
       Alignment -> Center
      ]
     ]
    }, Spacings -> 1, Alignment -> Center],
   ImageMargins -> 10,
   ImageSize -> panelWidth (* Imposta larghezza fissa del pannello *)
  ]
 ]

(* Creazione di un Dialog con componente EuclideComponent *)
EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] :=
 CreateDialog[
  EuclideComponent[a, b, stepsToComplete, onSuccessCallback],
  WindowTitle -> "Algoritmo di Euclide", (* Titolo della finestra *)
  Modal -> True                    (* Modale: blocca interazione col resto *)
 ]

End[]
EndPackage[]
