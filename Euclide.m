(* ::Package:: *)

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

(* Ridefinizione di ClearFields per uniformit\[AGrave] di stile del pacchetto *)
ClearFields[numero1, numero2, message_] :=
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

  (* Struttura principale *)
  Panel[
   Column[{  (* Disposizione verticale dei contenuti *)
     Style["Algoritmo di Euclide", Bold, 14, TextAlignment -> Center], (* Titolo del Panel principale*)
     Dynamic[ (*Permette dinamicit\[AGrave] nel contenuto del Panel pricipale*)
      Column[
       Join[ 
        (* Mostra tutti i passi gi\[AGrave] effettuati *)
        Table[
         With[{stepData = steps[[i]]},
          Row[{(* Rappresentazione testuale del passo i *)
            Style[Row[{"Passo ", i}], Bold],
            "     a = ", stepData[[1]], (* Valore corrente di a *)
            " b = ", stepData[[2]], (* Valore corrente di b *)
            "     ", stepData[[1]], " div ", stepData[[2]], (* Divisione *)
            " = ", stepData[[3]], ", resto ", stepData[[4]] (* Resto *)
          }]
         ],
         {i, Length[steps]} (* Itera su tutti i passi *)
        ],

        (* Verifica se l'algoritmo \[EGrave] completato controllando che il resto della divisione sia 0*)
        If[currentB === 0,
         { (* Caso completato: mostra MCD e il bottone di avanzamento *)
          Style["Algoritmo completato! Il MCD \[EGrave] " <> ToString[currentA], Bold, 14, TextAlignment -> Center],
          Button["Avanza di " <> ToString[stepsToComplete] <> " caselle",
          (* Quando premi il bottone viene chiamata la callback che notifica al file i Divisori di dover avanzare nella board *)
           onSuccessCallback[currentA],
           ImageSize -> {buttonWidth, 30}, (* Dimensioni del bottone *)
           BaseStyle -> {FontWeight -> Bold} (* Stile di base: grassetto *)
          ]
         },

         (* Altrimenti, gestisce sia il passo corrente sia la fase di inserimento dei nuovi valori che la fase di inserimento del risultato della 
         divisione e del resto della divisone *)
         
         (*Controllo che il passo precedente sia stato completato correttamente, se \[EGrave] stato completamento correttamente allora posso procedere;
         richiedendo in input i valori necessari per completare il prossimo passo dell'algoritmo*)
         If[isCompleted,
         
          { (* Viene richiesto all'utente di inserire in input il nuovo dividendo e il nuovo divisore per il successivo passo dell'algoritmo*)
           Style["Inserisci i nuovi valori a e b per continuare nella ricerca dell'MCD:", Bold, TextAlignment -> Center],
           Row[{
          (* Campi di input per inserire i prossimi valori di a e b ovvero il prossimo dividendo e il prossimo divisore divisore *)
             "a = ", InputField[Dynamic[nextA], Number, FieldSize -> 5], (* Campo di input per a *)
             "   ",
             "b = ", InputField[Dynamic[nextB], Number, FieldSize -> 5] (* Campo di input per b *)
           }, Alignment -> Center],
           
           Dynamic[Style[errorMessage, Red, TextAlignment -> Center]],
           
           Column[{
          
          (* Lista dei bottoni per pulire i campi, l'aiuto visivo e la verifica dei nuovi valori inseriti*)
             
             (*Bottoni per ripulire i campi in input*)
             ClearFields[nextA, nextB, errorMessage],

            (* Bottone per mostrare l'aiuto visivo nel caso in cui l'utente non sappia quali sono i nuovi valori per a e b da inserire*)
             Dynamic[If[!showHelp, 
               Button[Dynamic[If[showSuggestion, "Nascondi aiuto", "Aiuto"]],
                showSuggestion = !showSuggestion;
                If[showSuggestion, suggestionContent = MostraSuggerimento[currentA, currentB]],
                ImageSize -> {buttonWidth, 30},
                BaseStyle -> {FontWeight -> Bold}
               ],
               ""
             ]],

			(*Bottone per verificare che i valori inseriti dall'utente siano corretti*)
             Button["Verifica",
              If[nextA === currentB && nextB === Mod[currentA, currentB],
               (* Se corretto, aggiorna currentA e currentB con i nuovi valori di a e b per il prossimo passo dell'algoritmo;
               Oltre ai nuovi valori di a e b resetto tutti i valori precedenti *)
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
               
               (* Se i valori inseriti dall'utente non sono corretti viene mostrato il messaggio di errore*)
               errorMessage = "I numeri inseriti non sono corretti! Prova a reinserire il prossimo a e il prossimo b per completare l''algoritmo"
              ],
              ImageSize -> {buttonWidth, 30},
              BaseStyle -> {FontWeight -> Bold}
             ]
           }, Spacings -> 1, Alignment -> Center]
          },


          { (* Fase di inserimento di quoziente e resto per il passo corrente *)
           Row[{(* Mostra il divisore e il dividendo del passo corrente e i campi di input dove inserire il risultato e il resto della divisione *)
             Style[Row[{"Passo ", currentStep}], Bold],
             "    a = ", currentA, (* Valore corrente di a *)
             ", b = ", currentB,  (* Valore corrente di b *)
             "     ", currentA, " div ", currentB,
             " = ", InputField[Dynamic[userQuotient], Number, FieldSize -> 5], 
             ", resto ",
             InputField[Dynamic[userRemainder], Number, FieldSize -> 5]
           }, Alignment -> Center],

          (* Mostra errore se presente *)
           Dynamic[Style[errorMessage, Red, TextAlignment -> Center]],

           Column[{
            (* Lista dei bottoni per pulire i campi, l'aiuto visivo e la verifica dei nuovi valori inseriti*)
            
            (*Bottoni per ripulire i campi in input*)
             ClearFields[userQuotient, userRemainder, errorMessage],

			
			(* Bottone per mostrare l'aiuto visivo nel caso in cui l'utente non sappia quali sono il risultato e il resto della divisione*)
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
                Column[{
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
			
			(*Bottone per verificare che i valori inseriti dall'utente siano corretti*)
             Button["Verifica",
              If[userQuotient === Quotient[currentA, currentB] &&
                 userRemainder === Mod[currentA, currentB],
               (* Se corretto, memorizza il passo in modo da poterlo mostrare successivamente nella Table *)
               AppendTo[steps, {currentA, currentB, userQuotient, userRemainder}];
               currentStep++;
               If[Mod[currentA, currentB] === 0,
                (*Controllo che il resto della divisione sia 0; 
                Se \[EGrave] 0 allora l'algoritmo \[EGrave] terminato*)
                currentB = 0;
               ,
                isCompleted = True;
               ];
               userQuotient = "";
               userRemainder = "";
               errorMessage = "";
               showHelp = False;
               showSuggestion = False;
               ,
               (* Se il risultato e il resto della divisione inseriti dall'utente non sono corretti, mostra il messaggio d'errore*)
               errorMessage = "I numeri inseriti non sono corretti! Prova a reinserire il quoziente e il resto della divisione corretti per poter completare l'algoritmo!";
               userQuotient = ""; userRemainder = "";
              ],
              ImageSize -> {buttonWidth, 30},
              BaseStyle -> {FontWeight -> Bold}
             ]
           }, Spacings -> 1, Alignment -> Center]
          }
         ]
        ],

        (* Se l'utente richiede l'aiuto per trovare il quoziente e il resto della divisione, l'aiuto viene mostrato sotto il Panel con i passi dell'algoritmo *)
        If[showHelp && helpContent =!= Null,
          {Spacer[5], helpContent},
          {}
        ],

        (* Se l'utente richiede l'aiuto per trovare i prossimi a e il prossimo b, l'aiuto viene mostrato sotto il Panel con i passi dell'algoritmo*)
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

(* Creazione del Dialog principale contenente la risoluzione dell'algoritmo di euclide*)
EuclideDialog[a_Integer, b_Integer, stepsToComplete_Integer, onSuccessCallback_Function] :=
 CreateDialog[
  EuclideComponent[a, b, stepsToComplete, onSuccessCallback],
  WindowTitle -> "Algoritmo di Euclide", (* Titolo della finestra *)
  Modal -> True                    (* Modale: blocca interazione col resto *)
 ]

End[]
EndPackage[]



