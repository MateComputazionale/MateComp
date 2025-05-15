(* ::Package:: *)

(* Inizio della definizione del pacchetto "Aiuto" *)
BeginPackage["Aiuto`"]

(* Definizione della documentazione per la funzione principale esportata *)
MostraAiuto::usage =
"MostraAiuto[a_Integer, b_Integer] mostra la divisione a = q*b + r come gruppi di aiuto colorate in base al valore (blu=10, arancione=1).";

(* Inizio del contesto privato del pacchetto per nascondere le funzioni di supporto *)
Begin["`Private`"]

(* 
  Funzione di supporto che crea una rappresentazione visiva di un numero intero
  Parametri:
  - n_Integer: il numero da rappresentare visivamente
  - radius_: raggio dei dischi (opzionale, default 0.2)
  
  Restituisce un array di oggetti Graphics che rappresentano il valore:
  - Dischi blu per le decine (valore=10)
  - Dischi arancioni per le unit\[AGrave] (valore=1)
*)
CreaAiutoValore[n_Integer, radius_: 0.2] := Module[
 {blu, arancioni},
 
 (* Calcola quante decine (dischi blu) servono *)
 blu = Quotient[n, 10];
 
 (* Calcola quante unit\[AGrave] (dischi arancioni) servono *)
 arancioni = Mod[n, 10];
 
 (* Unisce le due liste di dischi colorati *)
 Join[
   (* Crea 'blu' dischi di colore blu per rappresentare le decine *)
   Table[
     Graphics[{EdgeForm[Black], FaceForm[Blue], Disk[{0, 0}, radius]}, ImageSize -> 30],
     {blu}
   ],
   
   (* Crea 'arancioni' dischi di colore arancione per rappresentare le unit\[AGrave] *)
   Table[
     Graphics[{EdgeForm[Black], FaceForm[Orange], Disk[{0, 0}, radius]}, ImageSize -> 30],
     {arancioni}
   ]
 ]
];

(* 
  Funzione principale che visualizza la divisione a = q*b + r graficamente
  Parametri:
  - a_Integer: dividendo (numero da dividere)
  - b_Integer: divisore
  
  Crea un pannello con una rappresentazione visiva della divisione
  mostrando i gruppi di dimensione b e il resto
*)
MostraAiuto[a_Integer, b_Integer] := Module[
 {
   q = Quotient[a, b], (* Calcola il quoziente *)
   r = Mod[a, b],      (* Calcola il resto *)
   gruppi, resto, radius = 0.2 (* Raggio dei dischi per la visualizzazione *)
 },
 
 (* Prepara la visualizzazione del divisore ripetuto q volte *)
 gruppi = {
   Row[CreaAiutoValore[b, radius], Spacer[5] ],  (* Rappresentazione visiva di un gruppo di b *)
   Style["x " <> ToString[q] <> " gruppi", Italic, Gray]  (* Etichetta che indica quanti gruppi ci sono *)
 };
 
 (* Crea la rappresentazione visiva del resto *)
 resto = CreaAiutoValore[r, radius];
 
 (* Assembla tutti gli elementi in un pannello ben formattato *)
 Panel[
   Column[{
     Style["Suggerimento", Bold, 14],  (* Titolo del pannello *)
     
     (* Testo esplicativo con i valori numerici evidenziati *)
     Row[{
       "Il risultato \[EGrave] ",
       Style[ToString[q], Bold],
       " con resto ",
       Style[ToString[r], Bold],
       "."
     }],
     
     (* Spiegazione testuale della divisione *)
	Row[{
    "Immagina di divere il numero totale di palline ", Style[a, Bold],
    "(pari al dividendo) nel maggiore numero di gruppo possibili di ", Style[b, Bold],
    "palline (divisore). Il numero di gruppi formatosi di ", Style[b, Bold],
    "palline \[EGrave] il risultato della divisione. Le palline rimanenti dalla divisione in gruppi uguali \[EGrave] il resto"
    }]
     (* Visualizzazione dei gruppi (il divisore ripetuto q volte) *)
     Column[gruppi],
     
     (* Visualizzazione del resto, solo se \[EGrave] maggiore di zero *)
     If[r > 0,
       Row[{"Resto: ", Row[resto]}],
       Nothing  (* Non mostra nulla se il resto \[EGrave] zero *)
     ]
   }],
   ImageMargins -> 10  (* Aggiunge margini al pannello per una migliore visualizzazione *)
 ]
];

(* Fine del contesto privato *)
End[]

(* Fine della definizione del pacchetto *)
EndPackage[]









