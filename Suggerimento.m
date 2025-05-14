(* Pacchetto Suggerimento per visualizzare un passo dell'algoritmo di Euclide con nodi grafici colorati *)

BeginPackage["Suggerimento`"]

(* Uso pubblico della funzione MostraSuggerimento *)
MostraSuggerimento::usage =
  "MostraSuggerimento[a_Integer, b_Integer] mostra un passo dell'algoritmo di Euclide con cerchi colorati, etichette e passaggi successivi.";

Begin["`Private`"]

(*
  CreaNodoGrafico:
  - val: valore numerico da mostrare nel cerchio
  - colore: colore di riempimento del cerchio
  - label: etichetta opzionale da posizionare sotto il cerchio
  Restituisce un oggetto Graphics con:
    - un Disk (cerchio) con bordo nero e riempimento del colore specificato
    - il valore val al centro del cerchio
    - un testo secondario opzionale sotto il cerchio
*)
CreaNodoGrafico[val_, colore_, label_: ""] := 
  Graphics[{  
    EdgeForm[Black],         (* Bordo nero *)
    FaceForm[colore],        (* Riempimento colorato *)
    Disk[{0, 0}, 0.1],       (* Cerchio di raggio 0.1 centrato in {0,0} *)
    Text[Style[ToString[val], 10, Black], {0, 0}],  (* Valore al centro *)
    If[label =!= "",     (* Se è specificata un'etichetta *)
      Text[Style[label, 8, Black], {0, -0.18}],  (* Etichetta sotto il cerchio *)
      Nothing          (* Altrimenti nessun elemento aggiuntivo *)
    ]
  }, ImageSize -> 40]     (* Dimensione complessiva dell'immagine *)

(*
  MostraSuggerimento:
  - a, b: valori interi correnti nell'algoritmo di Euclide
  Calcola:
    - r = a mod b (resto)
    - q = Quotient[a, b] (quoziente) [non mostrato esplicitamente ma può servire]
  Crea nodi grafici per A, B, r e per i valori del passo successivo.
  Restituisce un Panel con:
    - Titolo "Suggerimento"
    - Informazioni testuali sul prossimo A e prossimo B
    - Visualizzazione dei nodi A, B e resto attuale
    - Visualizzazione dei nodi per i valori successivi
*)
MostraSuggerimento[a_Integer, b_Integer] := Module[  
  { r = Mod[a, b],               (* Calcola resto *)
    q = Quotient[a, b],          (* Calcola quoziente *)
    nodoA, nodoB, nodoR,         (* Nodi per valori correnti *)
    nodoNextA, nodoNextB         (* Nodi per valori successivi *)
  },
  
  (* Crea i tre nodi correnti con etichette *)
  nodoA = CreaNodoGrafico[a, LightGray, "A"];      (* Nodo per A *)
  nodoB = CreaNodoGrafico[b, Yellow, "B"];         (* Nodo per B *)
  nodoR = CreaNodoGrafico[r, LightBlue, "A mod B"];(* Nodo per resto *)
  
  (* Crea i nodi per i valori del prossimo passo *)
  nodoNextA = CreaNodoGrafico[b, Yellow, "Prossimo A"];    (* Il vecchio B diventa nuovo A *)
  nodoNextB = CreaNodoGrafico[r, LightBlue, "Prossimo B"]; (* Il resto diventa nuovo B *)
  
  (* Costruisce un Panel di presentazione *)
  Panel[
    Column[{                                      (* Layout verticale *)
      Style["Suggerimento", Bold, 14],           (* Titolo in grassetto *)
      Row[{                                     
        "Il prossimo A \[EGrave] ",         (* Testo descrittivo *)
        Style[b, Bold],                         (* Valore di B in grassetto *)
        " e il prossimo B \[EGrave] ",        
        Style[r, Bold]                          (* Valore del resto in grassetto *)
      }],
      
      Style["Passo Corrente:", Bold, 12],       (* Sottotitolo passo corrente *)
      Row[{nodoA, Spacer[10], nodoB, Spacer[10], nodoR}],  (* Visualizzazione nodi correnti *)
      
      Style["Prossimo passo:", Bold, 12],       (* Sottotitolo passo seguente *)
      Row[{"Il prossimo A \[EGrave] il vecchio B", Spacer[10], nodoNextA}],
      Row[{"Il prossimo B \[EGrave] A mod B", Spacer[10], nodoNextB}]
    }, Spacings -> 1],                           (* Spaziatura interna *)
    ImageMargins -> 10                           (* Margini esterni del Panel *)
  ]
];

End[]
EndPackage[]
