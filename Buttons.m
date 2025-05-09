BeginPackage["Buttons`"]

(* Dichiarazione dei simboli esportati dal pacchetto *)
Restart::usage = "Restart[position_, dice_, gameOver_, seed_, showEuclide_, diceButtonEnabled_] crea il bottone per ricominciare il gioco.";
ClearFields::usage = "ClearFields[quotient_, remainder_, message_] restituisce un bottone che ripulisce i campi.";

Begin["`Private`"]

(* Imposta gli attributi per trattenere gli argomenti senza valutarli *)
SetAttributes[ClearFields, HoldAll]
ClearFields[quotient_, remainder_, message_] := Button[
  "Pulisci Campi",
  (
    quotient = "";
    remainder = "";
    message = "";
  )
]

(* Funzione per il bottone Ricomincia con lo stesso stile visuale del bottone originale *)
(* Imposta gli attributi per trattenere gli argomenti senza valutarli *)
SetAttributes[Restart, HoldAll]
Restart[position_, dice_, gameOver_, seed_, showEuclide_, diceButtonEnabled_] := Button[
  TextCell["Ricomincia", "Text", FontColor -> White],
  (
    SeedRandom[seed];
    position = 1;
    dice = 0;
    gameOver = False;
    showEuclide = False;
    diceButtonEnabled = True;
  ),
  Background -> RGBColor[0.2, 0.6, 0.8],
  FrameMargins -> 10,
  Appearance -> None,
  BaseStyle -> {
    FontSize -> 14,
    FontColor -> White,
    FontWeight -> "Bold",
    FontFamily -> "Arial"
  },
  ImageSize -> {120, Automatic},
  Method -> "Queued",
  ContentPadding -> 10,
  RoundingRadius -> 8,
  BoxShadow -> {0, 2, 4, GrayLevel[0.5]}
]

End[]

EndPackage[]