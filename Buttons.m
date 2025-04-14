BeginPackage["Buttons`"]

Restart::usage = 
  "Restart[position_, dice_, gameOver_] crea il bottone per ricominciare il gioco.";

ClearFields::usage = 
  "ClearFields[quotient_, remainder_, message_] restituisce un bottone che ripulisce i campi.";

Begin["`Private`"]

SetAttributes[Restart, HoldAll];

Restart[position_, dice_, gameOver_] := Button[
  "Ricomincia", 
  (
    position = 1; 
    dice = 0; 
    gameOver = False;
  )
]

SetAttributes[ClearFields, HoldAll]
ClearFields[quotient_, remainder_, message_] := Button[
  "Pulisci Campi",
  (
    quotient = "";
    remainder = "";
    message = "";
  )
]

End[]
EndPackage[]
