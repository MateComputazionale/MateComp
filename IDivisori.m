(* ::Package:: *)

(* :Title: GCD Euclide*)
(* :Context: TODO*)
(* :Author: Gruppo 8: Sara Casadio, Enrico Ferraiolo, \
Federica Santisi*)
(* :Summary: TODO*)
(* :Copyright: TODO*)
(* :Package Version: 1.0*)
(* :Mathematica Version: TODO*)
(* :History: last modified TODO*)
(* :Keywords: TODO*)
(* :Sources: TODO*)
(* :Limitations: \
this is a preliminary version,for educational purposes only.*)
(* :Discussion: TODO*)
(* :Requirements: TODO*)
(* :Warnings: TODO*)



BeginPackage["IDivisori`"]

(*Qui dichiari le funzioni pubbliche e,se vuoi,eventuali messaggi*)
StartGame::usage = 
  "StartGame[] avvia il Gioco dell'Oca con l'Algoritmo di Euclide.";

Begin["`Private`"]

(*Definisci la funzione che incapsula il codice del tuo gioco*)
StartGame[___] := 
  Module[{seed, finalPos, 
    boardColors, numObstacles, obstacles, isBlocked},(*1. Prompt per il seed*)
   seed = DialogInput[
     Column[{"Inserisci il numero seed per il gioco:", 
       InputField[Dynamic[seedInput], Number], 
       DefaultButton[DialogReturn[seedInput]]}], 
     WindowTitle -> "Seed del Gioco"];
   If[NumericQ[seed],(*Se il seed \[EGrave] numerico,
    imposta il seed e definisci il tabellone*)SeedRandom[seed];
    cols = 7;
    rows = 9;
    totalCells = cols*rows;
    finalPos = totalCells;
    (*Genera colori casuali per le caselle*)
    boardColors = Table[RandomColor[], {totalCells}];
    (*Genera ostacoli casuali*)numObstacles = Round[totalCells*0.15];
    obstacles = RandomSample[Range[2, totalCells - 1], numObstacles];
    (*Funzione per controllare se una casella \[EGrave] bloccata*)
    isBlocked[pos_] := MemberQ[obstacles, pos];
    (*Creazione della griglia grafica*)
    boardPrimitives = 
     Flatten[Table[Module[{i, col, row, x, y}, i = cell;
        row = Quotient[i - 1, cols] + 1;
        col = Mod[i - 1, cols] + 1;
        x = col - 1;
        y = row - 1;
        {EdgeForm[Black], 
         FaceForm[If[isBlocked[i], Gray, boardColors[[i]]]], 
         Rectangle[{x, y}, {x + 1, y + 1}], 
         If[isBlocked[i], 
          Text[Style["X", 14, Bold, Red], {x + 0.5, y + 0.5}], 
          Text[Style[ToString[i], 8], {x + 0.5, y + 0.5}]]}], {cell, 
        1, totalCells}], 1];
    (*2. Creazione della finestra del gioco*)
    CreateDocument[
     DynamicModule[{position = 1, dice = 0, gameOver = False}, 
      Column[{Style["Gioco dell'Oca", Bold, 16], 
        Dynamic@Graphics[{boardPrimitives, 
           Dynamic[If[
             position <= totalCells, {Red, 
              Disk[{Mod[position - 1, cols] + 0.5, 
                Quotient[position - 1, cols] + 0.5}, 0.3]}, {}]]}, 
          PlotRange -> {{0, cols}, {0, rows}}, ImageSize -> 400], 
        Button["Tira il dado", dice = RandomInteger[{1, 6}];
         (*Algoritmo di Euclide*)
         Module[{a, b}, a = RandomInteger[{10, 99}];
          b = RandomInteger[{1, a - 1}];
          
          CreateDialog[
           DynamicModule[{localA = a, localB = b, steps = {}, 
             stepCount = dice, currentStep = 1, locQuotient = "", 
             locRemainder = "", locMessage = ""}, 
            Column[{Row[{"Risolvi l'Algoritmo di Euclide per ", 
                localA, " e ", localB}], 
              Dynamic[If[localB == 0, 
                Column[{Style[
                   "Algoritmo completato! Il MCD \[EGrave] " <> 
                    ToString[localA], Bold, 14], 
                  Button["Avanza di " <> ToString[dice] <> " caselle",
                    DialogReturn[]; position += dice]}], 
                Column[{Row[{"Passo ", currentStep, ": ", localA, 
                    " div ", localB}], 
                  Row[{"Quoziente: ", 
                    InputField[Dynamic[locQuotient], String, 
                    FieldSize -> 5]}], 
                  Row[{"Resto: ", 
                    InputField[Dynamic[locRemainder], String, 
                    FieldSize -> 5]}], Dynamic[Style[locMessage, Red]],
                   Button["Verifica", 
                   Module[{q, r}, q = ToExpression[locQuotient];
                    r = ToExpression[locRemainder];
                    
                    If[! NumericQ[q] || ! NumericQ[r], 
                    locMessage = "Inserisci numeri validi.", 
                    If[q == Quotient[localA, localB] && 
                    r == Mod[localA, localB], 
                    AppendTo[steps, {localA, localB, q, r}];
                    currentStep++;
                    locMessage = "Corretto!";
                    {localA, localB} = {localB, r};
                    locQuotient = "";
                    locRemainder = "";
                    
                    If[currentStep > stepCount, DialogReturn[]; 
                    position += dice], 
                    locMessage = "Risposta errata. Riprova."]]]]}]]]}]],
            WindowTitle -> "Algoritmo di Euclide", Modal -> True]], 
         Enabled -> Dynamic[! gameOver]], 
        Dynamic[If[gameOver, "Hai vinto!", 
          "Ultimo lancio: " <> ToString[dice]]], 
        Spacer[10],(*Pulsante per riavviare il gioco,ad es.:
        si pu\[OGrave] abilitare quando il gioco \[EGrave] terminato*)
        Dynamic[If[gameOver, 
          Button["Nuova Partita", position = 1; gameOver = False;], ""]]},
        Alignment -> Center, Spacings -> 2]], 
     WindowTitle -> "Gioco dell'Oca"];
    (*Se il valore inserito non \[EGrave] numerico*), 
    Print["Il valore inserito non \[EGrave] valido."]];];

End[]  (*Fine della sezione privata*)
EndPackage[]  (*Fine del pacchetto*)
