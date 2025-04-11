(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.2' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[     24193,        549]
NotebookOptionsPosition[     23833,        535]
NotebookOutlinePosition[     24233,        551]
CellTagsIndexPosition[     24190,        548]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{":", "Title", ":", " ", 
    RowBox[{"GCD", " ", "Euclide"}]}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Context", ":", " ", "TODO"}], "*)"}], "\[IndentingNewLine]", 
  
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{":", "Author", ":", " ", 
     RowBox[{"Gruppo", " ", "8"}], ":", " ", 
     RowBox[{"Sara", " ", "Casadio"}]}], ",", " ", 
    RowBox[{"Enrico", " ", "Ferraiolo"}], ",", " ", 
    RowBox[{"Federica", " ", "Santisi"}]}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Summary", ":", " ", "TODO"}], "*)"}], "\[IndentingNewLine]", 
  
  RowBox[{"(*", " ", 
   RowBox[{":", "Copyright", ":", " ", "TODO"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", 
    RowBox[{"Package", " ", "Version"}], ":", " ", "1.0"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", 
    RowBox[{"Mathematica", " ", "Version"}], ":", " ", "TODO"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "History", ":", " ", 
    RowBox[{"last", " ", "modified", " ", "TODO"}]}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Keywords", ":", " ", "TODO"}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Sources", ":", " ", "TODO"}], "*)"}], "\[IndentingNewLine]", 
  
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{":", "Limitations", ":", " ", 
     RowBox[{
     "this", " ", "is", " ", "a", " ", "preliminary", " ", "version"}]}], ",", 
    RowBox[{"for", " ", "educational", " ", "purposes", " ", 
     RowBox[{"only", "."}]}]}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Discussion", ":", " ", "TODO"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Requirements", ":", " ", "TODO"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Warnings", ":", " ", "TODO"}], "*)"}], "\[IndentingNewLine]",
   "\[IndentingNewLine]", "\[IndentingNewLine]", "\[IndentingNewLine]", 
  RowBox[{
   RowBox[{"BeginPackage", "[", "\"\<GCDEuclide`\>\"", "]"}], 
   "\[IndentingNewLine]", "\n", 
   RowBox[{"(*", 
    RowBox[{
     RowBox[{
     "Qui", " ", "dichiari", " ", "le", " ", "funzioni", " ", "pubbliche", " ",
       "e"}], ",", 
     RowBox[{"se", " ", "vuoi"}], ",", 
     RowBox[{"eventuali", " ", "messaggi"}]}], "*)"}], "\[IndentingNewLine]", 
   
   RowBox[{
    RowBox[{
     RowBox[{"StartGame", "::", "usage"}], 
     "=", "\"\<StartGame[] avvia il Gioco dell'Oca con l'Algoritmo di \
Euclide.\>\""}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], "\[IndentingNewLine]", 
   "\n", 
   RowBox[{"(*", 
    RowBox[{
    "Definisci", " ", "la", " ", "funzione", " ", "che", " ", "incapsula", " ",
      "il", " ", "codice", " ", "del", " ", "tuo", " ", "gioco"}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"StartGame", "[", "]"}], ":=", 
     RowBox[{"Module", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "seed", ",", "seedInput", ",", "cols", ",", "rows", ",", "totalCells",
          ",", "finalPos", ",", "boardColors", ",", "numObstacles", ",", 
         "obstacles", ",", "isBlocked", ",", "boardPrimitives", ",", 
         RowBox[{"dice", "=", "0"}], ",", 
         RowBox[{"position", "=", "1"}], ",", 
         RowBox[{"gameOver", "=", "False"}]}], "}"}], ",", 
       RowBox[{"(*", 
        RowBox[{"1.", " ", "Prompt", " ", "per", " ", "il", " ", "seed"}], 
        "*)"}], 
       RowBox[{
        RowBox[{"seed", "=", 
         RowBox[{"DialogInput", "[", 
          RowBox[{
           RowBox[{"Column", "[", 
            RowBox[{"{", 
             RowBox[{"\"\<Inserisci il numero seed per il gioco:\>\"", ",", 
              RowBox[{"InputField", "[", 
               RowBox[{
                RowBox[{"Dynamic", "[", "seedInput", "]"}], ",", "Number"}], 
               "]"}], ",", 
              RowBox[{"DefaultButton", "[", 
               RowBox[{"DialogReturn", "[", "seedInput", "]"}], "]"}]}], 
             "}"}], "]"}], ",", 
           RowBox[{"WindowTitle", "->", "\"\<Seed del Gioco\>\""}]}], "]"}]}],
         ";", "\[IndentingNewLine]", 
        RowBox[{"If", "[", 
         RowBox[{
          RowBox[{"NumericQ", "[", "seed", "]"}], ",", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{
            "Se", " ", "il", " ", "seed", " ", "\[EGrave]", " ", "numerico"}],
             ",", 
            RowBox[{
            "imposta", " ", "il", " ", "seed", " ", "e", " ", "definisci", " ",
              "il", " ", "tabellone"}]}], "*)"}], 
          RowBox[{
           RowBox[{"SeedRandom", "[", "seed", "]"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"cols", "=", "7"}], ";", "\[IndentingNewLine]", 
           RowBox[{"rows", "=", "9"}], ";", "\[IndentingNewLine]", 
           RowBox[{"totalCells", "=", 
            RowBox[{"cols", "*", "rows"}]}], ";", "\[IndentingNewLine]", 
           RowBox[{"finalPos", "=", "totalCells"}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "Genera", " ", "colori", " ", "casuali", " ", "per", " ", "le", " ",
              "caselle"}], "*)"}], 
           RowBox[{"boardColors", "=", 
            RowBox[{"Table", "[", 
             RowBox[{
              RowBox[{"RandomColor", "[", "]"}], ",", 
              RowBox[{"{", "totalCells", "}"}]}], "]"}]}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{"Genera", " ", "ostacoli", " ", "casuali"}], "*)"}], 
           RowBox[{"numObstacles", "=", 
            RowBox[{"Round", "[", 
             RowBox[{"totalCells", "*", "0.15"}], "]"}]}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"obstacles", "=", 
            RowBox[{"RandomSample", "[", 
             RowBox[{
              RowBox[{"Range", "[", 
               RowBox[{"2", ",", 
                RowBox[{"totalCells", "-", "1"}]}], "]"}], ",", 
              "numObstacles"}], "]"}]}], ";", "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "Funzione", " ", "per", " ", "controllare", " ", "se", " ", "una",
              " ", "casella", " ", "\[EGrave]", " ", "bloccata"}], "*)"}], 
           RowBox[{
            RowBox[{"isBlocked", "[", "pos_", "]"}], ":=", 
            RowBox[{"MemberQ", "[", 
             RowBox[{"obstacles", ",", "pos"}], "]"}]}], ";", 
           "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "Creazione", " ", "della", " ", "griglia", " ", "grafica"}], 
            "*)"}], 
           RowBox[{"boardPrimitives", "=", 
            RowBox[{"Flatten", "[", 
             RowBox[{
              RowBox[{"Table", "[", 
               RowBox[{
                RowBox[{"Module", "[", 
                 RowBox[{
                  RowBox[{"{", 
                   RowBox[{"i", ",", "col", ",", "row", ",", "x", ",", "y"}], 
                   "}"}], ",", 
                  RowBox[{
                   RowBox[{"i", "=", "cell"}], ";", "\[IndentingNewLine]", 
                   RowBox[{"row", "=", 
                    RowBox[{
                    RowBox[{"Quotient", "[", 
                    RowBox[{
                    RowBox[{"i", "-", "1"}], ",", "cols"}], "]"}], "+", 
                    "1"}]}], ";", "\[IndentingNewLine]", 
                   RowBox[{"col", "=", 
                    RowBox[{
                    RowBox[{"Mod", "[", 
                    RowBox[{
                    RowBox[{"i", "-", "1"}], ",", "cols"}], "]"}], "+", 
                    "1"}]}], ";", "\[IndentingNewLine]", 
                   RowBox[{"x", "=", 
                    RowBox[{"col", "-", "1"}]}], ";", "\[IndentingNewLine]", 
                   RowBox[{"y", "=", 
                    RowBox[{"row", "-", "1"}]}], ";", "\[IndentingNewLine]", 
                   RowBox[{"{", 
                    RowBox[{
                    RowBox[{"EdgeForm", "[", "Black", "]"}], ",", 
                    RowBox[{"FaceForm", "[", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"isBlocked", "[", "i", "]"}], ",", "Gray", ",", 
                    RowBox[{"boardColors", "[", 
                    RowBox[{"[", "i", "]"}], "]"}]}], "]"}], "]"}], ",", 
                    RowBox[{"Rectangle", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"x", ",", "y"}], "}"}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"x", "+", "1"}], ",", 
                    RowBox[{"y", "+", "1"}]}], "}"}]}], "]"}], ",", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"isBlocked", "[", "i", "]"}], ",", 
                    RowBox[{"Text", "[", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\<X\>\"", ",", "14", ",", "Bold", ",", "Red"}],
                     "]"}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"x", "+", "0.5"}], ",", 
                    RowBox[{"y", "+", "0.5"}]}], "}"}]}], "]"}], ",", 
                    RowBox[{"Text", "[", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{
                    RowBox[{"ToString", "[", "i", "]"}], ",", "8"}], "]"}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"x", "+", "0.5"}], ",", 
                    RowBox[{"y", "+", "0.5"}]}], "}"}]}], "]"}]}], "]"}]}], 
                    "}"}]}]}], "]"}], ",", 
                RowBox[{"{", 
                 RowBox[{"cell", ",", "1", ",", "totalCells"}], "}"}]}], 
               "]"}], ",", "1"}], "]"}]}], ";", "\[IndentingNewLine]", 
           RowBox[{"(*", 
            RowBox[{
            "2.", " ", "Creazione", " ", "della", " ", "finestra", " ", "del",
              " ", "gioco"}], "*)"}], 
           RowBox[{"CreateDocument", "[", 
            RowBox[{
             RowBox[{"DynamicModule", "[", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"position", "=", "1"}], ",", 
                 RowBox[{"dice", "=", "0"}], ",", 
                 RowBox[{"gameOver", "=", "False"}]}], "}"}], ",", 
               RowBox[{"Column", "[", 
                RowBox[{
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"Style", "[", 
                    
                    RowBox[{"\"\<Gioco dell'Oca\>\"", ",", "Bold", ",", 
                    "16"}], "]"}], ",", 
                   RowBox[{"Dynamic", "@", 
                    RowBox[{"Graphics", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"boardPrimitives", ",", 
                    RowBox[{"Dynamic", "[", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"position", "<=", "totalCells"}], ",", 
                    RowBox[{"{", 
                    RowBox[{"Red", ",", 
                    RowBox[{"Disk", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"Mod", "[", 
                    RowBox[{
                    RowBox[{"position", "-", "1"}], ",", "cols"}], "]"}], "+",
                     "0.5"}], ",", 
                    RowBox[{
                    RowBox[{"Quotient", "[", 
                    RowBox[{
                    RowBox[{"position", "-", "1"}], ",", "cols"}], "]"}], "+",
                     "0.5"}]}], "}"}], ",", "0.3"}], "]"}]}], "}"}], ",", 
                    RowBox[{"{", "}"}]}], "]"}], "]"}]}], "}"}], ",", 
                    RowBox[{"PlotRange", "->", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"0", ",", "cols"}], "}"}], ",", 
                    RowBox[{"{", 
                    RowBox[{"0", ",", "rows"}], "}"}]}], "}"}]}], ",", 
                    RowBox[{"ImageSize", "->", "400"}]}], "]"}]}], ",", 
                   RowBox[{"Button", "[", 
                    RowBox[{"\"\<Tira il dado\>\"", ",", 
                    RowBox[{
                    RowBox[{"dice", "=", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", "6"}], "}"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"(*", 
                    RowBox[{"Algoritmo", " ", "di", " ", "Euclide"}], "*)"}], 
                    
                    RowBox[{"Module", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"a", ",", "b"}], "}"}], ",", 
                    RowBox[{
                    RowBox[{"a", "=", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"10", ",", "99"}], "}"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"b", "=", 
                    RowBox[{"RandomInteger", "[", 
                    RowBox[{"{", 
                    RowBox[{"1", ",", 
                    RowBox[{"a", "-", "1"}]}], "}"}], "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"CreateDialog", "[", 
                    RowBox[{
                    RowBox[{"DynamicModule", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"localA", "=", "a"}], ",", 
                    RowBox[{"localB", "=", "b"}], ",", 
                    RowBox[{"steps", "=", 
                    RowBox[{"{", "}"}]}], ",", 
                    RowBox[{"stepCount", "=", "dice"}], ",", 
                    RowBox[{"currentStep", "=", "1"}], ",", 
                    RowBox[{"locQuotient", "=", "\"\<\>\""}], ",", 
                    RowBox[{"locRemainder", "=", "\"\<\>\""}], ",", 
                    RowBox[{"locMessage", "=", "\"\<\>\""}]}], "}"}], ",", 
                    RowBox[{"Column", "[", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Row", "[", 
                    RowBox[{"{", 
                    
                    RowBox[{"\"\<Risolvi l'Algoritmo di Euclide per \>\"", ",",
                     "localA", ",", "\"\< e \>\"", ",", "localB"}], "}"}], 
                    "]"}], ",", 
                    RowBox[{"Dynamic", "[", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"localB", "==", "0"}], ",", 
                    RowBox[{"Column", "[", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{
                    
                    RowBox[{"\"\<Algoritmo completato! Il MCD \[EGrave] \>\"",
                     "<>", 
                    RowBox[{"ToString", "[", "localA", "]"}]}], ",", "Bold", ",",
                     "14"}], "]"}], ",", 
                    RowBox[{"Button", "[", 
                    RowBox[{
                    RowBox[{"\"\<Avanza di \>\"", "<>", 
                    RowBox[{"ToString", "[", "dice", "]"}], 
                    "<>", "\"\< caselle\>\""}], ",", 
                    RowBox[{
                    RowBox[{"DialogReturn", "[", "]"}], ";", 
                    RowBox[{"position", "+=", "dice"}]}]}], "]"}]}], "}"}], 
                    "]"}], ",", 
                    RowBox[{"Column", "[", 
                    RowBox[{"{", 
                    RowBox[{
                    RowBox[{"Row", "[", 
                    RowBox[{"{", 
                    
                    RowBox[{"\"\<Passo \>\"", ",", "currentStep", 
                    ",", "\"\<: \>\"", ",", "localA", ",", "\"\< div \>\"", ",",
                     "localB"}], "}"}], "]"}], ",", 
                    RowBox[{"Row", "[", 
                    RowBox[{"{", 
                    RowBox[{"\"\<Quoziente: \>\"", ",", 
                    RowBox[{"InputField", "[", 
                    RowBox[{
                    RowBox[{"Dynamic", "[", "locQuotient", "]"}], ",", 
                    "String", ",", 
                    RowBox[{"FieldSize", "->", "5"}]}], "]"}]}], "}"}], "]"}],
                     ",", 
                    RowBox[{"Row", "[", 
                    RowBox[{"{", 
                    RowBox[{"\"\<Resto: \>\"", ",", 
                    RowBox[{"InputField", "[", 
                    RowBox[{
                    RowBox[{"Dynamic", "[", "locRemainder", "]"}], ",", 
                    "String", ",", 
                    RowBox[{"FieldSize", "->", "5"}]}], "]"}]}], "}"}], "]"}],
                     ",", 
                    RowBox[{"Dynamic", "[", 
                    RowBox[{"Style", "[", 
                    RowBox[{"locMessage", ",", "Red"}], "]"}], "]"}], ",", 
                    RowBox[{"Button", "[", 
                    RowBox[{"\"\<Verifica\>\"", ",", 
                    RowBox[{"Module", "[", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"q", ",", "r"}], "}"}], ",", 
                    RowBox[{
                    RowBox[{"q", "=", 
                    RowBox[{"ToExpression", "[", "locQuotient", "]"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"r", "=", 
                    RowBox[{"ToExpression", "[", "locRemainder", "]"}]}], ";",
                     "\[IndentingNewLine]", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"!", 
                    RowBox[{"NumericQ", "[", "q", "]"}]}], "||", 
                    RowBox[{"!", 
                    RowBox[{"NumericQ", "[", "r", "]"}]}]}], ",", 
                    RowBox[{
                    "locMessage", "=", "\"\<Inserisci numeri validi.\>\""}], ",", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{
                    RowBox[{"q", "==", 
                    RowBox[{"Quotient", "[", 
                    RowBox[{"localA", ",", "localB"}], "]"}]}], "&&", 
                    RowBox[{"r", "==", 
                    RowBox[{"Mod", "[", 
                    RowBox[{"localA", ",", "localB"}], "]"}]}]}], ",", 
                    RowBox[{
                    RowBox[{"AppendTo", "[", 
                    RowBox[{"steps", ",", 
                    RowBox[{"{", 
                    RowBox[{"localA", ",", "localB", ",", "q", ",", "r"}], 
                    "}"}]}], "]"}], ";", "\[IndentingNewLine]", 
                    RowBox[{"currentStep", "++"}], ";", "\[IndentingNewLine]", 
                    RowBox[{"locMessage", "=", "\"\<Corretto!\>\""}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{
                    RowBox[{"{", 
                    RowBox[{"localA", ",", "localB"}], "}"}], "=", 
                    RowBox[{"{", 
                    RowBox[{"localB", ",", "r"}], "}"}]}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"locQuotient", "=", "\"\<\>\""}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"locRemainder", "=", "\"\<\>\""}], ";", 
                    "\[IndentingNewLine]", 
                    RowBox[{"If", "[", 
                    RowBox[{
                    RowBox[{"currentStep", ">", "stepCount"}], ",", 
                    RowBox[{
                    RowBox[{"DialogReturn", "[", "]"}], ";", 
                    RowBox[{"position", "+=", "dice"}]}]}], "]"}]}], ",", 
                    RowBox[{
                    "locMessage", 
                    "=", "\"\<Risposta errata. Riprova.\>\""}]}], "]"}]}], 
                    "]"}]}]}], "]"}]}], "]"}]}], "}"}], "]"}]}], "]"}], 
                    "]"}]}], "}"}], "]"}]}], "]"}], ",", 
                    RowBox[{
                    "WindowTitle", "->", "\"\<Algoritmo di Euclide\>\""}], ",", 
                    RowBox[{"Modal", "->", "True"}]}], "]"}]}]}], "]"}]}], ",", 
                    RowBox[{"Enabled", "->", 
                    RowBox[{"Dynamic", "[", 
                    RowBox[{"!", "gameOver"}], "]"}]}]}], "]"}], ",", 
                   RowBox[{"Dynamic", "[", 
                    RowBox[{"If", "[", 
                    RowBox[{"gameOver", ",", "\"\<Hai vinto!\>\"", ",", 
                    RowBox[{"\"\<Ultimo lancio: \>\"", "<>", 
                    RowBox[{"ToString", "[", "dice", "]"}]}]}], "]"}], "]"}], 
                   ",", 
                   RowBox[{"Spacer", "[", "10", "]"}], ",", 
                   RowBox[{"(*", 
                    RowBox[{
                    RowBox[{
                    "Pulsante", " ", "per", " ", "riavviare", " ", "il", " ", 
                    "gioco"}], ",", 
                    RowBox[{
                    RowBox[{"ad", " ", 
                    RowBox[{"es", "."}]}], ":", 
                    RowBox[{
                    "si", " ", "pu\[OGrave]", " ", "abilitare", " ", "quando",
                     " ", "il", " ", "gioco", " ", "\[EGrave]", " ", 
                    "terminato"}]}]}], "*)"}], 
                   RowBox[{"Dynamic", "[", 
                    RowBox[{"If", "[", 
                    RowBox[{"gameOver", ",", 
                    RowBox[{"Button", "[", 
                    RowBox[{"\"\<Nuova Partita\>\"", ",", 
                    RowBox[{
                    RowBox[{"position", "=", "1"}], ";", 
                    RowBox[{"gameOver", "=", "False"}], ";"}]}], "]"}], 
                    ",", "\"\<\>\""}], "]"}], "]"}]}], "}"}], ",", 
                 RowBox[{"Alignment", "->", "Center"}], ",", 
                 RowBox[{"Spacings", "->", "2"}]}], "]"}]}], "]"}], ",", 
             RowBox[{"WindowTitle", "->", "\"\<Gioco dell'Oca\>\""}]}], "]"}],
            ";"}], "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
           "Se", " ", "il", " ", "valore", " ", "inserito", " ", "non", " ", 
            "\[EGrave]", " ", "numerico"}], "*)"}], ",", 
          RowBox[{
          "Print", "[", "\"\<Il valore inserito non \[EGrave] valido.\>\"", 
           "]"}]}], "]"}], ";"}]}], "]"}]}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{"End", "[", "]"}], "  ", 
   RowBox[{"(*", 
    RowBox[{"Fine", " ", "della", " ", "sezione", " ", "privata"}], "*)"}], "\n", 
   RowBox[{"EndPackage", "[", "]"}], "  ", 
   RowBox[{"(*", 
    RowBox[{"Fine", " ", "del", " ", "pacchetto"}], "*)"}], "\n"}]}]], "Input",\

 CellChangeTimes->{{3.953107236757402*^9, 3.953107353454084*^9}, {
   3.953107392908574*^9, 3.953107541204576*^9}, {3.953107574395261*^9, 
   3.953107595868313*^9}, {3.9531076494440804`*^9, 3.953107650125031*^9}, {
   3.95310771282895*^9, 3.953107713019994*^9}, {3.953279871243966*^9, 
   3.9532799087844667`*^9}, {3.95327994670427*^9, 3.953280115639119*^9}, 
   3.953281809312385*^9, 3.9532823159419518`*^9, 3.9532839513095016`*^9, 
   3.9533453473657084`*^9},ExpressionUUID->"873d27ea-fcfa-4f81-a77b-\
9c999619f67f"]
},
WindowSize->{Full, Full},
WindowMargins->{{Automatic, 133}, {Automatic, 22}},
FrontEndVersion->"14.2 for Mac OS X x86 (64-bit) (December 26, 2024)",
StyleDefinitions->"Default.nb",
ExpressionUUID->"c9e226cf-0ebd-4c60-a329-d8df1b9a7850"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[554, 20, 23275, 513, 1581, "Input",ExpressionUUID->"873d27ea-fcfa-4f81-a77b-9c999619f67f"]
}
]
*)

