(**
# Numbers
An application that converts numbers to written form in Turkish.
For instance 12345 -> OnİkiBinÜçYüzKırkBeş

It also works with fable so that you can compile it to javascript.

    - First make sure you  have installed NodeJS
    - Also make sure you build the project to restore nuget dependencies;
    - Then install Fable with npm install -g fable from command line.
    - Go to the project folder (not solution) then run fable from command line. Ignore the errors.
    - Then run index.html from a web server or browser (IE and edge requires a browser)
That's all.
*)
module NumbersLib

open Fable.Core
open Fable.Import.Browser

(**
## We define the domain here:
*)
type Digit =
    | Digit of int

type Thousands =
    | Thousands of Digit * Digit * Digit

//Number is an alias
type Number = Thousands list

type StringToDigits = string -> Digit list

//functions to be used
type DigitsToNumber = Digit list -> Number

type NumberToString = Number -> string

type IConvertToText = string -> string

type GetAlgorithm = StringToDigits -> DigitsToNumber -> NumberToString -> IConvertToText

(**
## We implement the functions here: *)
(** Fuse the functions: *)
let getAlgorithm : GetAlgorithm =
    fun strToDigits digitsToNumber numToStr ->
        strToDigits
        >> digitsToNumber
        >> numToStr

(** *)
let stringToDigits : StringToDigits =
    fun str ->
        //function char -> State -> State
        let strToDigit (c : char) (state : Digit list) =
            (c.ToString()
             |> int
             |> Digit)
            :: state
        str.ToCharArray()
        |> List.ofArray
(** We lift the function to char list -> State -> State.
    We call fold back because we want to start from back of the list.
*)
        |> (List.foldBack strToDigit)
        <| [] // initial state as empty list

let digitsToNumber : DigitsToNumber =
    fun digits ->
        let addToCurrentThousands (digit : Digit) (state : Thousands list, place) =
            //place represents the current digit in thousands
            match place, state with
            | 0, _ | _, [] ->
                //create a new thousands
                let thousands = Thousands(Digit(0), Digit(0), digit)
                thousands :: state, 1
            | 1, Thousands(f, s, t) :: tail -> Thousands(f, digit, t) :: tail, 2
            | _, Thousands(f, s, t) :: tail -> Thousands(digit, s, t) :: tail, 0
(**
###Example use of back and forth pipes.
using such techniques we reduce the temporary variables thus
increase the readability.*)
        List.foldBack addToCurrentThousands
            <| digits
            <| ([], 0)
        //we only care fist element of state, not places.
            |> fst

(**
###String conversion functions: *)
let numberToString : NumberToString =
    fun number ->
        let digitToString =
            function
            | Digit 1 -> "Bir"
            | Digit 2 -> "İki"
            | Digit 3 -> "Üç"
            | Digit 4 -> "Dört"
            | Digit 5 -> "Beş"
            | Digit 6 -> "Altı"
            | Digit 7 -> "Yedi"
            | Digit 8 -> "Sekiz"
            | Digit 9 -> "Dokuz"
            | Digit _ -> ""

        let digitTensToString =
            function
            | Digit 1 -> "On"
            | Digit 2 -> "Yirmi"
            | Digit 3 -> "Otuz"
            | Digit 4 -> "Kırk"
            | Digit 5 -> "Elli"
            | Digit 6 -> "Altmış"
            | Digit 7 -> "Yetmiş"
            | Digit 8 -> "Seksen"
            | Digit 9 -> "Doksan"
            | Digit _ -> ""

        let thousandplaceToString =
            function
            | 3 -> "Milyar"
            | 2 -> "Milyon"
            | 1 -> "Bin"
            | _ -> ""

        (** Final string constriction: *)
        let thousandsToString (state : string, place) thousands =
            //this is the common concatting function always adds new string to "state"
            //at each call we reduce place by 1 thus define "Milyar", "Milyon" etc
            let concat s t prefix =
                state + prefix + (s |> digitTensToString) + (t |> digitToString) + (place |> thousandplaceToString),
                place - 1

(** Pattern matching*)
            match thousands, place with
(** If 000 then no string *)
            | Thousands(Digit 0, Digit 0, Digit 0), _ -> state, place - 1
(** If 001 and place is 1 then it's Bin *)
            | Thousands(Digit 0, Digit 0, Digit 1), 1 -> "Bin" + state, place - 1
            | Thousands(Digit 0, s, t), _ -> "" |> concat s t
(** We say Yüz not Bir Yüz*)
            | Thousands(Digit 1, s, t), _ -> "Yüz" |> concat s t
            | Thousands(f, s, t), _ -> (f |> digitToString) + "Yüz" |> concat s t

(** Lift the function to the list world and give
    initial state as "", number of thousands -1 *)
        List.fold thousandsToString
            <| ("", (number |> List.length) - 1)
            <| number
(** We only care fist element of state, not places.*)
            |> fst

(** Main function to be called. This can be called from a console application as well. *)
let convertToText str =
    let algo = getAlgorithm stringToDigits digitsToNumber numberToString
    algo str

(**
##Fable (Web UI) specific.
Here we read text from text boxes and write to the text area:
 *)
let textBox = document.getElementById ("input") :?> HTMLInputElement
let button = document.getElementById ("button") :?> HTMLButtonElement
let result = document.getElementById ("result") :?> HTMLTextAreaElement

let submit() =
    textBox.value
    |> convertToText
    |> fun r -> result.textContent <- r

button.addEventListener_click (fun _ ->
    submit()
    null)
