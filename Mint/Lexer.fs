module Lexer

open System

type Element =
    | Text of string
    | Numeric of int

type Symbol =
    | PlusEqual
    | Colon
    | LessThan
    | GreaterThan

type Token = 
    | Space
    | Element of string
    | RawText of string
    | Symbol of Symbol
    | End
    | IllegalWhitespace
    | IllegalCharacter of char
    | MalformedLexeme

let private isWhitespace = Char.IsWhiteSpace
let private isDigit = Char.IsDigit
let private alphas = Seq.append { 'a'..'z' } { 'A'..'Z' }
let private isAlpha c = Seq.contains c alphas
let private isAlphaNumeric c = isAlpha c || isDigit c || c = '+' || c = '-' || c = '_'
let private isRawTextStart c = 
    match c with
    | '\"' -> true
    | _    -> false

let private asString = Array.ofList >> System.String

let private asInt = asString >> int

let rec private consume p x =
    match x with
    | c::cs when p c -> let (matches, rest) = consume p cs
                        (c::matches, rest)
    | cs             -> ([], cs)

let private whitespace x =
    let (w, rest) = consume isWhitespace x
    match w with
    | [' '] -> (Space, rest)
    | _     -> (IllegalWhitespace, rest)

let private element x =
    let (e, rest) = consume isAlphaNumeric x
    (Element <| asString e, rest)

let private rawText x =
    match x with
    | '\"'::xs -> let (r, rest) = consume (fun c -> c <> '\"') xs
                  match rest with
                  | '\"'::rest' -> (RawText <| asString r, rest')
                  | _           -> (IllegalCharacter '\"', xs)
    | c::xs    -> (MalformedLexeme, xs)
    | []       -> (End, [])

let private next x =
    match x with
    | []                         -> (End, [])
    | '+'::'='::xs               -> (Symbol PlusEqual, xs)
    | ':'::xs                    -> (Symbol Colon, xs)
    | '<'::xs                    -> (Symbol LessThan, xs)
    | '>'::xs                    -> (Symbol GreaterThan, xs)
    | c::_ when isWhitespace c   -> whitespace x
    | c::_ when isAlphaNumeric c -> element x
    | c::_ when isRawTextStart c -> rawText x
    | c::cs                      -> (IllegalCharacter c, cs)

let rec lex chars =
    let (token, rest) = next chars
    match token with
    | End -> [End]
    | _   -> token::lex rest
