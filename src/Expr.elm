module Expr exposing (..)

import Debug exposing (todo)
import Dict as Dict exposing (..)
import Html exposing (text)
import Parser exposing (..)
import Set as Set exposing (..)


undefined : a
undefined =
    todo "TODO"


type Expr
    = T
    | F
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Imply Expr Expr
    | Equivalent Expr Expr
    | Var String


isTrue : Expr -> Bool
isTrue exp =
    let
        allCombs =
            allCombinations exp

        sat comb =
            eval exp comb
    in
    List.all sat allCombs


vars : Expr -> Set String
vars exp =
    case exp of
        T ->
            Set.empty

        F ->
            Set.empty

        Not subExp ->
            vars subExp

        And subExp1 subExp2 ->
            Set.union (vars subExp1) (vars subExp2)

        Or subExp1 subExp2 ->
            Set.union (vars subExp1) (vars subExp2)

        Imply subExp1 subExp2 ->
            Set.union (vars subExp1) (vars subExp2)

        Equivalent subExp1 subExp2 ->
            Set.union (vars subExp1) (vars subExp2)

        Var name ->
            Set.fromList [ name ]


allCombinations : Expr -> List (Dict String Bool)
allCombinations exp =
    let
        combinationsFromList : List String -> List (Dict String Bool)
        combinationsFromList lst =
            case lst of
                x :: xs ->
                    let
                        inner =
                            combinationsFromList xs
                    in
                    List.map (Dict.insert x True) inner ++ List.map (Dict.insert x False) inner

                [] ->
                    [ Dict.empty ]
    in
    combinationsFromList <| Set.toList <| vars exp


eval : Expr -> Dict String Bool -> Bool
eval exp mp =
    case exp of
        T ->
            True

        F ->
            False

        Not subExp ->
            not (eval subExp mp)

        And subExp1 subExp2 ->
            eval subExp1 mp && eval subExp2 mp

        Or subExp1 subExp2 ->
            eval subExp1 mp || eval subExp2 mp

        Imply subExp1 subExp2 ->
            not (eval subExp1 mp) || eval subExp2 mp

        Equivalent subExp1 subExp2 ->
            (not (eval subExp1 mp) || eval subExp2 mp) && (eval subExp1 mp || not (eval subExp2 mp))

        Var name ->
            Dict.get name mp |> Maybe.withDefault True


expr : Parser Expr
expr =
    succeed identity
        |. spaces
        |= equivalentExpr
        |. spaces
        |. end


equivalentExpr : Parser Expr
equivalentExpr =
    oneOf
        [ backtrackable <|
            succeed Equivalent
                |= implicationExpr
                |. spaces
                |. symbol "≡"
                |. spaces
                |= lazy (\_ -> equivalentExpr)
        , implicationExpr
        ]


implicationExpr : Parser Expr
implicationExpr =
    oneOf
        [ backtrackable <|
            succeed Imply
                |= disjunctiveExpr
                |. spaces
                |. symbol "⇒"
                |. spaces
                |= lazy (\_ -> implicationExpr)
        , disjunctiveExpr
        ]



--The order of operations is ¬ , ∧, ∨, =>, <=>
--expr -> EquivalenceExpr
--EquivalenceExpr -> ImplicationExpr <=> EquivalenceExpr | ImplicationExpr
--ImplicationExpr -> DisjunctiveExpr => ImplicationExpr | DisjunctiveExpr
--DisjunctiveExpr -> ConjunctiveExpr ∨ DisjunctiveExpr | ConjunctiveExpr
--ConjunctiveExpr -> NegateExpr ∧ ConjunctiveExpr | NegateExpr
--NegateExpr -> ¬ NegateExpr | GroupingExpr
--GroupingExpr -> (expr) | AtomicExpr
--AtomicExpr -> T | F | p | q | r | ...


disjunctiveExpr : Parser Expr
disjunctiveExpr =
    oneOf
        [ backtrackable <|
            succeed Or
                |= conjunctiveExpr
                |. spaces
                |. symbol "∨"
                |. spaces
                |= lazy (\_ -> disjunctiveExpr)
        , conjunctiveExpr
        ]


conjunctiveExpr : Parser Expr
conjunctiveExpr =
    oneOf
        [ backtrackable <|
            succeed And
                |= negateExpr
                |. spaces
                |. symbol "∧"
                |. spaces
                |= lazy (\_ -> conjunctiveExpr)
        , negateExpr
        ]



--


negateExpr : Parser Expr
negateExpr =
    oneOf
        [ backtrackable <|
            succeed Not
                |. symbol "¬"
                |. spaces
                |= lazy (\_ -> negateExpr)
        , groupingExpr
        ]


groupingExpr : Parser Expr
groupingExpr =
    oneOf
        [ backtrackable <|
            succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> equivalentExpr)
                |. spaces
                |. symbol ")"
        , atomicExpr
        ]


atomicExpr : Parser Expr
atomicExpr =
    backtrackable <|
        oneOf
            [ succeed T |. keyword "true"
            , succeed F |. keyword "false"
            , succeed Var
                |= variable { start = Char.isLower, inner = always False, reserved = Set.empty }
            ]
