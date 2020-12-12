{
module Parser.Lexer ( Token (..)
                    , lexProg ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$symb = [\_]

tokens:-
    $white+                             ;

    "if"                                { const TIf }
    "then"                              { const TThen }
    "else"                              { const TElse }
    "end"                               { const TEnd }
    "while"                             { const TWhile }
    "skip"                              { const TSkip }
    "inv"                               { const TInv }
    "do"                                { const TDo }
    "program"                           { const TProgram }
    "is"                                { const TIs }
    "pre"                               { const TPre }
    "post"                              { const TPost }
    "forall"                            { const TForall }
    "exists"                            { const TExists }
    "true"                              { const TTrue }
    "false"                             { const TFalse }

    $digit+                             { TokenInt . read }
    $alpha [$alpha $digit $symb ]*      { TokenName }
    [\[ \] \+ \- \* \/ \% ]             { TokenSymb }
    [\( \) ]                            { TokenSymb }
    
    [\= \< \>]                          { TokenSymb }
    \!\=                                { TokenSymb }
    \>\=                                { TokenSymb }
    \<\=                                { TokenSymb }
    \!                                  { TokenSymb }
    
    \|\|                                { TokenSymb }
    \&\&                                { TokenSymb }

    \:\=                                { TokenSymb }
    \,                                  { TokenSymb }
    \;                                  { TokenSymb }
    \=\=\>                              { TokenSymb }


{
data Token = TokenInt Int
           | TokenName String
           | TokenSymb String
           | TIf
           | TThen
           | TElse
           | TEnd
           | TWhile
           | TDo
           | TSkip
           | TProgram
           | TIs
           | TForall
           | TExists
           | TInv
           | TPre
           | TPost
           | TTrue
           | TFalse
           deriving (Eq, Show)

lexProg :: String -> [Token]
lexProg = alexScanTokens

}