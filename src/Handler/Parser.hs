{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-#LANGUAGE QuasiQuotes #-}
module Handler.Parser where

import Import
import Handler.ModalParser
import Handler.AST
import Text.Parsec


data Wff = Wff
    { getFormula :: Text}

instance Show Wff where
    show (Wff{ getFormula    = xs }) = show $ tr $ extractRight $ parse parseFormula "" $ (unpack xs)

wffForm ::  Html -> MForm Handler (FormResult Wff, Widget)
wffForm = renderDivs $ Wff
    <$> areq textField "Your formula:  " Nothing

getParserR :: Handler Html
getParserR = do 
    (widget, enctype) <- generateFormPost wffForm
    defaultLayout $ do 
        setTitle "Dynamic Epistemic Logic translator"
        [whamlet| <p> Please, type your DEL formula </p>
    <form method=post action=@{ParsedR} enctype=#{enctype}>
        ^{widget} </form>
        <button>Submit </button>|]