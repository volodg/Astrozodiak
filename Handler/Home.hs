{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}
module Handler.Home where

import Import
import Control.Monad.Catch (MonadThrow)
import Text.Blaze (ToMarkup)
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ homepageFileWidget formWidget formEnctype submission handlerName

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ homepageFileWidget formWidget formEnctype submission handlerName

homepageFileWidget :: forall (m :: * -> *) a a1 a2 a3.
                            (MonadThrow m,
                             ToMarkup a2,
                             ToMarkup a1,
                             ToMarkup a, MonadBaseControl IO m,
                             MonadIO m, ToWidget App a3) =>
                            a3 -> a2 -> Maybe (FileInfo, a1) -> a -> WidgetT App m ()
homepageFileWidget formWidget formEnctype submission handlerName = do
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
