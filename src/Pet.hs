{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pet (
    Pet (..),
    PetM (..),
    runPet,
    initialParser,
    finalParser,
) where

import Control.Monad.State.Lazy
import Data.Text.Lazy (Text)
import Final
import Initial

data Pet = Pet
    { petName :: Text
    , petState :: Text
    }
    deriving (Show)

newtype PetM a = PetM {unPetM :: State Pet a}
    deriving (Functor, Applicative, Monad, MonadState Pet)

class (Monad m) => PetMonad m where
    setPetName :: Text -> m ()
    setPetState :: Text -> m ()

instance PetMonad PetM where
    setPetName nn = do
        ps <- gets petState
        put $ Pet nn ps
    setPetState nps = do
        pn <- gets petName
        put $ Pet pn nps

instance PetLang (PetM Text) where
    namePet n = do
        setPetName n
        gets petName
    petDo pn ps = do
        setPetName pn
        setPetState ps
        gets petState

runPet :: PetM a -> Pet -> Pet
runPet act = execState (unPetM act)
