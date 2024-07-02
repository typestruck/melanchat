module Server.File where

import Prelude

import Control.Promise (Promise)
import Control.Promise as CP
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Set (member) as DS
import Data.String (Pattern(..))
import Data.String (split) as DS
import Data.UUID as DU
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Buffer (Buffer)
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Aff as NFA
import Run as R
import Server.Effect (BaseEffect, Configuration)
import Server.Response as SR
import Shared.Resource (Media(..), ResourceType(..), allowedExtensions, allowedMediaTypes, localBasePath, maxImageSize, maxImageSizeKB, uploadFolder)
import Shared.Resource as SP

foreign import realFileExtension_ ∷ Buffer → Effect (Promise String)

realFileExtension ∷ Buffer → Aff String
realFileExtension buffer = CP.toAffE $ realFileExtension_ buffer

invalidImageMessage ∷ String
invalidImageMessage = "Invalid image"

imageTooBigMessage ∷ String
imageTooBigMessage = "Max allowed size for pictures is " <> maxImageSizeKB

saveBase64File ∷ ∀ r. String → BaseEffect { configuration ∷ Configuration | r } String
saveBase64File input =
      case DS.split (Pattern ",") input of
            [ mediaType, base64 ] → do
                  case DH.lookup mediaType allowedMediaTypes of
                        Nothing → invalidImage
                        Just _ → do
                              buffer ← R.liftEffect $ NB.fromString base64 Base64
                              bufferSize ← R.liftEffect $ NB.size buffer
                              if bufferSize > maxImageSize then
                                    SR.throwBadRequest imageTooBigMessage
                              else do
                                    extension ← map ("." <> _) <<< R.liftAff $ realFileExtension buffer
                                    if DS.member extension allowedExtensions then do
                                          uuid ← R.liftEffect (DU.toString <$> DU.genUUID)
                                          let fileName = uuid <> extension
                                          R.liftAff $ NFA.writeFile (localBasePath <> uploadFolder <> fileName)  buffer
                                          pure fileName
                                    else
                                          invalidImage
            _ → invalidImage
      where
      invalidImage = SR.throwBadRequest invalidImageMessage