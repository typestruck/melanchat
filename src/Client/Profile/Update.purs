module Client.Profile.Update where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types
import Shared.Profile.Types

import Client.Common.Dom as CCD
import Client.Common.File as CCF
import Client.Common.Network (request)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate, Environment)
import Flame.Application.Effectful as FAE
import Flame.Subscription as FS
import Payload.ResponseTypes (Response(..))
import Shared.Element (ElementId(..))
import Shared.Network (RequestStatus(..))
import Shared.Options.MountPoint (imId)
import Web.DOM (Element)

getFileInput ∷ Effect Element
getFileInput = CCD.unsafeGetElementById AvatarFileInput

update ∷ AffUpdate ProfileModel ProfileMessage
update rc@{ message } =
      case message of
            SelectAvatar → selectAvatar
            SetPField setter → pure setter
            Save field → saveField rc field
            SetProfileChatExperiment experiment → setChatExperiment experiment

saveField ∷ Environment ProfileModel ProfileMessage → Field → Aff (ProfileModel → ProfileModel)
saveField rc@{ display } field = do
      display $ _ { loading = true }
      case field of
            Generated what → saveGeneratedField rc what
            Avatar base64 → saveAvatar rc base64
            Age → saveAge rc
            Gender → saveGender rc
            Country → saveCountry rc
            Languages → saveLanguages rc
            Tags → saveTags rc
      EA.delay $ Milliseconds 3000.0
      pure
            ( _
                    { updateRequestStatus = Nothing
                    , loading = false
                    }

            )

saveGeneratedField ∷ Environment ProfileModel ProfileMessage → What → Aff Unit
saveGeneratedField rc@{ display, model } what =
      case what of
            Name → do
                  value ← save display $ req rc.model.nameInputed
                  let name = DM.fromMaybe model.user.name value
                  liftEffect <<< FS.send imId $ SetNameFromProfile name
                  display
                        ( _
                                { nameInputed = Nothing
                                , user { name = name }
                                }
                        )
            Headline → do
                  value ← save display $ req rc.model.headlineInputed
                  display
                        ( _
                                { headlineInputed = Nothing
                                , user { headline = DM.fromMaybe model.user.headline value }
                                }
                        )
            Description → do
                  value ← save display $ req rc.model.descriptionInputed
                  display
                        ( _
                                { descriptionInputed = Nothing
                                , user { description = DM.fromMaybe model.user.description value }
                                }
                        )
      where
      req value = request.profile.field.generated { body: { field: what, value: if value == Just "" then Nothing else value } }

saveAvatar ∷ Environment ProfileModel ProfileMessage → Maybe String → Aff Unit
saveAvatar { display } base64 = do
      void <<< save display $ request.profile.field.avatar { body: { base64 } }
      liftEffect <<< FS.send imId $ SetAvatarFromProfile base64
      display $ _ { user { avatar = base64 } }

saveAge ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveAge { display, model: { ageInputed } } = do
      let birthday = DM.fromMaybe Nothing ageInputed
      void <<< save display $ request.profile.field.age { body: { birthday } }
      display
            ( _
                    { ageInputed = Nothing
                    , user { age = birthday }
                    }
            )

saveGender ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveGender { display, model: { genderInputed } } = do
      let gender = DM.fromMaybe Nothing genderInputed
      void <<< save display $ request.profile.field.gender { body: { gender } }
      display
            ( _
                    { genderInputed = Nothing
                    , user { gender = gender }
                    }
            )

saveCountry ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveCountry { display, model: { countryInputed } } = do
      let country = DM.fromMaybe Nothing countryInputed
      void <<< save display $ request.profile.field.country { body: { country } }
      display
            ( _
                    { countryInputed = Nothing
                    , user { country = country }
                    }
            )

saveLanguages ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveLanguages { display, model: { languagesInputedList } } = do
      void <<< save display $ request.profile.field.language { body: { ids: languagesInputedList } }
      display
            ( _
                    { languagesInputed = Nothing
                    , languagesInputedList = Nothing
                    , user { languages = DM.fromMaybe [] languagesInputedList }
                    }
            )

saveTags ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveTags { display, model: { tagsInputedList } } = do
      void <<< save display $ request.profile.field.tag { body: { tags: tagsInputedList } }
      display
            ( _
                    { tagsInputed = Nothing
                    , tagsInputedList = Nothing
                    , user { tags = DM.fromMaybe [] tagsInputedList }
                    }
            )

save display request = do
      result ← request
      _ ← display $ _ { loading = false }
      case result of
            Right (Response { body }) → do
                  --compiler bug? we need _ <- here even tho display is Unit
                  _ ← display $ _ { updateRequestStatus = Just Success }
                  pure $ Just body
            _ → do
                  _ ← display $ _ { updateRequestStatus = Just Failure }
                  pure Nothing

selectAvatar ∷ Aff (ProfileModel → ProfileModel)
selectAvatar = do
      liftEffect do
            field ← getFileInput
            CCF.triggerFileSelect field
      FAE.noChanges

setChatExperiment ∷ Maybe ExperimentData → Aff (ProfileModel → ProfileModel)
setChatExperiment experimenting = FAE.diff { experimenting }
