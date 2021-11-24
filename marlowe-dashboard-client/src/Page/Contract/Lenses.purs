module Page.Contract.Lenses
  ( _Starting
  , _Started
  , _nickname
  , _tab
  , _executionState
  , _pendingTransaction
  , _previousSteps
  , _marloweParams
  , _selectedStep
  , _metadata
  , _participants
  , _userParties
  , _namedActions
  , _expandPayments
  , _resultingPayments
  , _stateNickname
  , _stateMetadata
  , _stateParticipants
  ) where

import Prologue
import Component.Contacts.Types (WalletNickname)
import Data.Lens (Lens', Prism', lens', prism')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\))
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.Semantics (Party)
import Page.Contract.Types (StartedState, State(..), StartingState)

_Starting :: Prism' State StartingState
_Starting =
  prism'
    Starting
    ( case _ of
        Starting s -> Just s
        _ -> Nothing
    )

_Started :: Prism' State StartedState
_Started =
  prism'
    Started
    ( case _ of
        Started s -> Just s
        _ -> Nothing
    )

_nickname :: forall a r. Lens' { nickname :: a | r } a
_nickname = prop (Proxy :: _ "nickname")

_stateNickname :: Lens' State String
_stateNickname = lens' go
  where
  go (Starting s) = s.nickname /\ Starting <<< s { nickname = _ }

  go (Started s) = s.nickname /\ Started <<< s { nickname = _ }

_tab :: forall a r. Lens' { tab :: a | r } a
_tab = prop (Proxy :: _ "tab")

_executionState :: forall a r. Lens' { executionState :: a | r } a
_executionState = prop (Proxy :: _ "executionState")

_pendingTransaction :: forall a r. Lens' { pendingTransaction :: a | r } a
_pendingTransaction = prop (Proxy :: _ "pendingTransaction")

_previousSteps :: forall a r. Lens' { previousSteps :: a | r } a
_previousSteps = prop (Proxy :: _ "previousSteps")

_marloweParams :: forall a r. Lens' { marloweParams :: a | r } a
_marloweParams = prop (Proxy :: _ "marloweParams")

_selectedStep :: forall a r. Lens' { selectedStep :: a | r } a
_selectedStep = prop (Proxy :: _ "selectedStep")

_metadata :: forall a r. Lens' { metadata :: a | r } a
_metadata = prop (Proxy :: _ "metadata")

_stateMetadata :: Lens' State MetaData
_stateMetadata = lens' go
  where
  go (Starting s) = s.metadata /\ Starting <<< s { metadata = _ }

  go (Started s) = s.metadata /\ Started <<< s { metadata = _ }

_participants :: forall a r. Lens' { participants :: a | r } a
_participants = prop (Proxy :: _ "participants")

_stateParticipants :: Lens' State (Map Party (Maybe WalletNickname))
_stateParticipants = lens' go
  where
  go (Starting s) = s.participants /\ Starting <<< s { participants = _ }

  go (Started s) = s.participants /\ Started <<< s { participants = _ }

_userParties :: forall a r. Lens' { userParties :: a | r } a
_userParties = prop (Proxy :: _ "userParties")

_namedActions :: forall a r. Lens' { namedActions :: a | r } a
_namedActions = prop (Proxy :: _ "namedActions")

_expandPayments :: forall a r. Lens' { expandPayments :: a | r } a
_expandPayments = prop (Proxy :: _ "expandPayments")

_resultingPayments :: forall a r. Lens' { resultingPayments :: a | r } a
_resultingPayments = prop (Proxy :: _ "resultingPayments")
