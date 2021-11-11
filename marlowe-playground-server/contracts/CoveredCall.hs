{-# LANGUAGE OverloadedStrings #-}
module CoveredCall where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

contract :: Contract
contract =
  coveredCall
    (Role "Party")
    (Role "Counterparty")
    (Token "" "Currency")
    (Token "" "Underlying")
    (ConstantParam "Strike")
    (ConstantParam "Ratio")
    (SlotParam "Expiry")

-- |A Covered Call is an option strategie constructed by writing a call on a token
coveredCall ::
     Party    -- ^ Issuer of the covered call, i.e. short the call option
  -> Party    -- ^ Counterparty, long the call option
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value    -- ^ Strike price (in currency)
  -> Value    -- ^ Amount of underlying tokens per contract
  -> Timeout  -- ^ Expiration date (American style exercise)
  -> Contract -- ^ Covered Call contract
coveredCall party counterparty currency underlying strike ratio expiry =
  deposit 10 party underlying ratio $                   -- Party deposits an underlying into the contract
    exercise
      counterparty                                      -- Counterparty chooses to exercise the call option
      Close                                             -- No exercise, the party is refunded
      ( deposit expiry counterparty currency strike     -- Exercise, the Counterparty has to deposit the strike
      $ Pay counterparty (Party party) currency strike  -- Strike is payed to the Party
      $ Pay party (Party counterparty) underlying ratio -- Underlying is payed to the Counterparty
        Close
      )
      expiry                                            -- Expiry date for the call option
      Close

-- |Building block for `Deposit`
deposit ::
     Timeout  -- ^ Timeout for deposit
  -> Party    -- ^ Deposit into account of party
  -> Token    -- ^ Token to deposit
  -> Value    -- ^ Amount of token
  -> Contract -- ^ Continuation after deposit
  -> Contract -- ^ Contract
deposit timeout from token amount cont =
  When
    [ Case
        (Deposit from from token amount)
        cont
    ]
    timeout
    Close

-- |Building block for option exercise contract logic
exercise ::
     Party    -- ^ The party that can choose to exercise the option
  -> Contract -- ^ Continuation if not exercised
  -> Contract -- ^ Continuation if exercised
  -> Timeout  -- ^ Expiration date (American style exercise)
  -> Contract -- ^ Continuation when expired
  -> Contract -- ^ Contract
exercise party contract0 contract1 =
  When
    [ Case
        (Choice choiceId [Bound 0 1])
        (If (ValueEQ (ChoiceValue choiceId) (Constant 0)) contract0 contract1)
    ]
  where
    choiceId :: ChoiceId
    choiceId = ChoiceId "exercise" party
