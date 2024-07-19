## Acronyms 
- AC : AyA-Committee. 
- AV : AyA-Validator.
- BFT : Byzantine Fault Tolerance. 
- DKG : Distributed Key Generation.
- EN : EarthNode.
- ENN : EarthNode Network.
- ENNFT : EarthNode Non-Fungible-Token.
- ENO : EarthNode Operator.
- ENOPNFT : EarthNode Operator Non-Fungible-Token. 
- eUTxO :  Extended UTxO.
- FROST : Flexible Round-Optimized Schnorr Threshold Signatures. 
- GDPR : General Data Protection Regulation.
- GDST : General-Data-Settlement-Transactions. 
- L1 : Layer 1.
- L2 : Layer 2.
- Ped-DKG :  Pedersen’s Distributed Key Generation. 
- TDST :  Telco-Data-Settlement-Transactions. 
- UTxO : Unspent transaction output.
- WMbft : World Mobile Byzantine Fault Tolerance. 
- WMC : World Mobile Chain.
- WMT : World Mobile Token.

## Glossary 

- AyA : Name of the Layer 2 Sidechain.
- AyA epoch :  The period a particular AyA Committee is in charge. Its length is bound to Root of Trust.
- Delegator : An end-user allocating WMT to one or more EarthNodes.
- Financial Settlement Layer : Public blockchain network where the cus- tomers hold their financial assets.
- Staker :  An end-user allocating WMT to one or more EarthNodes. 
- Telecommunications Settlement Layer :  World Mobile blockchain network that manages and stores telecommunications data. 
- Trust Layer : Source of truth for the connection to and from AyA.

## AyA Validator Registration Specification 

*N.B* : extracted from [Aya Whitepaper](AyAWhitePaper.pdf)

- A valid ENO is in possession of an ENNFT, which allows the registration of an AyA-Validator. 
- Registrations are handled by using a smart contract on the Root of Trust blockchain. 
- On successful execution of this smart contract, a Registration UTxO is created and paid to the smart contract address. 
- A successful registration creates an ENOPNFT, which manages the registration and identifies the operator for this AV on Cardano. 

The following information is required as input for the smart contract transaction:
1. Arbitrary UTxO to be spent, used as nonce
2. UTxO containing a valid ENNFT, which can be used as the nonce input, and the ENNFT is locked in the RegistrationUTxO
3. AyA-Validator PublicKey
4. AyA-Validator Address
5. ReferenceInput AyACommitteeUTxO, which must contain the Sidechain Identity NFT
6. Link to (MpOpToken) policy for ENOPNFT minting
7. Validator-PublicKey to verify signatures on Cardano

- The smart contract verifies that the MpOpToken minting policy successfully mints a unique ENOPNFT, which has to be paid to the ENNFT sending address. 

- The minting of an ENOPNFT has to ensure a unique allocation to its ENNFT. This approach ensures that the relevant UTxOs are spent in this transaction. 

- The ENOPNFT tokenname is stored in the registration UTxO, to allow for changes and de-registration.

- The AVRegistrationContract confirms that the provided AV-PublicKey, used to identify the individual AV on Cardano, is stored in the Datum of the RegistrationUTxO and that the registration data is signed by the AV-PublicKey. 

- Additionally, the provided Sidechain Parameter Hash must match with that in the AyACommitteeUTxO’s Inline-Datum and it must contain the Identity-NFT of the sidechain for verification. The final condition is that the ENNFT must be locked on the RegistrationUTxO.

- During installation of an EarthNode, an AV is set up before registration on Root of Trust because the data needed for the registration is generated during the installation. This means, if the registration is valid, the specific AV is already present on the sidechain and waiting for authorisation to become an active AyA-Validator.

- The registration transaction is picked up by a Light-Client-Module belonging to an existing AV and a transaction is issued on the sidechain. 

- This transaction is verified by the AyA network. On approval, a newly-registered AV receives the base stake to become a validator.

- This finalises the registration on the sidechain and the new AV is eligible for block proposals as well as AC membership.


## Executable Specifications

### **Property 1 - Non Fungible Property Transitivity** : EN Token is an NFT => the ENOP Token should be an NFT

- **Register**
    - `Property 1.0` : Tokens Quantities are verified
      - `r.1.0.0 violation` - No ENNOP Minted (can't be enforced) 
      - `r.1.0.1 violation` - ENNOP Minted Quantity > 1
      - `r.1.0.2 violation` - No ENNFT on Registration validator output                              
      - `r.1.0.3 violation` - ENNFT Quantity > 1                         
    - `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
      - `r.1.1.0 violation` - ENOPNFT TokenName =/ ENNFT TokenName                   
      - `r.1.1.1 violation` - |ENOP NFT| > 1 (Cardinality Violation)                
      - `r.1.1.2 violation` - |EN NFT|   > 1 (Cardinality Violation)
- **Update** 
  - `Property 1.0` : Tokens Quantities are verified
    - `u.1.0.0 violation` - ENNOP Not Output to Operator
    - `u.1.0.1 violation` - No ENNFT on Registration validator output    
    - `u.1.0.2 violation` - No Minting Allowed  
    - `u.1.0.3 violation` - No Burning Allowed                             
  - `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
    - `u.1.1.0 violation` - ENOPNFT TokenName =/ ENNFT TokenName:                   
    - `u.1.1.1 violation` - |ENOP NFT| > 1 (Cardinality Violation)                 
    - `u.1.1.2 violation` - |EN NFT|   > 1 (Cardinality Violation)
- **Deregister** 
  - `Property 1.0` : Tokens Quantities are verified
    - `d.1.0.0 violation` - No ENNOP NFT To Burn  
    - `d.1.0.1 violation` - No Minting Allowed                             
    - `d.1.0.2 violation` - ENOP NFT Quantity to Burn > 1  (enforced by Non Fungible Property of EN Tokens )    
    - `d.1.0.3 violation` - No ENNFT Released To Operator                        
  - `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
    - `d.1.1.0 violation` - ENOPNFT TokenName =/ ENNFT TokenName:                   
    - `d.1.1.1 violation` - |ENOP NFT| > 1 (Cardinality Violation)               
    - `d.1.1.2 violation` - |EN NFT|   > 1 (Cardinality Violation)
    - `d.1.1.3 violation` - No Registration validator Output Allowed 
                  
### **Property 2 - Preserving NFTs ownership** : ENOP and ENNFT can be swapped only between the operator and the registration smart contract

- **Register**
  - `Property 2.0` : ENOP NFT should be given to the operator
    - `r.2.0.0 violation` - ENNOP Minted Not Output to Operator                    
  - `Property 2.1` : ENNFT should be output on script (Already Validated by - `1.0.2 violation` - No ENNFT on Registration validator output  )
          
- **Update**
  - `Property 2.0` : ENOP NFT should be spent and given back to the operator
    - `u.2.0.0 violation` -ENNOP Not Output to Operator(Already Validated by - `u.1.0.0 violation` )                   
  - `Property 2.1` : ENNFT should be output on script (Already Validated by - `u.1.0.2 violation` - No ENNFT on Registration validator output  )
         
- **Deregister** 
  - `Property 2.0` : ENOP NFT should be spent from the operator and burnt
    - `d.2.0.0 violation` - No ENNOP Burn (Already Validated by - `d.1.0.0 violation` )
    - `d.2.0.1 violation` - Burn Without A Proper Registration Validator Input Not Allowed 
  - `Property 2.1` : ENNFT should be spent from Registration scipt and output to Operator (Already Validated by - `d.1.0.1 violation` )

- **Register & Update & Deregister**
  - `Property 2.2` : Only the operator should sign the transaction
    - `2.1.0 violation` - No signer found (Enforced by Ledger Properties)        
    - `2.1.1 violation` - signer is not unique                                  

### **Property 3 - Datum Authenticity & Validity** : The registration details (datum) should be verifiable and Valid (Signed and Provided by the owner of the ENNFT)

- **Register & Update**
    - `3.0.o violation` - Registration datum Output should be valid
        -  `3.0.1.o violation` - `ennftTokenName` field should be equal to the spent ennft token Name
        -  `3.0.2.o violation` - `enopNFTCurrencySymbol` field should be equal to the ENOP NFT currency Symbol minted
    - `3.1.o violation` - Registration datum Output can not be deserialized
    - `3.2.o violation` - Registration datum Output is not Auhtentic                              
    - `3.3.o violation` - Registration validator Output not found (not verifiable)                 
    - `3.4.o violation` - Registration validator Output has no datum (not verifiable)             
    - `3.5.o violation` - Registration validator Output has only the hashed datum (not verifiable)  
    - `3.6.o violation` - More than one Registration validator Output is not allowed (Reducing complexity)
