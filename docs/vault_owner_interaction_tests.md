## Tests
### Vault
#### Querying Vaults
##### Offchain
 - Querying for all vaults known by the protocol should return a list of all known vaults.
 - Querying for all vaults a user owns by passing the users public key hash returns a list of all of the user owned vaults.
 - Querying for vaults that are undercollaterized should return all undercollaterized vaults.
Note that for all the cases where no vault exists matching the criteria we expect an empty result, this means we need a specific test case covering this.
We also need to check whether all vault information is present when getting the query result, this includes:
 - the amount of collateral
 - the amount of dUSD (debt/credit)
 - the collateralization ratio
 - the vaults health score, TODO: what is this score? e.g. `liquidationRatio - collateralizationRatio` normalized?

##### UI
###### Functional
 - Visiting the "Vaults" page lists all the vaults owned by the visiting user.
 - Applying a filter to view undercollaterized vaults, lists all undercollaterized vaults owned by the visiting user.
 - Selecting a sort method on the list of owned vaults, like amount of collateral, debt, collateralization ratio, and vault health state, sorts the list accordingly.
 - Clicking on one of the enlisted vaults, shows more details about respective valut (TODO: what information are of interest) in form of a TODO popup/seperate page?
 - Unhealthy vaults should be visually highlighted (e.g. using a color like red).
 - While waiting for the vault list to be fetched a [Throbber](https://en.wikipedia.org/wiki/Throbber) should be displayed
 - If no vaults are available a respective informative text should be displayed
 - If no vaults are available the user should be offered to initialize his/her first vault
###### Non-Functional 
 - Applying a filter has immediate effect on the displayed list.
 - Applying a sort method has immediate effect on the displayed list.
 - The list should respond immediately to interactions.

#### Initializing Vaults
##### Offchain
 - Initializing a vault if the last update time of the price-oracle was more than two hours ago should fail. Returning a descriptive message.
 - Initializing a vault given the provided collateral and collateralization ratio, succeeds only if the minimal amount of dUSD that is going to be minted is greater or equal the configured `Debt Floor` (i.e. min. amount of dUSD that is minted) which  is configured by the protocol (TODO: by the protocol? This was described in the whitepaper see 1.5 Debt Floor). The opposite case if that criteria is not met should fail.
 - Initializing a vault even though owning an undercollaterized vault should succeed, if the above criteria is met.
 - Initializing multiple vaults should succeed if the above criteria is met.
##### UI
 - Clicking on an `Initialize Vault` button opens up a page/popup with a form guiding through the vault initialization/configuration process.
 - The "Initialize Vault" page/popup shows the amount of the to be minted dUSD (debt/credit) given the entered collateralization amount.
 - The "Initialize Vault" page/popup prevents the user from initializing the vault, if he/she does not have sufficient funds in the wallet.
 - The "Initialize Vault" page/popup prevents the user from initializing the vault, if the entered collateral amount does not exceed the minimal collateralization rate.
 - The "Initialize Vault" pae/popup warns the user if the entered collateral amount does not exceed the minimal collateralization rate.
 - After the successful initialization the user is informed about success.
 - After the successful initialization the user is redirected to his owned vaults list.
 - If the vault initialization fails a descriptive error message is presented.
 - If the vault initialization fails the user is offered to try it again or cancel.
 - Retrying will reuse the previously entered data.

#### Depositing Collateral to Vaults
##### Offchain
 - Depositing an amount to the vault that leaves the amount of dUSD (debt/credit) above the `Debt Floor` should succeed. Test inverse case too.
 - Depositing collateral to a non-undercollaterized vault leaving the amount of dUSD (debt/creadit) above the `Debt Floor` should succeed.
 - Depositing collateral to a undercollaterized vault succeeds (TODO: only if the vault will result in not being in a undercollaterized state?)
 - Depositing a negative amount should fail. Returning a descriptive error message.
 - Depositing 0 should fail. Returning a descriptive error message.
##### UI
###### Functional
 - Clicking the `Deposit` button opens up a page/popup with a form guiding the user through the process for depositing collateral to the vault.
 - The "deposit to vault" page/popup displays the current information about the vautl he/she wants to deposit to.
 - The "deposit to vault" page/popup displays a textfield allowing the user to specify an amount that should be deposited to the collateral.
 - Entering a invalid (negative or zero) amount should prevent the user to continue with the deposit process.
 - Entering a valid amount shows the effects on the vaults information transparently. (e.g. use diff color)
 - The "deposit to vault" page/popup displays a button to cancel the process.
 - Canceling the "deposit to vault" process returns the user to his vault list.
 - The "deposit to vault" page/popup displays a button to submit the deposit.
 - Submitting the deposit displays a Throbber while the transaction is being processed.
 - If the deposit transaction fails, a descriptive error message is shown.
 - If the deposit transaction fails, the user is offered to retry or cancel.
 - Retrying will reuse the previously entered data.
###### Non-Functional
 - Entering modifying the collateral amount has immediate effects on the vaults information displayed.


### Vault Integration Tests
