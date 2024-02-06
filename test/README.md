# Single Asset Staking Unit Tests Documentation

This page offers a comprehensive guide to the Single Asset Staking unit tests within the project. The tests are meticulously designed to assess the accuracy and efficiency of the Single Asset Staking implementation, particularly focusing on the validation of actions involved during the staking  process.

## Overview

The unit tests for the Single Asset Staking are organized into a suite titled "Staking Unit Test." This suite is dedicated to verifying the validation process for the various actions involved in orchestrating and participating in staking.

## Test Suite Details

### Tests Included

-**Pass - Init Staking**: This test confirms the proper initialization of Staking by creating of a head node for the linked list which will contain all the staking requests and stake  amount.
-**Pass - Deinit Staking**: This confirms correct working validation of deinitization of head node once the staking rewards have been processed correctly.
-**Pass - Insert Staking**: Aims to validate a new user's request to participate in Staking, confirming its correct positioning and validation.
-**Pass - Remove Staking**: Aims to validate an existing user's request to discontinue participation (before stake is frozen) in Staking and reclaim their stake. It also checks that the removal happens without compromising the integrity of the linked list.
-**Pass - Late Remove Staking**: Aims to validate an existing user's request to discontinue participation in Staking (after stake is frozen and before staking ends) and reclaim their stake. It checks that a penalty fee is paid for withdrawing late from Staking but is still allowed. Additionally ensuring that removal happens without compromising the integrity of the linked list.
-**Pass - Claim Stake & Reward**: This test confirms that a user is able to successfully claim their stake and respective reward after the end of the staking period.

## Running the Tests

To execute the Staking unit tests for the project, you should follow the standard testing procedures outlined in the project documentation. Typically, this involves executing a command such as:

```sh
cabal new-test --test-show-details=streaming
```

This command will compile and execute all the test suites defined in the project. The output will show the status of each test case.

### Test Outcome Summary

In the most recent execution:

The tests for validating Staking actions such as Init, Deinit, Insert, Remove, Late Remove & Claim all passed. Ensuring the system appropriately handles these actions as a part of conducting a flawless Staking Event.

```markdown
Unit Test Group
  Staking Unit Test
    Pass - Init Staking:         OK (0.08s)
    Pass - Deinit Staking:       OK
    Pass - Insert Staking:       OK
    Pass - Remove Staking:       OK
    Pass - Late Remove Staking:  OK
    Pass - Claim Stake & Reward: OK

All 6 tests passed (0.09s)
```

A history of test execution results can be found on Github at [Single Asset Staking Tests](https://github.com/Anastasia-Labs/single-asset-staking/actions)

### Execution Time

The entire suite was executed in roughly 0.09 seconds, showcasing the Staking validation process's speed and efficiency.

## Conclusion

The unit tests for the Single Asset Staking project plays a vital role in verifying the integrity and functionality of the Staking implementation. By ensuring the accurate validation of actions associated with Staking event, these tests contribute significantly to maintaining the reliability, fairness and robustness of Single Asset Staking Implementation.