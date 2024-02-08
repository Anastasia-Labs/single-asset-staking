# Single Asset Staking Tests Documentation

This page offers a comprehensive guide to the Single Asset Staking Tests within the project in (`./Spec/StakingSpec.hs`). The tests are meticulously designed to assess the accuracy and efficiency of the Single Asset Staking implementation, particularly focusing on the validation of actions involved during the staking  process.

## Overview

The unit tests and property based tests for the Single Asset Staking are organized into a suite titled "Single Asset Staking Tests" This suite is dedicated to verifying the validation process for the various actions involved in orchestrating and participating in staking.

## Test Suite Details

### Unit Tests Included

- **Pass - Init Staking**: This test confirms the proper initialization of Staking by creating of a head node for the linked list which will contain all the staking requests and stake  amount.
- **Pass - Deinit Staking**: This confirms correct working validation of deinitization of head node once the staking rewards have been processed correctly.
- **Pass - Insert Staking**: Aims to validate a new user's request to participate in Staking, confirming its correct positioning and validation.
- **Pass - Remove Staking**: Aims to validate an existing user's request to discontinue participation (before stake is frozen) in Staking and reclaim their stake. It also checks that the removal happens without compromising the integrity of the linked list.
- **Pass - Late Remove Staking**: Aims to validate an existing user's request to discontinue participation in Staking (after stake is frozen and before staking ends) and reclaim their stake. It checks that a penalty fee is paid for withdrawing late from Staking but is still allowed. Additionally ensuring that removal happens without compromising the integrity of the linked list.
- **Pass - Claim Stake & Reward**: This test confirms that a user is able to successfully claim their stake and respective reward after the end of the staking period.

### Property Based Tests Included

- **Valid Node Check**: This property based test confirms that only valid nodes are added to the linked list maintaing all the active stake. It does so by programmatically generating **100** test scenarios with different node values and checking they are indeed valid ones. Since the whole test suit is ran automatically with each commit, this means that thousands (if not millions) of test cases are tried during the whole lifetime of the project.

## Running the Tests

To execute the tests for the project, you should follow the standard testing procedures outlined in the project documentation. Typically, this involves executing a command such as:

```sh
cabal new-test --test-show-details=streaming
```

or (which prints out the dataset generated for property based tests)

```sh
cabal new-test --test-show-details=streaming --test-option=--quickcheck-verbose
```

This command will compile and execute all the test suites defined in the project. The output will show the status of each test case.

### Test Outcome Summary

In the most recent execution:

The tests for validating Staking actions such as Init, Deinit, Insert, Remove, Late Remove & Claim all passed. It also guaranteed that nodes are valid using Valid Node Check. Ensuring the system appropriately handles these actions as a part of conducting a flawless Staking Event.

```markdown
Running 1 test suites...
Test suite single-asset-staking-test: RUNNING...
Single Asset Staking Tests
  Unit Tests
    Pass - Init Staking:         OK (0.08s)
    Pass - Deinit Staking:       OK
    Pass - Insert Staking:       OK
    Pass - Remove Staking:       OK
    Pass - Late Remove Staking:  OK
    Pass - Claim Stake & Reward: OK
  Property Based Tests
    Valid Node Check:            OK (0.05s)
      +++ OK, passed 100 tests.

All 7 tests passed (0.13s)
```

Note: The datasets are generated randomly for each of the 100 tests, every time the tests are run. Dataset belonging to this test outcome summary can be found [here](../assets/test-reports/valid-node.txt)

A history of test execution results can be found on Github at [Single Asset Staking Tests](https://github.com/Anastasia-Labs/single-asset-staking/actions)

### Execution Time

The entire suite was executed in roughly 0.13 seconds, showcasing the Staking validation process's speed and efficiency.

## Conclusion

The unit tests for the Single Asset Staking project plays a vital role in verifying the integrity and functionality of the Staking implementation. By ensuring the accurate validation of actions associated with Staking event, these tests contribute significantly to maintaining the reliability, fairness and robustness of Single Asset Staking Implementation.