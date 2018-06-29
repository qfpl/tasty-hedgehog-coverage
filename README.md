<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

# Tasty Hedgehog Coverage

[![Build Status](https://travis-ci.org/qfpl/tasty-hedgehog-coverage.svg?branch=master)](https://travis-ci.org/qfpl/tasty-hedgehog-coverage)

An addon for the property-based testing library that can provide a report on the
break down of inputs. Similar to the `classify` function in [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Property.html#v:classify).

There must be a better way of wiring the layers together to avoid the need for
the custom `Cover` type?

### TODO

- [ ] More seamlessly integrate with Hedgehog to retire `Cover` type.
- [ ] Better solution for boolean blindness?
- [ ] Allow for retrying of tests if thresholds are not met?
- [x] No nice way to modify `Property` configuration yet.

```haskell
prop_reverse_involutive :: Cover
prop_reverse_involutive = withCoverage $ do
  xs <- forAll genAlphaList

  classify (length xs > 50) "non-trivial"

  test_involutive reverse xs
  
main :: IO ()
main =
  defaultMain $
  testGroup "tasty-hedgehog-coverage tests"
    [ testPropertyCoverage
        "reverse involutive"
        prop_reverse_involutive
    ]
```

Provides output similar to the following:

```
Test suite tasty-hedgehog-coverage-tests: RUNNING...
tasty-hedgehog tests
  reverse involutive:          OK
    17.00% non-trivial
```

This allows you to see a distribution of the inputs when using property-based
testing. Which may help craft better generators or see if failing test cases are
all targeting a specific subset of your inputs.
