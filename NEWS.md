# portvine 1.0.1.9000

Development version

- Bugfix: Set the `shape` parameter for the inverse PIT transformation of the copula scale residuals and do not use the default.
- Bugfix: Allow for 0 orders in the utility function `default_garch_spec()`
- Bugfix: The residual calculation should allow for orders for the mean and variance models that are different than the standard (1,1) order

# portvine 1.0.1

- **D-vine ordering algorithm:** Now using the transform to the normal scale as suggested in Section 1.4 of the book *Dependence Modeling with Copulas* (2014) by Harry Joe.

# portvine 1.0.0

First stable package version.
