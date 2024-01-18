# portvine 1.0.3.9000

Current Development Version


# portvine 1.0.3 

- Clarify what is meant as equally weighted portfolio in the documenation of the function `estimate_risk_roll`
- **Dependency management:** Due to the changes in the `BH` packages I dropped the system requirement `C++11` as suggested by the CRAN maintainers.

# portvine 1.0.2

- Bugfix: Set the `shape` parameter for the inverse PIT transformation of the copula scale residuals and do not use the default.
- Bugfix: Allow for 0 orders in the utility function `default_garch_spec()`
- Bugfix: The residual calculation should allow for orders for the mean and variance models that are different than the standard (1,1) order

# portvine 1.0.1

- **D-vine ordering algorithm:** Now using the transform to the normal scale as suggested in Section 1.4 of the book *Dependence Modeling with Copulas* (2014) by Harry Joe.

# portvine 1.0.0

First stable package version.
