# Create an AddiVortes Object

A constructor for the AddiVortes class.

## Usage

``` r
new_AddiVortes(
  posteriorTess,
  posteriorDim,
  posteriorSigma,
  posteriorPred,
  xCentres,
  xRanges,
  yCentre,
  yRange,
  inSampleRmse,
  metric = "E",
  members = rep(1, length(xCentres)),
  catEncoding = NULL
)
```

## Arguments

- posteriorTess:

  A list of the posterior samples of the tessellations.

- posteriorDim:

  A list of the posterior samples of the dimensions.

- posteriorSigma:

  A list of the posterior samples of the error variance.

- posteriorPred:

  A list of the posterior samples of the predictions.

- xCentres:

  The centres of the covariates.

- xRanges:

  The ranges of the covariates.

- yCentre:

  The centre of the output values.

- yRange:

  The range of the output values.

- inSampleRmse:

  The in-sample RMSE.

- metric:

  The metric used for scaling covariates (default "E" for Euclidean).

- members:

  The membership vector for the covariates

- catEncoding:

  Optional list of categorical encoding metadata returned by
  `encodeCategories_internal`, or `NULL` if no categorical covariates
  were present.

## Value

An object of class AddiVortes.
