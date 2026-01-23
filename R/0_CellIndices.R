#' @title Assign Observations to Tessellation Cells
#' @description For a given tessellation, this function identifies which cell
#'   (centre) each observation belongs to based on nearest neighbour classification.
#'
#' @details It finds the closest tessellation centre for each observation (row) in
#'   the covariate matrix, considering only the specified dimensions. This is
#'   achieved using the k-nearest neighbour algorithm where k=1.
#'
#' @param x A numeric matrix of covariates where each row is an observation.
#' @param tess A numeric matrix representing the tessellation centres, where each
#'   row is a unique centre.
#' @param dim An integer vector specifying the column indices of `x` to be used
#'   for calculating distance.
#'
#' @return A numeric vector of integers where each element corresponds to a row
#'   in `x` and its value is the row index of the nearest centre in `tess`.
#'
#' @keywords internal
#' @noRd
cellIndices <- function(x, tess, dim, metric = "Euclidean") {
  if (length(tess[, 1]) == 1) { # only 1 centre
    CellsForGivenTess <- rep(1, length(x[, 1]))
  } else { # multiple
    if (ncol(tess) != ncol(x)) {
      new_tess <- matrix(0, nrow = nrow(tess), ncol = ncol(x))
      for (i in seq_along(dim)) {
        new_tess[,dim[i]] <- tess[,i];
      }
      tess <- new_tess
    }
    CellsForGivenTess <- knnx_index(tess, 
                                    x, 1,
                                    dim,
                                    metric
    )
  }
} # Implicit return of CellsForGivenTess
