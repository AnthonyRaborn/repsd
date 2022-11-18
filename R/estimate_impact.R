#' Estimate the effect size difference between focal and composite group abilities
#'
#' @param responses The `data.frame` of responses, including the `focal_column`.
#' @param focal_column The `numeric` location of the focal column.
#' @param focal_id The `numeric`, `character`, or `logical` value that identifies
#' the focal group.
#'
#' @return A `numeric` estimate of the impact as the effect size *D*, e.g., the
#' standardized mean theta difference between the focal group and the composite
#' (total) group abilities.
#'
#' @importFrom stats var
#' @export

estimate_impact <-
  function(
    responses = timmsData,
    focal_column = 21,
    focal_id = 1) {

    response_matrix = responses[, -focal_column]

    composite_ss = nrow(responses)
    focal_ss     = sum(responses[, focal_column] == focal_id)

    composite_mean = rowSums(response_matrix) |>
      mean()
    composite_var  = rowSums(response_matrix) |>
      var()

    focal_mean = rowSums(response_matrix[responses[, focal_column] == focal_id, ]) |>
      mean()
    focal_var  = rowSums(response_matrix[responses[, focal_column] == focal_id, ]) |>
      var()

    pooled_sd  =
      sqrt(
        ((composite_ss - 1) * composite_var + (focal_ss - 1) * focal_var) /
          (composite_ss + focal_ss - 2)
      )

    standardized_focal_mean =
      ((focal_mean - composite_mean) /
         pooled_sd) |>
      round(digits = 3)

    return(standardized_focal_mean)
  }
