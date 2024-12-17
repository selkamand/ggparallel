fct_relevel_base <- function(x, ..., after = 0) {
  if (!is.factor(x)) {
    stop("`x` must be a factor.")
  }

  # Levels to move, provided as ...
  to_move <- c(...)

  # Original levels
  orig_levels <- levels(x)

  # Check if all levels to move are indeed present
  if (!all(to_move %in% orig_levels)) {
    missing_levels <- to_move[!to_move %in% orig_levels]
    stop("The following levels are not present in `x`: ", paste(missing_levels, collapse = ", "))
  }

  # Remove the specified levels from the original ordering
  remaining_levels <- orig_levels[!orig_levels %in% to_move]

  # The `after` parameter controls where to insert.
  # `after = 0` means put at the front, `after = length(remaining_levels)` means put at the end.
  # You can also use negative values if you like, but let's assume non-negative for simplicity.

  # Insert `to_move` after the `after` position
  # Note: `append(x, values, after = pos)` inserts values after position `pos` in `x`.
  new_levels <- append(remaining_levels, to_move, after = after)

  # Re-factor `x` with the new level order
  factor(x, levels = new_levels)
}


#' Count Edge Crossings in Parallel Coordinates
#'
#' This function calculates the total number of edge crossings in a 2-column
#' parallel coordinates plot. Each axis represents one of the columns, and
#' edges (lines) connect corresponding points between the two axes. An edge
#' crossing occurs when two edges intersect between the axes.
#'
#' @param l A numeric vector representing values on the left axis. Must have
#'          the same length as `r`.
#' @param r A numeric vector representing values on the right axis. Must have
#'          the same length as `l`.
#'
#' @return An integer value indicating the total number of edge crossings
#'         between the two axes.
#'
#' @details The function checks all pairs of edges formed by the points in `l`
#'          and `r` to determine whether they cross. Two edges connecting
#'          points (l[i], r[i]) and (l[j], r[j]) are said to cross if:
#'          \deqn{(l[i] - l[j]) * (r[i] - r[j]) < 0}
#'
#'          The computational complexity of this function is O(n^2) due to the
#'          pairwise comparison of all edges. It is suitable for datasets with
#'          a moderate number of points.
#'
#' @export
#'
#' @examples
#' # Example with no crossings
#' left <- c(1, 2, 3)
#' right <- c(1, 2, 3)
#' count_edge_crossings(left, right) # Returns 0
#'
#' # Example with one crossing
#' left <- c(1, 3)
#' right <- c(3, 1)
#' count_edge_crossings(left, right) # Returns 1
#'
#' # Example with multiple crossings
#' left <- c(1, 2, 3)
#' right <- c(3, 1, 2)
#' count_edge_crossings(left, right) # Returns 2
count_edge_crossings <- function(l, r){
  if(length(l) != length(r)) stop("Length of two axis must be equal to compute edge-crossings")

  n_points <- length(l)

  # Get every unique combination of 2 indices
  ls_combos <- combn(x=seq_len(n_points), m = 2, simplify = FALSE)

  # For each two indices, check whether the coordinates are crossing
  crossings <- vapply(X = ls_combos, FUN = function(c_index){
    index1 <- c_index[1]
    index2 <- c_index[2]
    l1 <- l[index1]
    r1 <- r[index1]
    l2 <- l[index2]
    r2 <- r[index2]
    is_crossing(l1, r1, l2, r2)
    }, FUN.VALUE = logical(1))

  # Return the number of crossing events
  return(sum(crossings))
}

#' Count Edge Crossings for All Numeric Column Pairs
#'
#' This function calculates the total number of edge crossings between all pairs
#' of numeric columns in a given dataset. For each pair of columns, it determines
#' the number of times edges cross in a parallel coordinates plot representation.
#'
#' @param data A `data.frame` or `tibble` containing the dataset. Only numeric
#'        columns will be considered for edge crossing calculations; non-numeric
#'        columns are ignored.
#'
#' @return A `data.frame` with three columns:
#' \describe{
#'   \item{col1}{The name of the first column in the pair.}
#'   \item{col2}{The name of the second column in the pair.}
#'   \item{crossings}{The total number of edge crossings between the two columns.}
#' }
#'
#' @details The function:
#' \enumerate{
#'   \item Filters the input data to retain only numeric columns.
#'   \item Computes all possible pairs of numeric columns.
#'   \item For each pair, uses the `count_edge_crossings` function to calculate
#'         the total number of crossings.
#'   \item Returns the results in a summarized `data.frame`.
#' }
#'
#' Non-numeric columns are ignored automatically. If no numeric columns exist
#' in the dataset, the function will return an empty `data.frame`.
#'
#' @export
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   x = c(1, 2, 3, 4),
#'   y = c(4, 3, 2, 1),
#'   z = c(2, 3, 4, 1),
#'   group = c("A", "B", "C", "D") # non-numeric column
#' )
#'
#' # Compute crossings
#' count_all_edge_crossings(df)
#
count_all_edge_crossings <- function(data){
  # for (col in colnames(data)) {assertions::assert_numeric(data[[col]], msg = "Edge crossing can only be computed for numeric variables. {col} is of type [{class(col)}], not {.strong numeric}")}
  data <- data[,vapply(data, is.numeric, logical(1))]


  ls_column_pairs <- combn(colnames(data), m = 2, simplify = FALSE)

  crossings <- vapply(ls_column_pairs, function(df_pair_colnames){
      res <- count_edge_crossings(data[[df_pair_colnames[1]]], data[[df_pair_colnames[2]]])
      return(res)
    }, numeric(1))

  df_column_pairs <- as.data.frame(do.call(rbind, ls_column_pairs))
  colnames(df_column_pairs) <- c("col1", "col2")

  df_column_pairs[["crossings"]] <- crossings
  return(df_column_pairs)
}

#' Determine whether a pair of points are crossing in parallel coords
#'
#' @param l1 position of line1 on left axis (number)
#' @param r1 position of line1 on right axis (number)
#' @param l2 position of line2 on left axis (number)
#' @param r2 position of line2 on right axis (number)
#'
#' @return boolean. TRUE if coordinates cross, FALSE if they do not
#'
#' @examples
#' is_crossing()
is_crossing <- function(l1, r1, l2, r2){
  l1 > l2 & r1 < r2 | l1 < l2 & r1 > r2
}

# Solve
optimise_axis_ordering <- function(mx, method = c("branch_and_bound")){
  method <- rlang::arg_match(method)
}

crossings_df_to_dist_matrix <- function(df){
  mx <- structure(df$crossings, class = 'dist',
            Size = Re(polyroot(c(-2*length(df$crossings),-1,1))[1]))
  colnames(mx) <- df$col1
  rownames(mx) <- df$col2
  return(mx)
}

fct_infreq <- function(x) {
  # Ensure x is a factor
  f <- as.factor(x)

  # Get frequency counts for each level
  freq <- table(f)

  # Reorder levels by frequency (descending order)
  new_levels <- names(sort(freq, decreasing = TRUE))

  # Re-factor with the new level order
  factor(f, levels = new_levels)
}

fct_rev <- function(x) {
  f <- as.factor(x)
  factor(f, levels = rev(levels(f)))
}


