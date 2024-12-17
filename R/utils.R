
coltypes <- function(data, col_id) {
  # First Pass of coltypes
  char_coltypes <- vapply(data, FUN = function(vec) {
    if (is.character(vec) | is.factor(vec) | is.logical(vec)) {
      return("categorical")
    } else if (is.numeric(vec)) {
      return("numeric")
    } else {
      return("invalid")
    }
  }, FUN.VALUE = character(1))

  # Overwrite coltype to tooltip if `_tooltip` suffix is found
  char_colnames <- colnames(data)
  tooltip_suffix <- "_tooltip"
  has_tooltip_in_name <- grepl(x = char_colnames, pattern = tooltip_suffix, ignore.case = TRUE)
  without_tooltip_has_matched_name <- gsub(x = char_colnames, pattern = tooltip_suffix, replacement = "") %in% char_colnames
  is_tooltip_col <- has_tooltip_in_name & without_tooltip_has_matched_name

  char_coltypes <- ifelse(is_tooltip_col, yes = "tooltip", no = char_coltypes)

  # Overwrite coltype to 'id' if colname == col_id
  char_coltypes <- ifelse(char_colnames == col_id, yes = "id", no = char_coltypes)

  return(char_coltypes)
}

colvalues <- function(data) {
  vapply(data, FUN = function(vec) {
    length(stats::na.omit(unique(vec)))
  }, FUN.VALUE = numeric(1))
}

choose_colours <- function(data, palettes, plottable, ndistinct, coltype, colours_default, colours_default_logical) {
  assertions::assert_character(colours_default)

  colors <- lapply(seq_len(ncol(data)), FUN = function(i) {
    colname <- colnames(data)[[i]]
    is_plottable <- plottable[[i]]
    is_lgl <- is.logical(data[[colname]])

    if (!is_plottable | coltype[i] != "categorical") {
      return(NULL)
    } else if (colname %in% names(palettes)) {
      colors <- unlist(palettes[[colname]])
      assertions::assert_names_include(colors, names = stats::na.omit(unique(data[[colname]])))
      return(palettes[[colname]])
    } else if (is_lgl) {
      colors <- colours_default_logical
    } else {
      assertions::assert(length(colours_default) >= ndistinct[i], msg = "Too many unique values in column to assign each a colour using the default palette. Either change the default palette to one that supports colours, reduce the number of levels in this column, or exclude it from the plotting using `cols_to_plot` argument OR maxlevels")
      colors <- colours_default
    }
  })

  return(colors)
}





theme_categorical <- function(fontsize_y_title = 12, show_legend = TRUE, show_legend_titles = FALSE, legend_position = "right", legend_title_size = NULL, legend_text_size = NULL, legend_key_size = 0.3, vertical_spacing = 0) {
  ggplot2::theme_minimal() %+replace%

    ggplot2::theme(
      panel.grid = element_blank(),
      axis.text.y.left = element_blank(),
      axis.text.y.right = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = fontsize_y_title, angle = 0),
      legend.title = if (show_legend_titles) element_text(size = legend_title_size, face = "bold", hjust = 0) else element_blank(),
      legend.justification = c(0, 0.5),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.location = "panel",
      legend.text = element_text(size = legend_text_size, vjust = 0.5),
      legend.position = if (show_legend) legend_position else "none",
      strip.placement = "outside",
      # plot.background = ggplot2::element_rect(color = "red"),
      # panel.background = ggplot2::element_rect(color = "black"),
      # legend.box.background = ggplot2::element_rect(color = "green"),
      # legend.key = ggplot2::element_rect(colour = "red"),
      legend.key.size = ggplot2::unit(legend_key_size, "lines"),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.key.spacing.y = ggplot2::unit(2, "pt"),
      plot.margin = ggplot2::margin(t = 0, r = 0, b = vertical_spacing, l = 0, unit = "pt"),
    )
}

theme_numeric_bar <- function(vertical_spacing = 0, fontsize_barplot_y_numbers = 8) {
  ggplot2::theme_minimal() %+replace%

    theme(
      panel.grid = element_blank(),
      axis.title.y.right = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.line.y = element_line(linewidth = 0.3),
      axis.line.x = element_blank(),
      axis.text.y.left = ggtext::element_markdown(size = fontsize_barplot_y_numbers, colour = "black"),
      axis.text.y.right = ggtext::element_markdown(size = fontsize_barplot_y_numbers, hjust = 0),
      axis.ticks.y = element_blank(),
      strip.placement = "outside",
      plot.margin = ggplot2::margin(t = 5, r = 0, b = vertical_spacing + 5, l = 0, unit = "pt")
    )
}

theme_numeric_heatmap <- function(fontsize_y_title = 12, show_legend = TRUE, legend_position = "right", show_legend_titles = FALSE, legend_title_size = NULL, legend_text_size = NULL, legend_key_size = 0.3, vertical_spacing = 0) {
  ggplot2::theme_minimal() %+replace%
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_text(size = fontsize_y_title, angle = 0, colour = "black"),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = if (show_legend_titles) element_text(size = legend_title_size, face = "bold", hjust = 0) else element_blank(),
      legend.justification = c(0, 0.5),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.text = element_text(size = legend_text_size),
      legend.position = if (show_legend) legend_position else "none",
      strip.placement = "outside",
      plot.margin = ggplot2::margin(t = 0, r = 0, b = vertical_spacing, l = 0, unit = "pt")
    )
}


tag_bold <- function(x) {
  paste0("<b>", x, "</b>", collapse = "")
}

#' GGplot breaks
#'
#' Find sensible values to add 2 breaks at for a ggplot2 axis
#'
#' @param vector vector fed into ggplot axis you want to define sensible breaks for
#'
#' @return vector of length 2. first element descripts upper break position, lower describes lower break
#'
sensible_2_breaks <- function(vector) {
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)
  c(upper, lower)
}

sensible_3_breaks <- function(vector, digits = 3) {
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)

  # Round
  if (!is.null(digits)) upper <- round_up(upper, digits)
  if (!is.null(digits)) lower <- round_down(lower, digits)

  middle <- mean(c(upper, lower))

  breaks <- c(upper, middle, lower)

  if (upper == lower) {
    return(lower)
  }

  return(breaks)
}

sensible_3_labels <- function(vector, axis_label, fontsize_y_title = 14, digits = 3) {
  upper <- max(vector, na.rm = TRUE)
  lower <- min(0, min(vector, na.rm = TRUE), na.rm = TRUE)


  # Round
  if (!is.null(digits)) upper <- round_up(upper, digits)
  if (!is.null(digits)) lower <- round_down(lower, digits)

  axis_label <- paste0("<span style = 'font-size: ", fontsize_y_title, "pt'>", axis_label, "</span>")

  if (lower == upper) {
    return(axis_label)
  }

  as.character(c(upper, axis_label, lower))
}


#' Make strings prettier for printing
#'
#' Takes an input string and 'beautify' by converting underscores to spaces and
#'
#' @param string input string
#' @param autodetect_units automatically detect units (e.g. mm, kg, etc) and wrap in brackets.
#'
#' @return string
#'
beautify <- function(string, autodetect_units = TRUE) {
  # underscores to spaces
  string <- gsub(x = string, pattern = "_", replacement = " ")

  # dots to spaces
  string <- gsub(x = string, pattern = ".", replacement = " ", fixed = TRUE)

  # camelCase to camel Case
  string <- gsub(x = string, pattern = "([a-z])([A-Z])", replacement = "\\1 \\2")

  # Autodetect units (and move to brackets)
  if (autodetect_units) {
    string <- sub("\\sm(\\s|$)", " (m)", string)
    string <- sub("\\smm(\\s|$)", " (mm)", string)
    string <- sub("\\sm(\\s|$)", " (cm)", string)
    string <- sub("\\sm(\\s|$)", " (km)", string)
    string <- sub("\\sg(\\s|$)", " (g)", string)
    string <- sub("\\skg(\\s|$)", " (kg)", string)
    string <- sub("\\smg(\\s|$)", " (mm)", string)
    string <- sub("\\soz(\\s|$)", " (oz)", string)
    string <- sub("\\slb(\\s|$)", " (lb)", string)
    string <- sub("\\sin(\\s|$)", " (in)", string)
    string <- sub("\\sft(\\s|$)", " (ft)", string)
    string <- sub("\\syd(\\s|$)", " (yd)", string)
    string <- sub("\\smi(\\s|$)", " (mi)", string)
  }


  # Capitalise Each Word
  string <- gsub(x = string, pattern = "^([a-z])", perl = TRUE, replacement = ("\\U\\1"))
  string <- gsub(x = string, pattern = " ([a-z])", perl = TRUE, replacement = (" \\U\\1"))

  return(string)
}

round_up <- function(x, digits) {
  multiplier <- 10^digits
  ceiling(x * multiplier) / multiplier
}

round_down <- function(x, digits) {
  multiplier <- 10^digits
  floor(x * multiplier) / multiplier
}

fct_inorder <- function(x){
  factor(x, levels = unique(x))
}


# Autoconvert pseudo-logical to a factor
numeric_only_includes_zero_one_and_na <- function(vec){
  is.numeric(vec) & all(unique(vec) %in% c(0, 1, NA))
}

convert_numerics_with_only_values_0_1_and_NA_to_logicals <- function(data, exclude = NULL) {
  col_is_convertable <- vapply(data, numeric_only_includes_zero_one_and_na, FUN.VALUE = logical(1))

  if(!is.null(exclude))
    col_is_convertable <- col_is_convertable & (colnames(data) != exclude)

  data[, col_is_convertable] <- lapply(data[, col_is_convertable, drop = FALSE], as_binary_factor)
  return(data)
}

as_binary_factor <- function(vec){
  vec <- as.factor(vec)
  levels(vec) <- c(0, 1, NA)
  return(vec)
}
