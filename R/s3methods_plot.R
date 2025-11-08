#' Plot Method for inbovegclassification Objects
#'
#' @description
#' Generic plot method that dispatches to `plot_classification_rose()`.
#'
#' @param x An object of class "inbovegclassification"
#' @param types Either a number indicating how many top types to show
#'   (default 12), or a character vector of syntaxon codes to display.
#' @param ... Additional arguments passed to `plot_classification_rose()`
#'
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 ggtitle
plot.inbovegclassification <- function(x, types = 12, ...) {
  indextype <- attr(x, "indextype")
  recording <- attr(x, "RecordingGivid")

  if (is.null(indextype)) {
    stop("Object missing 'indextype' attribute")
  }

  # Call the rose plot function
  p <- plot_classification_rose(x,
    indextype = indextype,
    types = types,
    ...
  ) +
    ggplot2::ggtitle(
      label = paste("Classification for:", recording),
      subtitle = paste("Method:", indextype)
    )

  return(p)
}

#' Plot Method for inbovegclassification_list Objects
#'
#' @description
#' Plots the classification rose for each relevé in the list.
#' In a non-interactive session, this will print plots one after another.
#' In an interactive session (like RStudio), it will show them sequentially.
#'
#' @param x list of classifications
#' @param ... arguments passed to `plot.inbovegclassification`
#'   and `plot_classification_rose`
#'
#' @return This function prints plots but returns `invisible(NULL)`.
#' @export
plot.inbovegclassification_list <- function(x, ...) {
  # This will print each plot.
  # For a grid, users can use packages like 'patchwork'
  invisible(lapply(names(x), function(name) {
    p <- plot(x[[name]], ...)
    print(p)
  }))
}

#' Create Classification Rose Plot
#'
#' @description
#' Creates a star-shaped radial plot ("classification rose") showing
#' classification results.
#'
#' @param data A tibble with classification results (an
#'   `inbovegclassification` object).
#' @param indextype Character string indicating the name of the index
#'   (e.g., "CoD", "Likelihood").
#' @param types Either a number indicating how many top types to show,
#'              or a character vector of syntaxon codes to display.
#' @param min_length Minimum length for normalization (default 0.5) to ensure
#'   visibility even for poor matches.
#' @param order On which should the classes be ordered? One of:
#'   \itemize{
#'     \item `"value"`: (Default) Best match (lowest value) is at the top.
#'     \item `"alphabetical"`: Clockwise, alphabetically by syntaxon code.
#'     \item `"given"`: Clockwise, in the order provided by the `types` vector.
#'   }
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes coord_polar theme_void geom_polygon
#' @importFrom ggplot2 geom_text geom_segment geom_path theme element_text
#' @importFrom ggplot2 expansion
#' @importFrom dplyr slice_min slice filter arrange mutate case_when
#' @importFrom dplyr bind_rows tibble select rename
#' @importFrom rlang .data
#' @export
plot_classification_rose <- function(data,
                                     indextype,
                                     types = 12,
                                     min_length = 0.5,
                                     order = c("value", "alphabetical", "given"),
                                     ...) {
  # Get the name of the index column
  index_col_name <- names(data)[3]
  index_col_sym <- rlang::sym(index_col_name)

  # Match order argument
  order <- match.arg(order)

  # --- 1. Filter and Order Data ---
  plot_data <- data

  if (is.numeric(types)) {
    # Select top N types, order by value
    n_types <- types
    plot_data <- plot_data |>
      dplyr::slice_min(order_by = !!index_col_sym, n = n_types)
    # If order is alphabetical, re-sort
    if (order == "alphabetical") {
      plot_data <- plot_data |> dplyr::arrange(.data$syntaxonCode)
    } else {
      # Default is "value", which is already done
      plot_data <- plot_data |> dplyr::arrange(!!index_col_sym)
    }
  } else if (is.character(types)) {
    # Select specific types
    plot_data <- plot_data |>
      dplyr::filter(.data$syntaxonCode %in% types)
    n_types <- nrow(plot_data)

    if (n_types == 0) {
      stop("None of the specified 'types' were found in the data.")
    }

    # Apply ordering
    plot_data <- switch(order,
      "value" = plot_data |> dplyr::arrange(!!index_col_sym),
      "alphabetical" = plot_data |> dplyr::arrange(.data$syntaxonCode),
      "given" = plot_data |>
        dplyr::mutate(order = match(.data$syntaxonCode, types)) |>
        dplyr::arrange(.data$order) |>
        dplyr::select(-.data$order)
    )
  } else {
    stop("'types' must be a number or a character vector of syntaxon codes")
  }

  # --- 2. Normalize and Calculate Coordinates ---
  # All indices are "lower is better"
  # We invert so "higher is better" for plotting (longer spoke = better match)
  plot_data <- plot_data |>
    dplyr::mutate(
      value_inv = 1 / pmax(!!index_col_sym, 0.0001)
    )

  # Normalize the inverted values
  min_val <- min(plot_data$value_inv, na.rm = TRUE)
  max_val <- max(plot_data$value_inv, na.rm = TRUE)

  # Avoid division by zero if all values are identical
  if (min_val == max_val) {
    plot_data$length <- 1.0
  } else {
    plot_data <- plot_data |>
      dplyr::mutate(
        length_norm = (.data$value_inv - min_val) / (max_val - min_val),
        length = .data$length_norm * (1 - min_length) + min_length
      )
  }

  # Calculate angles (clockwise from top)
  plot_data <- plot_data |>
    dplyr::mutate(
      angle = (pi / 2) - (seq(0, 2 * pi, length.out = n_types + 1)[-(n_types + 1)])
    )

  # Calculate x, y coordinates
  plot_data <- plot_data |>
    dplyr::mutate(
      x = .data$length * cos(.data$angle),
      y = .data$length * sin(.data$angle),
      label_x = 1.15 * cos(.data$angle), # Labels on a fixed circle
      label_y = 1.15 * sin(.data$angle)
    )

  # --- 3. Create Data for ggplot ---
  # Polygon for the "rose"
  polygon_data <- plot_data |>
    dplyr::select(.data$x, .data$y)

  # Spokes
  spokes_data <- plot_data |>
    dplyr::select(.data$x, .data$y) |>
    dplyr::mutate(x_start = 0, y_start = 0)

  # Reference circle (at max length = 1)
  circle_data <- dplyr::tibble(
    angle_circ = seq(0, 2 * pi, length.out = 100),
    x_circ = 1 * cos(.data$angle_circ),
    y_circ = 1 * sin(.data$angle_circ)
  )

  # --- 4. Build Plot ---
  ggplot2::ggplot(
    data = plot_data,
    aes(x = .data$x, y = .data$y)
  ) +
    # Reference circle
    ggplot2::geom_path(
      data = circle_data,
      aes(x = .data$x_circ, y = .data$y_circ),
      color = "grey70",
      linetype = "dashed",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    # Rose polygon
    ggplot2::geom_polygon(
      data = polygon_data,
      fill = "lightblue",
      color = "darkblue",
      linewidth = 1
    ) +
    # Spokes
    ggplot2::geom_segment(
      data = spokes_data,
      aes(
        x = .data$x_start, y = .data$y_start,
        xend = .data$x, yend = .data$y
      ),
      color = "darkblue",
      linewidth = 0.5
    ) +
    # Type labels
    ggplot2::geom_text(
      aes(x = .data$label_x, y = .data$label_y, label = .data$syntaxonCode),
      hjust = 0.5, vjust = 0.5, # Center-aligned, position is controlled by x/y
      size = 3
    ) +
    ggplot2::coord_polar() +
    # Set fixed scale to ensure circle is circular and labels fit
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.3)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.3)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}
