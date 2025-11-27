#' Plot Method for inbovegclassification Objects
#'
#' @description
#' Generic plot method that dispatches to `plot_classification_rose()`.
#' Visualizes the best matching syntaxa for a vegetation recording.
#'
#' @param x An object of class "inbovegclassification"
#' @param indextype Character string. The index to plot (e.g., "cod", "med", "likelihood").
#'   Defaults to "cod".
#' @param types Either a number indicating how many top types to show
#'   (default 12), or a character vector of syntaxon codes to display.
#' @param plot_type type of plot "rose" or "bar"
#' @param ... Additional arguments passed to `plot_classification_rose()`
#'
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 ggtitle
plot.inbovegclassification <- function(x,
                                       indextype = "cod",
                                       types = 12,
                                       plot_type = "rose",
                                       ...) {
  # Extract metadata
  recording <- x$recording_id

  # Validate index availability
  if (!indextype %in% colnames(x$results)) {
    stop(paste("Index", indextype, "not found in classification results."))
  }

  # Call the rose plot function
  if (plot_type == "rose") {
    p <- plot_classification_rose(
      x,
      indextype = indextype,
      types = types,
      ...
    ) +
      ggplot2::ggtitle(
        label = paste(recording),
        subtitle = paste("Index:", indextype)
      )
  } else if (plot_type == "bar") {
    p <- plot_classification_bar(
      x,
      indextype = indextype,
      types = types,
      ...
    ) +
      ggplot2::ggtitle(
        label = paste(recording),
        subtitle = paste("Index:", indextype)
      )
  } else {
    message("plot_type not recognised, choose between rose and bar")
    p <- NULL
  }

  return(p)
}

#' Plot Method for inbovegclassification_list Objects
#'
#' @description
#' Plots classification roses for multiple recordings, arranged in a grid.
#' Includes safeguards against printing too many pages.
#'
#' @param x An object of class "inbovegclassification_list".
#' @param ncol Integer. Number of columns in the plot grid. Default 2.
#' @param nrow Integer. Number of rows in the plot grid. Default 2.
#' @param max_pages Integer. Maximum number of pages allowed to print. Default 10.
#' @param force Logical. If TRUE, overrides the max_pages limit. Default FALSE.
#' @param ... arguments passed to `plot.inbovegclassification`.
#'
#' @return Prints plots to the active device and returns `invisible(NULL)`.
#' @export
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.interactive dev.flush
plot.inbovegclassification_list <- function(x, ncol = 2, nrow = 2,
                                            max_pages = 10, force = FALSE, ...) {
  if (length(x) == 0) {
    message("Empty classification list. Nothing to plot.")
    return(invisible(NULL))
  }

  plots_per_page <- ncol * nrow
  total_plots <- length(x)
  total_pages <- ceiling(total_plots / plots_per_page)

  # --- Safety Check ---
  if (total_pages > max_pages && !force) {
    stop(paste0(
      "This command will generate ", total_pages, " pages of plots.\n",
      "This exceeds the safety limit of ", max_pages, ".\n",
      "Use 'force = TRUE' to override this limit or filter your list first."
    ))
  }

  # --- Plotting Loop ---

  # Helper to handle interactive paging
  ask_user <- dev.interactive(orNone = TRUE) && total_pages > 1
  if (ask_user) {
    old_ask <- graphics::par(ask = TRUE)
    on.exit(graphics::par(old_ask))
  }

  for (i in seq_len(total_pages)) {
    # Calculate indices for this page
    start_idx <- (i - 1) * plots_per_page + 1
    end_idx <- min(i * plots_per_page, total_plots)

    # Create list of ggplot objects for this page
    page_plots <- lapply(x[start_idx:end_idx], function(obj) {
      plot(obj, ...) +
        # Tweak theme for small multiples: smaller text
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 10),
          plot.subtitle = ggplot2::element_text(size = 8)
        )
    })

    # Print the grid
    gridExtra::grid.arrange(grobs = page_plots, ncol = ncol, nrow = nrow)

    # If not interactive, we might want to force a flush to ensure plotting happens
    if (!dev.interactive()) grDevices::dev.flush()
  }

  invisible(NULL)
}

#' Create Classification Rose Plot
#'
#' @description
#' Creates a star-shaped radial plot ("classification rose") showing classification results.
#' The scaling is relative to the best match in the set.
#'
#' @param object An object of class `inbovegclassification` containing the results.
#' @param indextype Character string indicating the name of the index column to use.
#'   Default "cod" (Lower values = better match).
#' @param types Either a number indicating how many top types to show,
#'   or a character vector of syntaxon codes to display.
#' @param relative_scale Logical. If TRUE (default), the spoke length is calculated
#'   relative to the best match's score. If FALSE, it normalizes min-max within the subset.
#' @param sensitivity Numeric. Controls how quickly spoke length decays for worse matches
#'   when `relative_scale = TRUE`. Default 1.0 (linear ratio).
#'   Higher values (>1) punish bad matches more visible; values < 1 make differences subtler.
#' @param min_length Minimum length for normalization (default 0.1) to ensure visibility.
#' @param order Character. How to order the classes. One of "value" (default),
#'   "alphabetical", or "given".
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes geom_polygon geom_text geom_segment geom_path
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme_void theme element_text expansion margin
#' @importFrom dplyr slice_min filter arrange mutate select
#' @importFrom rlang .data sym
#' @export
plot_classification_rose <- function(object,
                                     indextype = "cod",
                                     types = 12,
                                     relative_scale = TRUE,
                                     sensitivity = 1.0,
                                     min_length = 0.2,
                                     order = c("value", "alphabetical", "given"),
                                     ...) {
  # --- 1. Data Prep ---
  data <- object$results

  # Ensure column 'syntaxon' exists (renamed from syntaxon_code/syntaxonCode)
  # If the input data still has old names, rename them on the fly for consistency
  if ("syntaxonCode" %in% names(data)) {
    data <- dplyr::rename(data, syntaxon = "syntaxonCode")
  } else if ("syntaxon_code" %in% names(data)) {
    data <- dplyr::rename(data, syntaxon = "syntaxon_code")
  }

  # Validate index column
  if (!indextype %in% names(data)) {
    stop(paste0("Index '", indextype, "' not found in data."))
  }
  index_col_sym <- rlang::sym(indextype)

  order <- match.arg(order)

  # --- 2. Filter and Order Data ---
  plot_data <- data

  if (is.numeric(types)) {
    # Select top N types (lowest values = best matches)
    plot_data <- plot_data |>
      dplyr::slice_min(order_by = !!index_col_sym, n = types, with_ties = FALSE)

    if (order == "alphabetical") {
      plot_data <- plot_data |> dplyr::arrange(.data$syntaxon)
    } else {
      # Default "value": best match first
      plot_data <- plot_data |> dplyr::arrange(!!index_col_sym)
    }
  } else if (is.character(types)) {
    # Select specific types
    plot_data <- plot_data |>
      dplyr::filter(.data$syntaxon %in% types)

    if (nrow(plot_data) == 0) stop("None of the specified types found.")

    if (order == "value") {
      plot_data <- plot_data |> dplyr::arrange(!!index_col_sym)
    } else if (order == "alphabetical") {
      plot_data <- plot_data |> dplyr::arrange(.data$syntaxon)
    } else if (order == "given") {
      plot_data <- plot_data |>
        dplyr::mutate(order_idx = match(.data$syntaxon, types)) |>
        dplyr::arrange(.data$order_idx) |>
        dplyr::select(-"order_idx")
    }
  }

  # --- 3. Calculate Spoke Length ---
  # Indices are distances (Lower = Better).
  # We want Longer Spoke = Better Match.

  # Safe values (avoid 0)
  raw_vals <- pmax(plot_data[[indextype]], 0.0001)

  if (relative_scale) {
    # --- Relative Scaling Strategy ---
    # The Best Match defines Length = 1.0.
    # Other matches are a ratio of the Best Score.
    # Formula: Length = (Best_Score / Current_Score) ^ sensitivity
    # If Current is 10% worse (1.1x Best), length is ~0.9.

    best_score <- min(raw_vals, na.rm = TRUE)

    # Calculate ratio (Best / Current). Since Best <= Current, this is <= 1.
    ratios <- best_score / raw_vals

    # Apply sensitivity and floor at min_length
    lengths <- ratios^sensitivity
    lengths <- pmax(lengths, min_length) # Ensure visibility

    plot_data$length <- lengths
  } else {
    # --- Min-Max Normalization (Old Method) ---
    # Scales the selected subset to fill the range [min_length, 1]
    # This emphasizes differences within the subset, even if they are tiny in absolute terms.

    inv_vals <- 1 / raw_vals
    min_inv <- min(inv_vals)
    max_inv <- max(inv_vals)

    if (abs(max_inv - min_inv) < 1e-9) {
      plot_data$length <- 1.0
    } else {
      norm_val <- (inv_vals - min_inv) / (max_inv - min_inv)
      plot_data$length <- min_length + (1 - min_length) * norm_val
    }
  }

  # --- 4. Calculate Coordinates ---
  n_points <- nrow(plot_data)

  # Start at 12 o'clock (pi/2) and go clockwise
  plot_data$angle <- seq(pi / 2, pi / 2 - 2 * pi, length.out = n_points + 1)[1:n_points]

  plot_data <- plot_data |>
    dplyr::mutate(
      x = .data$length * cos(.data$angle),
      y = .data$length * sin(.data$angle),
      # Label position: fixed radius circle (1.25)
      label_x = 1.25 * cos(.data$angle),
      label_y = 1.25 * sin(.data$angle),
      # Determine text alignment based on angle to avoid overlap
      # Right side of plot: left-align text. Left side: right-align.
      hjust = dplyr::case_when(
        .data$x > 0.05 ~ 0, # Right side
        .data$x < -0.05 ~ 1, # Left side
        TRUE ~ 0.5 # Center (top/bottom)
      )
    )

  # --- 5. Geometries ---
  # Spokes
  spokes_data <- plot_data |>
    dplyr::select("x", "y") |>
    dplyr::mutate(x_start = 0, y_start = 0)

  # Reference Circle (r=1)
  theta <- seq(0, 2 * pi, length.out = 100)
  circle_data <- data.frame(x = cos(theta), y = sin(theta))

  # --- 6. Plotting ---
  ggplot2::ggplot() +
    # Reference Circle (The "Best Match" Line)
    ggplot2::geom_path(
      data = circle_data, aes(x = .data$x, y = .data$y),
      color = "grey85", linetype = "solid"
    ) +
    # Star Polygon
    ggplot2::geom_polygon(
      data = plot_data,
      aes(x = .data$x, y = .data$y),
      fill = "lightblue", alpha = 0.5,
      color = "steelblue", linewidth = 0.8
    ) +
    # Spokes
    ggplot2::geom_segment(
      data = spokes_data,
      aes(x = .data$x_start, y = .data$y_start, xend = .data$x, yend = .data$y),
      color = "steelblue", linewidth = 0.4, linetype = "dotted"
    ) +
    # Labels
    ggplot2::geom_text(
      data = plot_data,
      aes(
        x = .data$label_x, y = .data$label_y,
        label = .data$syntaxon, hjust = .data$hjust
      ),
      size = 3, fontface = "bold", vjust = 0.5
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    # Expand limits to fit labels
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.4)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.4)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey40"),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}


##############################################################################

#' Bar Plot Method for inbovegclassification Objects
#'
#' @description
#' Creates a horizontal bar plot for a single vegetation classification object.
#' Dispatches to `plot_classification_bar()`.
#'
#' @param height An object of class "inbovegclassification".
#' @param indextype Character string. The index to plot (e.g., "cod", "med").
#'   Defaults to "cod".
#' @param types Number of top matches to show (default 20) or specific codes.
#' @param ... Additional arguments passed to `plot_classification_bar`.
#'
#' @return A ggplot object.
#' @export
barplot.inbovegclassification <- function(height, indextype = "cod", types = 20, ...) {
  # Extract metadata
  recording <- height$recording_id

  # Call the bar plot function
  p <- plot_classification_bar(
    height,
    indextype = indextype,
    types = types,
    ...
  ) +
    ggplot2::ggtitle(
      label = paste("Classification for:", recording),
      subtitle = paste("Index:", indextype, "(Lower value = Better match)")
    )

  return(p)
}

#' Bar Plot Method for inbovegclassification_list Objects
#'
#' @description
#' Plots bar charts for multiple recordings, arranged in a grid.
#' Includes safeguards against printing too many pages.
#'
#' @param height An object of class "inbovegclassification_list".
#' @param ncol Integer. Number of columns in the plot grid. Default 2.
#' @param nrow Integer. Number of rows in the plot grid. Default 2.
#' @param max_pages Integer. Maximum number of pages allowed. Default 10.
#' @param force Logical. If TRUE, overrides the max_pages limit.
#' @param ... arguments passed to `barplot.inbovegclassification`.
#'
#' @return Prints plots to active device; returns `invisible(NULL)`.
#' @export
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.interactive dev.flush
#' @importFrom graphics barplot
barplot.inbovegclassification_list <- function(height, ncol = 2, nrow = 2,
                                               max_pages = 10, force = FALSE, ...) {
  x <- height # standard generic arg name is height, internal logic uses x

  if (length(x) == 0) {
    message("Empty classification list. Nothing to plot.")
    return(invisible(NULL))
  }

  plots_per_page <- ncol * nrow
  total_plots <- length(x)
  total_pages <- ceiling(total_plots / plots_per_page)

  # --- Safety Check ---
  if (total_pages > max_pages && !force) {
    stop(paste0(
      "This command will generate ", total_pages, " pages of plots.\n",
      "This exceeds the safety limit of ", max_pages, ".\n",
      "Use 'force = TRUE' to override this limit."
    ))
  }

  # --- Plotting Loop ---
  ask_user <- dev.interactive(orNone = TRUE) && total_pages > 1
  if (ask_user) {
    old_ask <- graphics::par(ask = TRUE)
    on.exit(graphics::par(old_ask))
  }

  for (i in seq_len(total_pages)) {
    start_idx <- (i - 1) * plots_per_page + 1
    end_idx <- min(i * plots_per_page, total_plots)

    page_plots <- lapply(x[start_idx:end_idx], function(obj) {
      barplot(obj, ...) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 10),
          plot.subtitle = ggplot2::element_text(size = 8),
          axis.text.y = ggplot2::element_text(size = 7)
        )
    })

    gridExtra::grid.arrange(grobs = page_plots, ncol = ncol, nrow = nrow)
    if (!dev.interactive()) grDevices::dev.flush()
  }

  invisible(NULL)
}

#' Create Classification Bar Plot
#'
#' @description
#' Creates a horizontal bar plot showing classification distance scores.
#' Because these indices are distance metrics, **shorter bars** represent
#' **better matches**. The plot automatically sorts the best matches to the top.
#'
#' @param object An object of class `inbovegclassification` containing the results.
#' @param indextype Character string indicating the name of the index column to use.
#'   Default "cod".
#' @param types Either a number indicating how many top types to show,
#'   or a character vector of syntaxon codes to display.
#' @param order Character. How to order the classes. One of:
#'   \itemize{
#'     \item `"value"`: (Default) Best match (lowest value) is at the top.
#'     \item `"alphabetical"`: Alphabetical by syntaxon code (A at top).
#'     \item `"given"`: Order provided by the `types` vector.
#'   }
#' @param show_values Logical. If TRUE, adds text labels with the exact values
#'   at the end of the bars. Default FALSE.
#' @param fill_color Character. Color of the bars. Default "steelblue".
#' @param ... Additional arguments (not used).
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes geom_col coord_flip scale_x_discrete theme_minimal
#' @importFrom ggplot2 labs geom_text theme element_text expansion margin
#' @importFrom dplyr slice_min filter arrange mutate select rename
#' @importFrom rlang .data sym
#' @export
plot_classification_bar <- function(object,
                                    indextype = "cod",
                                    types = 20,
                                    order = c("value", "alphabetical", "given"),
                                    show_values = FALSE,
                                    fill_color = "steelblue",
                                    ...) {
  # --- 1. Data Prep ---
  data <- object$results

  # Handle column renaming consistency
  if ("syntaxonCode" %in% names(data)) {
    data <- dplyr::rename(data, syntaxon = "syntaxonCode")
  } else if ("syntaxon_code" %in% names(data)) {
    data <- dplyr::rename(data, syntaxon = "syntaxon_code")
  }

  # Validate index
  if (!indextype %in% names(data)) {
    stop(paste0("Index '", indextype, "' not found in data."))
  }
  index_col_sym <- rlang::sym(indextype)

  order <- match.arg(order)

  # --- 2. Filter and Order Data ---
  plot_data <- data

  if (is.numeric(types)) {
    # Select top N (lowest values)
    plot_data <- plot_data |>
      dplyr::slice_min(order_by = !!index_col_sym, n = types, with_ties = FALSE)

    # Define Factor Levels for Plot Ordering
    # In coord_flip(), the first factor level appears at the BOTTOM.
    # To put the Best (Lowest) Value at the TOP, it must be the LAST factor level.

    if (order == "value") {
      # Sort descending (Worst -> Best) so Best becomes last level -> Top of plot
      plot_data <- plot_data |> dplyr::arrange(dplyr::desc(!!index_col_sym))
      plot_data$syntaxon <- factor(plot_data$syntaxon, levels = plot_data$syntaxon)
    } else if (order == "alphabetical") {
      # Sort Z->A so A becomes last level -> Top of plot
      plot_data <- plot_data |> dplyr::arrange(dplyr::desc(.data$syntaxon))
      plot_data$syntaxon <- factor(plot_data$syntaxon, levels = plot_data$syntaxon)
    }
  } else if (is.character(types)) {
    plot_data <- plot_data |>
      dplyr::filter(.data$syntaxon %in% types)

    if (nrow(plot_data) == 0) stop("None of the specified types found.")

    if (order == "value") {
      plot_data <- plot_data |> dplyr::arrange(dplyr::desc(!!index_col_sym))
      plot_data$syntaxon <- factor(plot_data$syntaxon, levels = plot_data$syntaxon)
    } else if (order == "alphabetical") {
      plot_data <- plot_data |> dplyr::arrange(dplyr::desc(.data$syntaxon))
      plot_data$syntaxon <- factor(plot_data$syntaxon, levels = plot_data$syntaxon)
    } else if (order == "given") {
      # Reverse the given vector so first item -> last level -> top of plot
      plot_data$syntaxon <- factor(plot_data$syntaxon, levels = rev(types))
    }
  }

  # --- 3. Plotting ---
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$syntaxon, y = !!index_col_sym)) +
    ggplot2::geom_col(fill = fill_color, width = 0.7, alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = NULL, # No label for syntaxon axis
      y = paste("Distance (", indextype, ")")
    ) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(), # Cleaner look for bars
      axis.text.y = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = round(!!index_col_sym, 2)),
      hjust = -0.2,
      size = 3
    ) +
      # Extend y-axis slightly to fit labels
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
  }

  return(p)
}
