#' Create Classification Rose Plot
#'
#' @description
#' Creates a star-shaped radial plot showing classification results
#'
#' @param data A tibble with classification results
#' @param indextype Character string indicating the type of index
#' @param types Either a number indicating how many top types to show,
#'              or a character vector of syntaxon codes to display
#' @param min_length Minimum length for normalization (default 0.5)
#' @param order on which should the classes be ordered, on the best match,
#'  alphabetical or as given in the order of the types argument
#' @param ... Additional arguments passed to methods
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes coord_polar theme_void geom_polygon
#' @importFrom ggplot2 geom_text geom_segment geom_path ggtitle coord_fixed
#' @importFrom ggplot2 theme element_text
#' @importFrom dplyr slice_head bind_rows tibble
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' plot(results, n_types = 8)
#' selected_types <- c("type1", "type2", "type3")
#' plot(results, types = selected_types)
#' library(patchwork)
#' record1_plot <- plot(results1, types = selected_types)
#' record2_plot <- plot(results2, types = selected_types)
#' record1_plot + record2_plot
#' }
#'
plot_classification_rose <- #nog iets toevoegen om volledig proportioneel te tonen zodat 0.75 en 0.67 weinig schelen
  function(data,
           indextype,
           types = 12,
           min_length = 0.5,
           order = c("likelihood", "alphabetical", "given"), ...) {

    # Match order argument
  order <- match.arg(order)

  # Define index properties
  index_properties <- list(
    likelihood = list(
      column = "likelihood",
      inverse = FALSE,
      title = "Likelihood Index"
    )
    # Add other index types as needed
  )

  props <- index_properties[[indextype]]

  # Filter data based on types argument
  if (is.numeric(types)) {
    plot_data <- data |>
      slice_head(n = types)
  } else if (is.character(types)) {
    plot_data <- data |>
      filter(.data$syntaxonCode %in% types)

    # Different ordering based on 'order' parameter
    plot_data <- switch(order,
      "likelihood" = plot_data |>
        arrange(.data[[props$column]]),
      "alphabetical" = plot_data |> arrange(.data$syntaxonCode),
      "given" = plot_data |>
        mutate(order = match(.data$syntaxonCode, types)) |>
        arrange(order) |>
        select(-order)
    )

    if (nrow(plot_data) != length(types)) {
      missing_types <- setdiff(types, plot_data$syntaxonCode)
      warning(sprintf(
        "Some requested types not found in data: %s",
        paste(missing_types, collapse = ", ")
      ))
    }
  } else {
    stop("'types' must  a number or character vector of syntaxon codes")
  }

  # Get number of types for angle calculations
  n_types <- nrow(plot_data)

  # First rotate so best value is at top (pi/2)
  start_angle <- pi / 2

  # Prepare data for plotting
  plot_data <- plot_data |>
    mutate(
      value = if (props$inverse) {
        1 / .data[[props$column]]
      } else {
        .data[[props$column]]
      },
      # Normalize to min_length-1 scale
      length = min_length + (1 - min_length) *
        (.data$value - min(.data$value)) /
        (max(.data$value) - min(.data$value)),
      # Calculate angles with equal spacing, going clockwise from top
      angle = start_angle - seq(0, 2 * pi - 2 * pi / n_types,
                                length.out = n_types),
      # Calculate x and y coordinates for polygon
      x = length * cos(.data$angle),
      y = length * sin(.data$angle),
      # Calculate label position adjustments
      label_hjust = case_when(
        cos(angle) > 0.1 ~ 0, # right side
        cos(angle) < -0.1 ~ 1, # left side
        TRUE ~ 0.5 # center
      ),
      label_vjust = case_when(
        sin(angle) > 0.8 ~ 1, # top
        sin(angle) < -0.8 ~ 0, # bottom
        TRUE ~ 0.5 # middle
      ),
      # Variable label distance to prevent overlap
      label_dist = case_when(
        abs(sin(angle)) > 0.8 ~ 0.25, # more space at top/bottom
        TRUE ~ 0.2 # sides
      )
    )

  # Create polygon coordinates
  polygon_data <- plot_data |>
    select("x", "y") |>
    bind_rows(tibble(x = 0, y = 0)) # Add center point

  # Create spokes data
  spokes_data <- plot_data |>
    select("x", "y") |>
    mutate(
      x_start = 0,
      y_start = 0
    )

  # Create circle data using best value
  best_length <- max(plot_data$length)
  circle_data <- tibble(
    angle = seq(0, 2 * pi, length.out = 100),
    x = best_length * cos(.data$angle),
    y = best_length * sin(.data$angle)
  )

  # Calculate plot limits to ensure labels are visible
  max_length <- max(plot_data$length)
  plot_margin <- 0.4 # Increased margin for labels

  # Create the plot
  ggplot() +
    # Draw reference circle
    geom_path(
      data = circle_data,
      aes(x = .data$x, y = .data$y),
      color = "darkgreen",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    # Draw filled polygon
    geom_polygon(
      data = polygon_data,
      aes(x = .data$x, y = .data$y),
      fill = "lightgreen",
      color = "darkgreen",
      linewidth = 1
    ) +
    # Draw spokes from center to corners
    geom_segment(
      data = spokes_data,
      aes(
        x = .data$x_start, y = .data$y_start,
        xend = .data$x, yend = .data$y
      ),
      color = "darkgreen",
      linewidth = 0.5
    ) +
    # Add type labels with adjusted positions
    geom_text(
      data = plot_data,
      aes(
        x = (.data$length + .data$label_dist) * cos(.data$angle),
        y = (.data$length + .data$label_dist) * sin(.data$angle),
        label = .data$syntaxonCode,
        hjust = .data$label_hjust,
        vjust = .data$label_vjust)) +
    # Add title and axis limits
    ggtitle(props$title) +
    coord_fixed(
      ratio = 1,
      xlim = c(-(max_length + plot_margin), max_length + plot_margin),
      ylim = c(-(max_length + plot_margin), max_length + plot_margin)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}
