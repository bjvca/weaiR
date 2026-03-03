#' Plot a weai_result object
#'
#' Creates a stacked bar chart showing the absolute contribution of each
#' indicator to disempowerment, for women and men.
#'
#' @param x A \code{weai_result} object.
#' @param ... Additional arguments (ignored).
#' @return A ggplot object (invisibly), or a base R plot if ggplot2 is unavailable.
#' @export
plot.weai_result <- function(x, ...) {
  decomp_f <- x$decomposition$female
  decomp_m <- x$decomposition$male

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    # Fallback: base R grouped bar plot
    mat <- rbind(decomp_f$abs_contribution, decomp_m$abs_contribution)
    colnames(mat) <- decomp_f$indicator
    rownames(mat) <- c("Women", "Men")
    barplot(mat, beside = TRUE, las = 2,
            main = "Absolute contribution to disempowerment",
            ylab = "Contribution",
            legend.text = TRUE,
            args.legend = list(x = "topright"))
    return(invisible(NULL))
  }

  # Build long-format data
  df_f <- data.frame(
    indicator = decomp_f$indicator,
    domain = decomp_f$domain,
    contribution = decomp_f$abs_contribution,
    sex = "Women",
    stringsAsFactors = FALSE
  )
  df_m <- data.frame(
    indicator = decomp_m$indicator,
    domain = decomp_m$domain,
    contribution = decomp_m$abs_contribution,
    sex = "Men",
    stringsAsFactors = FALSE
  )
  plot_df <- rbind(df_f, df_m)
  plot_df$indicator <- factor(plot_df$indicator,
                              levels = rev(decomp_f$indicator))

  p <- ggplot2::ggplot(plot_df,
                       ggplot2::aes(x = .data$sex, y = .data$contribution,
                                    fill = .data$indicator)) +
    ggplot2::geom_bar(stat = "identity", position = "stack", width = 0.6) +
    ggplot2::scale_y_continuous(
      labels = function(x) formatC(x, format = "f", digits = 2)
    ) +
    ggplot2::labs(
      title = "Absolute contribution of each indicator to disempowerment",
      x = NULL,
      y = "Disempowerment index (1 - DE)",
      fill = "Indicator"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", size = 12)
    )

  print(p)
  invisible(p)
}

# Avoid R CMD check NOTE about .data
utils::globalVariables(".data")
