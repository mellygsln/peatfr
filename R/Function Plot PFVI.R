plotpfvi <- function(PFVI, h) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  library(ggplot2)

  Time <- seq_along(PFVI)
  df_plot <- data.frame(
    Time = Time,
    PFVI = PFVI
  )

  df_plot_line <- df_plot[1:(length(Time) - h), ]
  df_plot_dot <- df_plot[(length(Time) - h + 1):length(Time), ]

  ggplot() +
    geom_line(data = df_plot_line, aes(x = Time, y = PFVI, color = "PFVI"), linewidth = 1) +
    geom_point(data = df_plot_dot, aes(x = Time, y = PFVI, color = "PFVI Predict"), size = 2) +
    labs(
      x = "Time",
      y = "PFVI",
      title = "PFVI and PFVI Predict Plot"
    ) +
    scale_color_manual(values = c(
      "PFVI" = "blue",
      "PFVI Predict" = "red"
    )) +
    ylim(-100, 400) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )
}
