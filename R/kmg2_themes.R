kmg2_theme_gray <- function(base_size = 25, base_family = "") {

  mod <- list(
    axis.title.x = theme_text(family = base_family, size = base_size, vjust = -0.5),
    axis.title.y = theme_text(family = base_family, size = base_size, angle = 90,
      vjust = 0.25),
    axis.ticks.margin = unit(0.2, "cm"),
    legend.key.size = unit(1.5, "lines"),
		legend.background = theme_blank(),
    legend.text = theme_text(family = base_family, size = base_size * 0.6),
    legend.title = theme_text(family = base_family, face = "bold",
			size = base_size * 0.6, hjust = 0),
    legend.position = c(0.95, 0.95),
    plot.title = theme_text(family = base_family, face = "bold",
			size = base_size * 0.9, vjust = 1.5),
    plot.margin = unit(c(1, 1, 1, 1.25), "lines")
  )
  modifyList(theme_gray(base_size = base_size, base_family = base_family), mod)

}

kmg2_theme_gray_natrisk <- function(base_size = 25, base_family = "") {

  mod <- list(
    axis.text.x = theme_blank(),
    axis.text.y = theme_text(family = base_family, size = base_size * 0.8,
      lineheight = 0.9, colour = "transparent", hjust = 1),
    axis.title.x = theme_blank(),
    axis.title.y = theme_text(family = base_family, size = base_size, angle = 90,
      vjust = 0.5, colour = "transparent"),
    axis.ticks = theme_segment(colour = "transparent"),
    axis.ticks.margin = unit(0.2, "cm"),
    legend.position = "none",
    panel.background = theme_rect(fill = "transparent", colour = "transparent"),
    panel.grid.major = theme_line(colour = "transparent"),
    panel.grid.minor = theme_line(colour = "transparent", size = 0.25),
    strip.background = theme_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = theme_blank(),
    strip.text.y = theme_text(family = base_family, size = base_size * 0.8,
      angle = -90, colour = "transparent"),
    plot.background = theme_rect(colour = "transparent", fill = "transparent"),
    plot.title = theme_blank(),
    plot.margin = unit(c(0, 1, 0, 1.25), "lines")
  )
  modifyList(theme_gray(base_size = base_size, base_family = base_family), mod)

}

kmg2_theme_gray_natrisk21 <- function(base_size = 25, base_family = "") {

  mod <- list(
    axis.line = theme_blank(),
    axis.text.x = theme_blank(),
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(),
    axis.title.x = theme_blank(),
    axis.title.y = theme_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(c(0, 0.2, 0, 0.2), "cm"),
    legend.position = "none",
    panel.background = theme_rect(fill = "transparent", colour = "transparent"),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.margin = unit(0, "lines"),
    strip.background = theme_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = theme_blank(),
    strip.text.y = theme_blank(),
    plot.background = theme_rect(colour = "transparent", fill = "transparent"),
    plot.title = theme_blank(),
    plot.margin = unit(c(0, 0, 0, -2), "lines")
  )
  modifyList(theme_gray(base_size = base_size, base_family = base_family), mod)

}

kmg2_theme_bw <- function(base_size = 25, base_family = "") {

  mod <- list(
    axis.title.x = theme_text(family = base_family, size = base_size, vjust = -0.5),
    axis.title.y = theme_text(family = base_family, size = base_size, angle = 90,
      vjust = 0.25),
    axis.ticks.margin = unit(0.2, "cm"),
		legend.background = theme_blank(),
    legend.key.size = unit(1.5, "lines"),
    legend.text = theme_text(family = base_family, size = base_size * 0.6),
    legend.title = theme_text(family = base_family, face = "bold",
			size = base_size * 0.6, hjust = 0),
    legend.position = c(0.95, 0.95),
    plot.title = theme_text(family = base_family, face = "bold",
			size = base_size * 0.9, vjust = 1.5),
    plot.margin = unit(c(1, 1, 1, 1.25), "lines")
  )
  modifyList(theme_bw(base_size = base_size, base_family = base_family), mod)

}

kmg2_theme_bw_natrisk <- function(base_size = 25, base_family = "") {

  mod <- list(
    axis.text.x = theme_blank(),
    axis.text.y = theme_text(family = base_family, size = base_size * 0.8,
      lineheight = 0.9, colour = "transparent", hjust = 1),
    axis.title.x = theme_blank(),
    axis.title.y = theme_text(family = base_family, size = base_size, angle = 90,
      vjust = 0.5, colour = "transparent"),
    axis.ticks = theme_segment(colour = "transparent"),
    axis.ticks.margin = unit(0.2, "cm"),
    legend.position = "none",
    panel.background = theme_rect(fill = "transparent", colour = "transparent"),
    panel.border = theme_blank(),
    panel.grid.major = theme_line(colour = "transparent"),
    panel.grid.minor = theme_line(colour = "transparent", size = 0.25),
    strip.background = theme_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = theme_blank(),
    strip.text.y = theme_text(family = base_family, size = base_size * 0.8,
      angle = -90, colour = "transparent"),
    plot.background = theme_rect(colour = "transparent", fill = "transparent"),
    plot.title = theme_blank(),
    plot.margin = unit(c(0, 1, 0, 1.25), "lines")
  )
  modifyList(theme_bw(base_size = base_size, base_family = base_family), mod)

}

kmg2_theme_bw_natrisk21 <- function(base_size = 25, base_family = "") {

  mod <- list(
    axis.line = theme_blank(),
    axis.text.x = theme_blank(),
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(),
    axis.title.x = theme_blank(),
    axis.title.y = theme_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(c(0, 0.2, 0, 0.2), "cm"),
    legend.position = "none",
    panel.background = theme_rect(fill = "transparent", colour = "transparent"),
    panel.border = theme_blank(),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.margin = unit(0, "lines"),
    strip.background = theme_rect(fill = "transparent", colour = "transparent"),
    strip.text.x = theme_blank(),
    strip.text.y = theme_blank(),
    plot.background = theme_rect(colour = "transparent", fill = "transparent"),
    plot.title = theme_blank(),
    plot.margin = unit(c(0, 0, 0, -2), "lines")
  )
  modifyList(theme_bw(base_size = base_size, base_family = base_family), mod)

}

