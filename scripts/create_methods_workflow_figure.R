library(grid)

figure_dir <- file.path("figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

draw_box <- function(label, x, y, w, h, fill, border = "#7E7A72",
                     fontsize = 7.6, fontface = "bold", lineheight = 1.05) {
  grid.roundrect(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(w, "npc"),
    height = unit(h, "npc"),
    r = unit(0.010, "npc"),
    gp = gpar(fill = fill, col = border, lwd = 0.7)
  )
  grid.text(
    label,
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    gp = gpar(fontsize = fontsize, fontface = fontface, col = "#222222", lineheight = lineheight),
    just = "centre"
  )
}

draw_arrow <- function(x0, y0, x1, y1, col = "#6F6B63", lwd = 0.85) {
  grid.lines(
    x = unit(c(x0, x1), "npc"),
    y = unit(c(y0, y1), "npc"),
    arrow = arrow(length = unit(0.10, "cm"), type = "closed"),
    gp = gpar(col = col, lwd = lwd)
  )
}

draw_path_arrow <- function(xs, ys, col = "#6F6B63", lwd = 0.85) {
  grid.lines(
    x = unit(xs, "npc"),
    y = unit(ys, "npc"),
    arrow = arrow(length = unit(0.10, "cm"), type = "closed"),
    gp = gpar(col = col, lwd = lwd)
  )
}

draw_lane_label <- function(label, x, y) {
  grid.text(
    label,
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    gp = gpar(fontsize = 6.8, fontface = "bold", col = "#5F5A52", letterspace = 0),
    just = "centre"
  )
}

draw_methods_workflow <- function(path, device = c("png", "pdf")) {
  device <- match.arg(device)
  if (device == "png") {
    png(path, width = 7.2, height = 4.2, units = "in", res = 450)
  } else {
    pdf(path, width = 7.2, height = 4.2)
  }

  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))

  grid.text(
    "Analytical workflow: separate accessibility, need and equity context",
    x = unit(0.04, "npc"),
    y = unit(0.955, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 12.2, fontface = "bold", col = "#1F1F1F")
  )
  grid.text(
    "R5r accessibility and Census need are combined only at the mismatch stage; IMD is retained for interpretation.",
    x = unit(0.04, "npc"),
    y = unit(0.905, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 7.8, col = "#555555")
  )

  draw_lane_label("INPUTS", 0.50, 0.800)
  draw_box("LSOA origins + OSM amenities\n+ GTFS/OSM network", 0.19, 0.720, 0.28, 0.090, "#EEF2F5", fontsize = 6.9)
  draw_box("Census 2021\nage + household", 0.50, 0.720, 0.23, 0.090, "#F4F0E8", fontsize = 7.0)
  draw_box("IMD2019\ndeprivation", 0.79, 0.720, 0.22, 0.090, "#F4F0E8", fontsize = 7.0)

  draw_lane_label("MEASUREMENT LAYERS", 0.50, 0.610)
  draw_box("R5r travel-time matrix\nWALK 15 / WALK+PT 30\nnearest time + counts", 0.19, 0.505, 0.29, 0.120, "#E8F2FF", fontsize = 6.8)
  draw_box("Group-specific need indicators\n+ caregiving proxy", 0.50, 0.505, 0.27, 0.100, "#F4F0E8", fontsize = 6.9)
  draw_box("Deprivation context\nfor equity interpretation", 0.79, 0.505, 0.24, 0.100, "#F4F0E8", fontsize = 6.9)

  draw_arrow(0.19, 0.670, 0.19, 0.568)
  draw_arrow(0.50, 0.670, 0.50, 0.560)
  draw_arrow(0.79, 0.670, 0.79, 0.560)

  draw_lane_label("DERIVED INDICATORS", 0.50, 0.385)
  draw_box("Group-weighted service-basket\naccessibility", 0.25, 0.305, 0.28, 0.085, "#E9EFEA", fontsize = 6.9)
  draw_box("Need-access\nmismatch", 0.50, 0.305, 0.20, 0.085, "#F7E6E2", fontsize = 7.0)
  draw_box("Transit gain\nWALK vs WALK+PT", 0.72, 0.305, 0.20, 0.085, "#EAF3F8", fontsize = 6.9)

  draw_arrow(0.19, 0.443, 0.25, 0.350)
  draw_arrow(0.39, 0.305, 0.40, 0.305)
  draw_arrow(0.50, 0.455, 0.50, 0.350)

  draw_lane_label("EQUITY ANALYSIS", 0.50, 0.190)
  draw_box(
    "Spatial equity analysis\nmismatch + transit gain + IMD context\nIMD distributions + typology + LISA + cases",
    0.50, 0.100, 0.56, 0.100, "#F7E6E2",
    fontsize = 6.8
  )
  draw_arrow(0.50, 0.260, 0.50, 0.155)
  draw_arrow(0.72, 0.260, 0.61, 0.155)

  dev.off()
}

draw_methods_workflow(file.path(figure_dir, "figure_methods_analytical_workflow.png"), "png")
draw_methods_workflow(file.path(figure_dir, "figure_methods_analytical_workflow.pdf"), "pdf")

message("Saved methods workflow figure.")
