library(grid)

figure_dir <- file.path("figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

draw_box <- function(label, x, y, w, h, fill, col = "#4A4A4A",
                     fontsize = 9.2, fontface = "bold") {
  grid.roundrect(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(w, "npc"),
    height = unit(h, "npc"),
    r = unit(0.008, "npc"),
    gp = gpar(fill = fill, col = col, lwd = 0.8)
  )
  grid.text(
    label,
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    gp = gpar(fontsize = fontsize, fontface = fontface, col = "#222222"),
    just = "centre"
  )
}

draw_arrow <- function(x0, y0, x1, y1, col = "#666666", lwd = 0.9) {
  grid.lines(
    x = unit(c(x0, x1), "npc"),
    y = unit(c(y0, y1), "npc"),
    arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
    gp = gpar(col = col, lwd = lwd)
  )
}

draw_theoretical_framework <- function(path, device = c("png", "pdf")) {
  device <- match.arg(device)
  if (device == "png") {
    png(path, width = 8.3, height = 7.15, units = "in", res = 450)
  } else {
    pdf(path, width = 8.3, height = 7.15)
  }

  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))

  grid.text(
    "Figure 1. Theoretical Framework",
    x = unit(0.04, "npc"),
    y = unit(0.965, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 16, fontface = "bold", col = "#1F1F1F")
  )
  grid.text(
    "Theory is translated into role-based accessibility subjects, service-basket measurement and two equity tests.",
    x = unit(0.04, "npc"),
    y = unit(0.925, "npc"),
    just = c("left", "top"),
    gp = gpar(fontsize = 9.5, col = "#555555")
  )

  draw_box("Mobility of Care", 0.28, 0.835, 0.18, 0.072, "#EAF3F8")
  draw_box("Accessibility Equity\n/ Transport Justice", 0.50, 0.835, 0.21, 0.078, "#EAF3F8", fontsize = 8.2)
  draw_box("Capability Approach", 0.72, 0.835, 0.18, 0.072, "#EAF3F8")

  draw_arrow(0.28, 0.792, 0.43, 0.725)
  draw_arrow(0.50, 0.792, 0.50, 0.725)
  draw_arrow(0.72, 0.792, 0.57, 0.725)
  draw_box("Need is not the same as proximity", 0.50, 0.695, 0.34, 0.074, "#F8F1E8")

  draw_arrow(0.50, 0.654, 0.50, 0.595)
  draw_box("Children & Adolescents\neducation + play", 0.24, 0.550, 0.23, 0.083, "#F6F6F6", fontsize = 8.0)
  draw_box("Caregiving Adults\nschool-food-health-\ntransport chains", 0.50, 0.550, 0.28, 0.092, "#FFF0E0", fontsize = 7.2)
  draw_box("Older Adults\nhealth + pharmacy\n+ local transport", 0.76, 0.550, 0.24, 0.092, "#F6F6F6", fontsize = 7.4)
  draw_arrow(0.50, 0.654, 0.24, 0.595)
  draw_arrow(0.50, 0.654, 0.76, 0.595)

  draw_arrow(0.24, 0.500, 0.40, 0.430)
  draw_arrow(0.50, 0.500, 0.50, 0.430)
  draw_arrow(0.76, 0.500, 0.60, 0.430)
  draw_box("Weighted Service Baskets\n+\nR5r Planned Accessibility", 0.50, 0.390, 0.42, 0.095, "#E8F2FF", fontsize = 8.8)

  draw_arrow(0.50, 0.332, 0.50, 0.280)
  draw_box("Need-Access Mismatch", 0.50, 0.250, 0.30, 0.070, "#FBEAEA")

  draw_arrow(0.50, 0.212, 0.35, 0.170)
  draw_arrow(0.50, 0.212, 0.65, 0.170)
  draw_box("Accessibility Paradox\nMore access != more equity", 0.35, 0.145, 0.31, 0.074, "#EAF3F8", fontsize = 8.3)
  draw_box("Vulnerability Paradox\nDeprivation is an incomplete proxy", 0.65, 0.145, 0.33, 0.074, "#FFF0E0", fontsize = 8.1)

  draw_arrow(0.35, 0.104, 0.46, 0.072)
  draw_arrow(0.65, 0.104, 0.54, 0.072)
  draw_box("Care-sensitive 15-minute City", 0.50, 0.050, 0.35, 0.048, "#E6F4EF", "#0D5C46", fontsize = 9.1)

  dev.off()
}

draw_theoretical_framework(file.path(figure_dir, "figure1_theoretical_framework.png"), "png")
draw_theoretical_framework(file.path(figure_dir, "figure1_theoretical_framework.pdf"), "pdf")

message("Saved theoretical framework figure to: ", figure_dir)
