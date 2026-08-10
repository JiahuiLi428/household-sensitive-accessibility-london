library(grid)

args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
figure_dir <- file.path(root, "figures", "publication")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

draw_box <- function(x, y, w, h, title, body, fill = "#F7F7F5", border = "#8B8B84") {
  grid.roundrect(
    x = x, y = y, width = w, height = h,
    r = unit(0.02, "npc"),
    gp = gpar(fill = fill, col = border, lwd = 0.8)
  )
  grid.text(title, x = x, y = y + h * 0.27, gp = gpar(fontsize = 9.5, fontface = "bold", col = "#222222"))
  grid.text(body, x = x, y = y - h * 0.10, gp = gpar(fontsize = 7.2, col = "#444444", lineheight = 1.05))
}

draw_arrow <- function(x0, x1, y) {
  grid.lines(
    x = unit(c(x0, x1), "npc"),
    y = unit(c(y, y), "npc"),
    arrow = arrow(length = unit(0.10, "inches"), type = "closed"),
    gp = gpar(col = "#777777", lwd = 0.7)
  )
}

png(file.path(figure_dir, "figure3_rq_evidence_structure.png"), width = 2400, height = 1550, res = 300)
grid.newpage()
grid.rect(gp = gpar(fill = "white", col = NA))

grid.text(
  "Figure 3. Research-Question Evidence Structure",
  x = 0.06, y = 0.94, just = "left",
  gp = gpar(fontsize = 15, fontface = "bold", col = "#1f1f1f")
)
grid.text(
  "Each figure group is assigned to one research question so the results read as an argument rather than as a catalogue of maps.",
  x = 0.06, y = 0.90, just = "left",
  gp = gpar(fontsize = 9.2, col = "#555555")
)

xs <- c(0.16, 0.39, 0.62, 0.85)
w <- 0.19
h <- 0.56
y <- 0.55

draw_box(
  xs[1], y, w, h,
  "RQ1",
  "Who is accessibility\nbeing measured for?\n\nFigure 4\nsame city, different\naccessibility subjects",
  fill = "#EEF4F5"
)
draw_box(
  xs[2], y, w, h,
  "RQ2",
  "Does deprivation\nexplain the pattern?\n\nFigure 5\nIMD distributions\n+ effect sizes",
  fill = "#F4F1EA"
)
draw_box(
  xs[3], y, w, h,
  "RQ3",
  "Where does need\nexceed access?\n\nFigures 6-8, 11\nmismatch maps +\ncaregiving evidence chain",
  fill = "#F6F0EE"
)
draw_box(
  xs[4], y, w, h,
  "RQ4",
  "What does this imply\nfor the 15-minute city?\n\nFigures 9-10\naccessibility gains,\nuneven benefit +\nequity implications",
  fill = "#EFF3EC"
)

draw_arrow(0.265, 0.285, y)
draw_arrow(0.495, 0.515, y)
draw_arrow(0.725, 0.745, y)

grid.roundrect(
  x = 0.50, y = 0.14, width = 0.84, height = 0.12,
  r = unit(0.02, "npc"),
  gp = gpar(fill = "#F7FAF8", col = "#86A59A", lwd = 0.8)
)
grid.text(
  "Evidence chain: generic resident -> household-sensitive access -> need-access mismatch -> care-sensitive 15-minute city",
  x = 0.50, y = 0.14,
  gp = gpar(fontsize = 8.0, fontface = "bold", col = "#2F5D52", lineheight = 1.15)
)

dev.off()

message("Saved Figure 3 evidence structure.")
