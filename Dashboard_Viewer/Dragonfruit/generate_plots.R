args <- commandArgs(trailingOnly = TRUE)

input_file <- if (length(args) >= 1) args[1] else "dragonfruit_metabolomics.tsv"
sample_id  <- if (length(args) >= 2) args[2] else "GGB101520"
out_dir    <- if (length(args) >= 3) args[3] else "plots"

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2')")
}

library(ggplot2)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df <- read.delim(input_file, check.names = FALSE, stringsAsFactors = FALSE)

if (!("sample_id" %in% names(df))) {
  stop("Input TSV must contain a 'sample_id' column.")
}

sample_row <- df[df$sample_id == sample_id, , drop = FALSE]
if (nrow(sample_row) == 0) {
  stop(sprintf("Sample '%s' not found in %s", sample_id, input_file))
}
sample_row <- sample_row[1, , drop = FALSE]

make_named_df <- function(row, cols, label_name = "Component", value_name = "Value") {
  cols <- cols[cols %in% names(row)]
  vals <- vapply(cols, function(x) as.numeric(row[[x]]), numeric(1))
  out <- data.frame(label = cols, value = vals, stringsAsFactors = FALSE)
  names(out) <- c(label_name, value_name)
  out
}

pretty_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 4, colour = "#2b1024"),
      plot.subtitle = element_text(size = base_size - 1, colour = "#6e4962"),
      axis.title = element_text(face = "bold", colour = "#4a2a42"),
      axis.text = element_text(colour = "#4a2a42"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#f1d9e7", linewidth = 0.4),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "#fffafb", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key = element_rect(fill = "white", colour = NA)
    )
}

label_top_n <- function(df, n = 10) {
  df <- df[is.finite(df$Value) & df$Value > 0, , drop = FALSE]
  df <- df[order(df$Value, decreasing = TRUE), , drop = FALSE]
  head(df, n)
}

# -----------------------------
# 1) Sugar / carbohydrate-related panel
# -----------------------------
carb_candidates <- c(
  "Fructose", "Glucose", "Galactose", "Mannose", "Ribose", "Xylose",
  "Arabinose", "Fucose", "Rhamnose", "Allose", "Apiose",
  "Fibers", "Starch", "Sugar and cellulose", "Glycogen",
  "2-Glucose", "2-Mannose", "2-Xylose", "3-Glucose", "4-Glucose",
  "6-Glucose", "T-Glucose", "T-Galactose", "T-Arabinose", "T-Xylose"
)

carb_df <- make_named_df(sample_row, carb_candidates, "Component", "Value")
carb_df <- label_top_n(carb_df, 10)

if (nrow(carb_df) == 0) {
  carb_df <- data.frame(Component = "No detected sugar-related compounds", Value = 0)
}

carb_df$Component <- factor(carb_df$Component, levels = rev(carb_df$Component))
carb_df$Label <- sprintf("%.3f", carb_df$Value)

p1 <- ggplot(carb_df, aes(x = Component, y = Value)) +
  geom_segment(aes(xend = Component, y = 0, yend = Value), linewidth = 2.2, colour = "#f4bfd6") +
  geom_point(size = 5.2, colour = "#c92d79") +
  geom_text(aes(label = Label), hjust = -0.15, size = 3.7, colour = "#7c2351", fontface = "bold") +
  coord_flip(clip = "off") +
  labs(
    title = sprintf("Sugar-Related Component Profile (%s)", sample_id),
    subtitle = "Top detected carbohydrate-associated compounds",
    x = NULL,
    y = "Abundance"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  pretty_theme(13)

ggsave(
  filename = file.path(out_dir, "carbohydrate_component_chart.png"),
  plot = p1, width = 11, height = 6.5, dpi = 320
)

# -----------------------------
# 2) Mineral panel
# -----------------------------
mineral_candidates <- c(
  "Calcium", "Copper", "Iron", "Magnesium", "Manganese", "Phosphorus",
  "Potassium", "Selenium", "Sodium", "Sulfur", "Zinc", "Boron",
  "Chromium", "Cobalt", "Lithium", "Molybdenum", "Nickel", "Strontium",
  "Vanadium", "Aluminum", "Arsenic", "Barium", "Beryllium", "Cadmium",
  "Lead", "Tungsten"
)

min_df <- make_named_df(sample_row, mineral_candidates, "Mineral", "Value")
min_df <- label_top_n(min_df, 10)

if (nrow(min_df) == 0) {
  min_df <- data.frame(Mineral = "No detected minerals", Value = 0)
}

min_df$Mineral <- factor(min_df$Mineral, levels = rev(min_df$Mineral))
min_df$Label <- sprintf("%.4f", min_df$Value)

p2 <- ggplot(min_df, aes(x = Mineral, y = Value)) +
  geom_col(width = 0.68, fill = "#8b46d6") +
  geom_text(aes(label = Label), hjust = -0.12, size = 3.6, colour = "#5f2f93", fontface = "bold") +
  coord_flip(clip = "off") +
  labs(
    title = sprintf("Mineral Micronutrient Profile (%s)", sample_id),
    subtitle = "Top detected mineral contributors",
    x = NULL,
    y = "Abundance"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  pretty_theme(13)

ggsave(
  filename = file.path(out_dir, "mineral_profile_chart.png"),
  plot = p2, width = 11, height = 6.5, dpi = 320
)

# -----------------------------
# 3) Proximate composition panel (donut chart)
# -----------------------------
proximate_cols <- c("Lipid", "Carbohydrate", "Protein", "Water", "Ash")
prox_df <- make_named_df(sample_row, proximate_cols, "Metric", "Value")
prox_df <- prox_df[is.finite(prox_df$Value) & prox_df$Value >= 0, , drop = FALSE]
prox_df <- prox_df[prox_df$Value > 0, , drop = FALSE]

if (nrow(prox_df) == 0) {
  stop("No proximate composition values found for the selected sample.")
}

prox_df <- prox_df[order(match(prox_df$Metric, proximate_cols)), , drop = FALSE]
prox_df$Fraction <- prox_df$Value / sum(prox_df$Value)
prox_df$ymax <- cumsum(prox_df$Fraction)
prox_df$ymin <- c(0, head(prox_df$ymax, -1))
prox_df$label_pos <- (prox_df$ymax + prox_df$ymin) / 2
prox_df$Label <- sprintf("%s\n%.2f%%", prox_df$Metric, prox_df$Value)

prox_colors <- c(
  "Lipid" = "#ff9bc8",
  "Carbohydrate" = "#ff5ca8",
  "Protein" = "#9b5de5",
  "Water" = "#4aa8ff",
  "Ash" = "#8b7d74"
)

p3 <- ggplot(prox_df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 1.8, fill = Metric)) +
  geom_rect(colour = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 4.7)) +
  scale_fill_manual(values = prox_colors) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, colour = "#2b1024", hjust = 0.5),
    plot.subtitle = element_text(size = 11, colour = "#6e4962", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "right",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "#fffafb", colour = NA)
  ) +
  labs(
    title = sprintf("Proximate Composition (%s)", sample_id),
    subtitle = "Donut chart of lipid, carbohydrate, protein, water, and ash"
  ) +
  annotate("text", x = 0, y = 0, label = "Raw\nDragon Fruit", size = 5, fontface = "bold", colour = "#5b314d")

ggsave(
  filename = file.path(out_dir, "proximate_composition_chart.png"),
  plot = p3, width = 8.8, height = 7.2, dpi = 320
)

cat(sprintf("Generated polished plots for %s in %s\n", sample_id, out_dir))