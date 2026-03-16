# =========================================================
# Chocolate Metabolomics Report Figures
# Sample selected: sample_id ending in 403
# Output:
#   plots/sugars_chart.png
#   plots/macronutrient_bar_chart.png
#   plots/micronutrient_chart.png
# =========================================================

# install.packages(c("ggplot2", "dplyr", "scales"))
library(ggplot2)
library(dplyr)
library(scales)

# -----------------------------
# 1) Load data
# -----------------------------
df <- read.delim(
  "chocolate_metabolomics.tsv",
  sep = "\t",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Convert all numeric columns except sample_id
for (nm in names(df)) {
  if (nm != "sample_id") {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
}

# -----------------------------
# 2) Select chocolate sample
# -----------------------------
chocolate <- df %>%
  filter(grepl("403$", sample_id))

if (nrow(chocolate) == 0) {
  stop("No sample_id ending in 403 was found.")
}
if (nrow(chocolate) > 1) {
  stop("More than one sample_id ends in 403. Please check the file.")
}

sample_id <- chocolate$sample_id[1]

dir.create("plots", showWarnings = FALSE)

# -----------------------------
# 3) Styling
# -----------------------------
bg_card  <- "#151c26"
panel_bg <- "#101720"
text_col <- "#ecf1f7"
muted    <- "#9eacbd"
accent   <- "#d9b27c"
grid_col <- "#2a3340"

pie_palette <- c(
  "#c08a52", "#9f6a47", "#7f5539", "#d5b38a",
  "#8a6a4a", "#6e4f38", "#b78a5b", "#e0c7a3"
)

macro_palette <- c(
  "Lipid" = "#8f5d3b",
  "Carbohydrate" = "#c08a52",
  "Protein" = "#d9b27c",
  "Water" = "#6f8196",
  "Ash" = "#8f99a6"
)

theme_dashboard <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = bg_card, color = NA),
      panel.background  = element_rect(fill = panel_bg, color = NA),
      legend.background = element_rect(fill = bg_card, color = NA),
      legend.key        = element_rect(fill = bg_card, color = NA),
      panel.grid.major.y = element_line(color = grid_col, linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = text_col, size = 11),
      axis.title         = element_text(color = muted, face = "bold", size = 11),
      plot.title         = element_text(color = text_col, face = "bold", hjust = 0.5, size = 15),
      plot.subtitle      = element_text(color = muted, hjust = 0.5, size = 10),
      legend.title       = element_text(color = muted, face = "bold"),
      legend.text        = element_text(color = text_col),
      plot.margin        = margin(14, 14, 14, 14)
    )
}

theme_pie <- function() {
  theme_void(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = bg_card, color = NA),
      legend.background = element_rect(fill = bg_card, color = NA),
      legend.key        = element_rect(fill = bg_card, color = NA),
      plot.title        = element_text(color = text_col, face = "bold", hjust = 0.5, size = 15),
      plot.subtitle     = element_text(color = muted, hjust = 0.5, size = 10),
      legend.title      = element_text(color = muted, face = "bold"),
      legend.text       = element_text(color = text_col),
      plot.margin       = margin(14, 14, 14, 14)
    )
}

# Helper to safely build a numeric vector from selected columns
extract_numeric_profile <- function(row_df, cols, label_name, value_name) {
  cols <- cols[cols %in% names(row_df)]
  out <- data.frame(
    label = cols,
    value = as.numeric(row_df[1, cols]),
    stringsAsFactors = FALSE
  )
  names(out) <- c(label_name, value_name)
  out %>%
    filter(!is.na(.data[[value_name]]), .data[[value_name]] > 0)
}

# -----------------------------
# 4) Sugar profile
# -----------------------------
sugar_cols <- c(
  "Allose", "Apiose", "Arabinose", "Fructose", "Fucose",
  "GalA", "Galactose", "GalN", "GalNAc", "GlcA", "GlcN", "GlcNAc",
  "Glucose", "Glycogen", "IdoA", "Mannose", "Rhamnose", "Ribose",
  "Starch", "Sugar and cellulose", "Xylose"
)

sugar_df <- extract_numeric_profile(
  chocolate,
  sugar_cols,
  label_name = "compound",
  value_name = "value"
) %>%
  arrange(desc(value))

# Top 8 for readability
sugar_plot_df <- sugar_df %>%
  slice_head(n = 8) %>%
  mutate(
    compound = factor(compound, levels = compound),
    label_text = number(value, accuracy = 0.01)
  )

p1 <- ggplot(sugar_plot_df, aes(x = "", y = value, fill = compound)) +
  geom_col(width = 1, color = bg_card, linewidth = 0.6) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label_text),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.7,
    fontface = "bold"
  ) +
  scale_fill_manual(values = pie_palette[seq_len(nrow(sugar_plot_df))]) +
  labs(
    title = "Sugar Component Profile",
    subtitle = paste("Top sugar-related components •", sample_id),
    fill = "Component"
  ) +
  theme_pie()

ggsave(
  filename = "plots/sugars_chart.png",
  plot = p1,
  width = 8.4,
  height = 5.3,
  dpi = 180,
  bg = bg_card
)

# -----------------------------
# 5) Proximate composition
# -----------------------------
macro_cols <- c("Lipid", "Carbohydrate", "Protein", "Water", "Ash")

macro_df <- extract_numeric_profile(
  chocolate,
  macro_cols,
  label_name = "nutrient",
  value_name = "value"
) %>%
  mutate(
    nutrient = factor(nutrient, levels = macro_cols),
    label_text = number(value, accuracy = 0.01)
  )

p2 <- ggplot(macro_df, aes(x = nutrient, y = value, fill = nutrient)) +
  geom_col(width = 0.68, show.legend = FALSE) +
  geom_text(
    aes(label = label_text),
    vjust = -0.5,
    color = text_col,
    size = 4.2,
    fontface = "bold"
  ) +
  scale_fill_manual(values = macro_palette) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    limits = c(0, max(macro_df$value, na.rm = TRUE) * 1.16),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Proximate Composition",
    subtitle = paste("Lipid, carbohydrate, protein, water, and ash •", sample_id),
    x = NULL,
    y = "Measured value"
  ) +
  theme_dashboard()

ggsave(
  filename = "plots/macronutrient_bar_chart.png",
  plot = p2,
  width = 9.0,
  height = 5.4,
  dpi = 180,
  bg = bg_card
)

# -----------------------------
# 6) Mineral profile
# -----------------------------
mineral_cols <- c(
  "Calcium", "Iron", "Magnesium", "Phosphorus", "Potassium",
  "Sodium", "Zinc", "Copper", "Manganese", "Selenium", "Sulfur",
  "Boron", "Chromium", "Cobalt", "Molybdenum", "Nickel",
  "Other Ash", "Strontium", "Vanadium", "Lithium", "Barium",
  "Aluminum", "Arsenic", "Cadmium", "Lead", "Tungsten", "Beryllium"
)

mineral_df <- extract_numeric_profile(
  chocolate,
  mineral_cols,
  label_name = "mineral",
  value_name = "value"
) %>%
  arrange(desc(value))

# Top 6 keeps the pie legible
mineral_plot_df <- mineral_df %>%
  slice_head(n = 6) %>%
  mutate(
    mineral = factor(mineral, levels = mineral),
    label_text = number(value, accuracy = 0.001)
  )

p3 <- ggplot(mineral_plot_df, aes(x = "", y = value, fill = mineral)) +
  geom_col(width = 1, color = bg_card, linewidth = 0.6) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label_text),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.7,
    fontface = "bold"
  ) +
  scale_fill_manual(values = pie_palette[seq_len(nrow(mineral_plot_df))]) +
  labs(
    title = "Mineral Micronutrient Profile",
    subtitle = paste("Top mineral contributors •", sample_id),
    fill = "Mineral"
  ) +
  theme_pie()

ggsave(
  filename = "plots/micronutrient_chart.png",
  plot = p3,
  width = 8.4,
  height = 5.3,
  dpi = 180,
  bg = bg_card
)

# -----------------------------
# 7) Console summary
# -----------------------------
cat("\nFigures created successfully for sample:", sample_id, "\n")
cat("Saved files:\n")
cat(" - plots/sugars_chart.png\n")
cat(" - plots/macronutrient_bar_chart.png\n")
cat(" - plots/micronutrient_chart.png\n\n")

cat("Top proximate values:\n")
print(macro_df[, c("nutrient", "value")], row.names = FALSE)