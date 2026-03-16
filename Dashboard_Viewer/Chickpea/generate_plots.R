# =========================================================
# Chickpea Metabolomics Report Figures
# Sample selected: GGB100039
#
# Input:
#   chickpea_metabolomics.tsv
#
# Output:
#   plots/carbohydrate_component_chart.png
#   plots/mineral_profile_chart.png
#   plots/proximate_composition_chart.png
# =========================================================

# install.packages(c("ggplot2", "dplyr", "scales"))
library(ggplot2)
library(dplyr)
library(scales)

# -----------------------------
# 1) Load data
# -----------------------------
df <- read.delim(
  "chickpea_metabolomics.tsv",
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
# 2) Select chickpea sample
# -----------------------------
sample_data <- df %>%
  filter(sample_id == "GGB100039")

if (nrow(sample_data) == 0) {
  stop("Sample GGB100039 was not found.")
}
if (nrow(sample_data) > 1) {
  stop("More than one row matched GGB100039.")
}

sample_id <- sample_data$sample_id[1]

dir.create("plots", showWarnings = FALSE)

# -----------------------------
# 3) Styling
# -----------------------------
bg_card  <- "#231914"
panel_bg <- "#16110d"
text_col <- "#f8f1ea"
muted    <- "#d8c4b3"
accent   <- "#e6b06a"
grid_col <- "#3a2b21"

warm_palette <- c(
  "#e6b06a", "#d77a61", "#c98d54", "#f0c36c",
  "#8c5a3c", "#bf7d4f", "#d7a066", "#f4d39a"
)

macro_palette <- c(
  "Lipid" = "#d77a61",
  "Carbohydrate" = "#e6b06a",
  "Protein" = "#9fc08a",
  "Water" = "#7fa7c7",
  "Ash" = "#b6a99a"
)

theme_dashboard <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background    = element_rect(fill = bg_card, color = NA),
      panel.background   = element_rect(fill = panel_bg, color = NA),
      legend.background  = element_rect(fill = bg_card, color = NA),
      legend.key         = element_rect(fill = bg_card, color = NA),
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
# 4) Carbohydrate-related profile
# -----------------------------
carb_cols <- c(
  "Fibers", "Fructose", "Fucose", "GalA", "Galactose", "GalN", "GalNAc",
  "GlcA", "GlcN", "GlcNAc", "Glucose", "Glycogen", "IdoA", "Mannose",
  "Rhamnose", "Ribose", "Starch", "Sugar and cellulose", "Xylose",
  "Allose", "Apiose", "Arabinose"
)

carb_df <- extract_numeric_profile(
  sample_data,
  carb_cols,
  label_name = "component",
  value_name = "value"
) %>%
  arrange(desc(value))

carb_plot_df <- carb_df %>%
  slice_head(n = 8) %>%
  mutate(
    component = factor(component, levels = component),
    label_text = number(value, accuracy = 0.01)
  )

p1 <- ggplot(carb_plot_df, aes(x = "", y = value, fill = component)) +
  geom_col(width = 1, color = bg_card, linewidth = 0.6) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label_text),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.7,
    fontface = "bold"
  ) +
  scale_fill_manual(values = warm_palette[seq_len(nrow(carb_plot_df))]) +
  labs(
    title = "Carbohydrate-Related Component Profile",
    subtitle = paste("Top measured components •", sample_id),
    fill = "Component"
  ) +
  theme_pie()

ggsave(
  filename = "plots/carbohydrate_component_chart.png",
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
  sample_data,
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
  filename = "plots/proximate_composition_chart.png",
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
  "Strontium", "Vanadium", "Lithium", "Barium", "Aluminum",
  "Arsenic", "Cadmium", "Lead", "Tungsten", "Beryllium"
)

mineral_df <- extract_numeric_profile(
  sample_data,
  mineral_cols,
  label_name = "mineral",
  value_name = "value"
) %>%
  arrange(desc(value))

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
  scale_fill_manual(values = warm_palette[seq_len(nrow(mineral_plot_df))]) +
  labs(
    title = "Mineral Micronutrient Profile",
    subtitle = paste("Top mineral contributors •", sample_id),
    fill = "Mineral"
  ) +
  theme_pie()

ggsave(
  filename = "plots/mineral_profile_chart.png",
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
cat(" - plots/carbohydrate_component_chart.png\n")
cat(" - plots/mineral_profile_chart.png\n")
cat(" - plots/proximate_composition_chart.png\n\n")

cat("Top proximate values:\n")
print(macro_df[, c("nutrient", "value")], row.names = FALSE)