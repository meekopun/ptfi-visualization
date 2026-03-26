# =========================================================
# Chickpea metabolomics figures — consistent theme version
# Uses the exact columns from your TSV
# =========================================================

# install.packages(c("ggplot2", "dplyr", "tidyr", "scales"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ---------------------------------------------------------
# 1) Load data
# ---------------------------------------------------------
df <- read.delim(
  "Dashboard_Viewer/Chickpea/chickpea_metabolomics.tsv",
  sep = "\t",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# convert everything except sample_id to numeric
for (nm in names(df)) {
  if (nm != "sample_id") {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
}

# ---------------------------------------------------------
# 2) Select sample
# ---------------------------------------------------------
target_sample <- "GGB100039"
sample_data <- df %>% filter(sample_id == target_sample)

if (nrow(sample_data) != 1) {
  stop("Expected exactly one matching sample row.")
}

sample_id <- sample_data$sample_id[1]
dir.create("plots", showWarnings = FALSE)

# ---------------------------------------------------------
# 3) Theme system
# ---------------------------------------------------------
chickpea_cols <- list(
  bg      = "#FFF8E8",
  panel   = "#FFFDF7",
  light   = "#EADFC8",
  fill    = "#D8B67A",
  line    = "#A37A35",
  dark    = "#5E4724"
)

theme_chickpea <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background      = element_rect(fill = chickpea_cols$bg, color = NA),
      panel.background     = element_rect(fill = chickpea_cols$panel, color = NA),
      panel.grid.minor     = element_blank(),
      panel.grid.major.y   = element_blank(),
      panel.grid.major.x   = element_line(color = chickpea_cols$light, linewidth = 0.5),
      axis.title           = element_text(face = "bold", color = chickpea_cols$dark),
      axis.text            = element_text(color = chickpea_cols$dark),
      plot.title           = element_text(face = "bold", size = 15, color = chickpea_cols$dark),
      plot.subtitle        = element_text(size = 10.5, color = chickpea_cols$dark),
      legend.position      = "none",
      plot.margin          = margin(12, 14, 12, 14)
    )
}

theme_set(theme_chickpea())

label_num <- function(x, acc = 0.001) {
  number(x, accuracy = acc, trim = TRUE)
}

# ---------------------------------------------------------
# 4) Helpers
# ---------------------------------------------------------
extract_profile <- function(row_df, cols, category_name, value_name) {
  keep <- cols[cols %in% names(row_df)]

  if (length(keep) == 0) {
    stop("None of the requested columns were found in the data.")
  }

  out <- data.frame(
    category = keep,
    value = as.numeric(row_df[1, keep]),
    stringsAsFactors = FALSE
  )

  names(out) <- c(category_name, value_name)

  out %>%
    filter(!is.na(.data[[value_name]]), .data[[value_name]] > 0)
}

plot_ranked_bar <- function(data, category_col, value_col, title, subtitle,
                            xlab = "Value", top_n = NULL) {

  plot_df <- data %>%
    arrange(desc(.data[[value_col]]))

  if (!is.null(top_n)) {
    plot_df <- plot_df %>% slice_head(n = top_n)
  }

  ggplot(
    plot_df,
    aes(
      x = .data[[value_col]],
      y = reorder(.data[[category_col]], .data[[value_col]])
    )
  ) +
    geom_col(
      fill = chickpea_cols$fill,
      color = chickpea_cols$line,
      linewidth = 0.7,
      width = 0.72
    ) +
    geom_text(
      aes(label = label_num(.data[[value_col]])),
      hjust = -0.12,
      size = 3.5,
      fontface = "bold",
      color = chickpea_cols$dark
    ) +
    scale_x_continuous(
      labels = label_number(),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = NULL
    )
}

# ---------------------------------------------------------
# 5) Exact column groups from your TSV
# ---------------------------------------------------------

carb_cols <- c(
  "Allose", "Apiose", "Arabinose", "Fibers", "Fructose", "Fucose",
  "GalA", "Galactose", "GalN", "GalNAc", "GlcA", "GlcN", "GlcNAc",
  "Glucose", "Glycogen", "IdoA", "Mannose", "Rhamnose", "Ribose",
  "Starch", "Sugar and cellulose", "Xylose"
)

macro_cols <- c("Water", "Carbohydrate", "Protein", "Lipid", "Ash")

mineral_cols <- c(
  "Aluminum", "Arsenic", "Barium", "Beryllium", "Boron", "Cadmium",
  "Calcium", "Chromium", "Cobalt", "Copper", "Iron", "Lead", "Lithium",
  "Magnesium", "Manganese", "Molybdenum", "Nickel", "Phosphorus",
  "Potassium", "Selenium", "Sodium", "Strontium", "Sulfur",
  "Tungsten", "Vanadium", "Zinc"
)

amino_cols <- c(
  "Alanine", "Arginine", "Asparagine", "Aspartic acid", "Cysteine",
  "Glutamic acid", "Glutamine", "Glycine", "Histidine", "Isoleucine",
  "Leucine", "Lysine", "Methionine", "Phenylalanine", "Proline",
  "Serine", "Threonine", "Tryptophan", "Tyrosine", "Valine"
)

fat_class_cols <- c(
  "Saturated", "Monounsaturated", "Omega-3", "Omega-6", "Trans", "Branched"
)

# ---------------------------------------------------------
# 6) Build data frames
# ---------------------------------------------------------
carb_df <- extract_profile(sample_data, carb_cols, "component", "value")
macro_df <- extract_profile(sample_data, macro_cols, "nutrient", "value")
mineral_df <- extract_profile(sample_data, mineral_cols, "mineral", "value")
amino_df <- extract_profile(sample_data, amino_cols, "amino_acid", "value")
fat_class_df <- extract_profile(sample_data, fat_class_cols, "lipid_class", "value")

# keep the plots focused
carb_top   <- carb_df %>% arrange(desc(value)) %>% slice_head(n = 10)
miner_top  <- mineral_df %>% arrange(desc(value)) %>% slice_head(n = 10)
amino_top  <- amino_df %>% arrange(desc(value)) %>% slice_head(n = 10)

# ---------------------------------------------------------
# 7) Plot: proximate composition
# ---------------------------------------------------------
macro_df <- macro_df %>%
  mutate(
    nutrient = factor(
      nutrient,
      levels = c("Water", "Carbohydrate", "Protein", "Lipid", "Ash")
    )
  )

p1 <- ggplot(macro_df, aes(x = nutrient, y = value)) +
  geom_col(
    fill = chickpea_cols$fill,
    color = chickpea_cols$line,
    linewidth = 0.7,
    width = 0.72
  ) +
  geom_text(
    aes(label = label_num(value, 0.01)),
    vjust = -0.35,
    size = 3.7,
    fontface = "bold",
    color = chickpea_cols$dark
  ) +
  scale_y_continuous(
    labels = label_number(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Proximate Composition",
    subtitle = paste("Sample:", sample_id),
    x = NULL,
    y = "Value"
  )

ggsave(
  "plots/proximate_composition.png",
  p1, width = 8, height = 5, dpi = 300, bg = chickpea_cols$bg
)

# ---------------------------------------------------------
# 8) Plot: carbohydrate profile
# ---------------------------------------------------------
p2 <- plot_ranked_bar(
  carb_top,
  category_col = "component",
  value_col = "value",
  title = "Top Carbohydrate Components",
  subtitle = paste("Sample:", sample_id),
  xlab = "Value"
)

ggsave(
  "plots/carbohydrate_profile.png",
  p2, width = 8.5, height = 5.8, dpi = 300, bg = chickpea_cols$bg
)

# ---------------------------------------------------------
# 9) Plot: mineral profile
# ---------------------------------------------------------
p3 <- plot_ranked_bar(
  miner_top,
  category_col = "mineral",
  value_col = "value",
  title = "Top Mineral Components",
  subtitle = paste("Sample:", sample_id),
  xlab = "Value"
)

ggsave(
  "plots/mineral_profile.png",
  p3, width = 8.5, height = 5.8, dpi = 300, bg = chickpea_cols$bg
)

# ---------------------------------------------------------
# 10) Plot: amino acid profile
# ---------------------------------------------------------
p4 <- plot_ranked_bar(
  amino_top,
  category_col = "amino_acid",
  value_col = "value",
  title = "Top Amino Acids",
  subtitle = paste("Sample:", sample_id),
  xlab = "Value"
)

ggsave(
  "plots/amino_acid_profile.png",
  p4, width = 8.5, height = 5.8, dpi = 300, bg = chickpea_cols$bg
)

# ---------------------------------------------------------
# 11) Plot: lipid class summary
# ---------------------------------------------------------
p5 <- plot_ranked_bar(
  fat_class_df,
  category_col = "lipid_class",
  value_col = "value",
  title = "Lipid Class Summary",
  subtitle = paste("Sample:", sample_id),
  xlab = "Value"
)

ggsave(
  "plots/lipid_class_summary.png",
  p5, width = 8, height = 4.8, dpi = 300, bg = chickpea_cols$bg
)

# ---------------------------------------------------------
# Done
# ---------------------------------------------------------
cat("Finished: 5 consistent chickpea-themed plots saved in /plots\n")

library(dplyr)
library(tidyr)
library(ggplot2)
library(fmsb)

amino_cols <- c(
  "Alanine", "Arginine", "Asparagine", "Aspartic acid", "Cysteine",
  "Glutamic acid", "Glutamine", "Glycine", "Histidine", "Isoleucine",
  "Leucine", "Lysine", "Methionine", "Phenylalanine", "Proline",
  "Serine", "Threonine", "Tryptophan", "Tyrosine", "Valine"
)

amino_data <- sample_data[, amino_cols, drop = FALSE]

max_vals <- rep(0.7, ncol(amino_data))
min_vals <- rep(0, ncol(amino_data))
radar_df <- rbind(max_vals, min_vals, amino_data)

radarchart(
  radar_df,
  axistype = 1,
  pcol = "darkgreen",
  pfcol = rgb(0, 0.6, 0, 0.25),
  plwd = 2,
  cglcol = "grey80",
  cglty = 1,
  axislabcol = "grey30",
  caxislabels = seq(0, 0.7, 0.1),
  vlcex = 0.7,
  title = "Amino Acid Profile"
)

amino_long <- amino_data %>%
  pivot_longer(
    cols = everything(),
    names_to = "AminoAcid",
    values_to = "Value"
  )

ggplot(amino_long, aes(x = AminoAcid, y = Value, group = 1)) +
  geom_polygon(fill = "darkgreen", alpha = 0.25) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(size = 2) +
  coord_polar() +
  theme_minimal() +
  labs(
    title = "Amino Acid Profile",
    subtitle = "Relative concentrations",
    x = NULL, y = NULL
  ) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )





# install.packages(c("ggplot2", "dplyr", "tidyr", "forcats", "scales"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)

# ---------------------------------------------------------
# 1) Read TSV + prepare sample
# ---------------------------------------------------------
df <- read.delim(
  "Dashboard_Viewer/Chickpea/chickpea_metabolomics.tsv",
  sep = "\t",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

for (nm in names(df)) {
  if (nm != "sample_id") {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
}

sample_data <- df %>% filter(sample_id == "GGB100039")
if (nrow(sample_data) != 1) stop("Sample not found or duplicated.")

dir.create("plots", showWarnings = FALSE)

# ---------------------------------------------------------
# 2) Theme system
# ---------------------------------------------------------
chickpea_cols <- list(
  bg      = "#FFF8E8",
  panel   = "#FFFDF7",
  light   = "#EADFC8",
  fill    = "#D8B67A",
  line    = "#A37A35",
  dark    = "#5E4724"
)

theme_chickpea <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background      = element_rect(fill = chickpea_cols$bg, color = NA),
      panel.background     = element_rect(fill = chickpea_cols$panel, color = NA),
      panel.grid.minor     = element_blank(),
      panel.grid.major.y   = element_blank(),
      panel.grid.major.x   = element_line(color = chickpea_cols$light, linewidth = 0.5),
      axis.title           = element_text(face = "bold", color = chickpea_cols$dark),
      axis.text            = element_text(color = chickpea_cols$dark),
      plot.title           = element_text(face = "bold", size = 15, color = chickpea_cols$dark),
      plot.subtitle        = element_text(size = 10.5, color = chickpea_cols$dark),
      legend.position      = "none",
      plot.margin          = margin(12, 14, 12, 14)
    )
}

theme_set(theme_chickpea())

label_num <- function(x, acc = 0.001) {
  number(x, accuracy = acc, trim = TRUE)
}

# ---------------------------------------------------------
# 3) Category groups
# ---------------------------------------------------------
carb_cols <- c(
  "Allose", "Apiose", "Arabinose", "Fibers", "Fructose", "Fucose",
  "GalA", "Galactose", "GalN", "GalNAc", "GlcA", "GlcN", "GlcNAc",
  "Glucose", "Glycogen", "IdoA", "Mannose", "Rhamnose", "Ribose",
  "Starch", "Sugar and cellulose", "Xylose"
)

macro_cols <- c("Water", "Carbohydrate", "Protein", "Lipid", "Ash")

mineral_cols <- c(
  "Aluminum", "Arsenic", "Barium", "Beryllium", "Boron", "Cadmium",
  "Calcium", "Chromium", "Cobalt", "Copper", "Iron", "Lead", "Lithium",
  "Magnesium", "Manganese", "Molybdenum", "Nickel", "Phosphorus",
  "Potassium", "Selenium", "Sodium", "Strontium", "Sulfur",
  "Tungsten", "Vanadium", "Zinc"
)

amino_cols <- c(
  "Alanine", "Arginine", "Asparagine", "Aspartic acid", "Cysteine",
  "Glutamic acid", "Glutamine", "Glycine", "Histidine", "Isoleucine",
  "Leucine", "Lysine", "Methionine", "Phenylalanine", "Proline",
  "Serine", "Threonine", "Tryptophan", "Tyrosine", "Valine"
)

fat_class_cols <- c(
  "Saturated", "Monounsaturated", "Omega-3", "Omega-6", "Trans", "Branched"
)

# ---------------------------------------------------------
# 4) Helper
# ---------------------------------------------------------
extract_profile <- function(row_df, cols, group_name) {
  keep <- cols[cols %in% names(row_df)]

  tibble(
    category = keep,
    value = as.numeric(row_df[1, keep]),
    group = group_name
  ) %>%
    filter(!is.na(value), value > 0) %>%
    mutate(
      share = value / sum(value),
      scaled_01 = value / max(value)
    )
}

carb_df  <- extract_profile(sample_data, carb_cols, "Carbohydrates")
macro_df <- extract_profile(sample_data, macro_cols, "Macros")
miner_df <- extract_profile(sample_data, mineral_cols, "Minerals")
amino_df <- extract_profile(sample_data, amino_cols, "Amino acids")
fat_df   <- extract_profile(sample_data, fat_class_cols, "Fat classes")

# =========================================================
# A) MACROS — CHICKPEA DONUT
# =========================================================
macro_plot <- macro_df %>%
  arrange(desc(value)) %>%
  mutate(label = paste0(category, "\n", label_num(share * 100, 0.1), "%"))

p_macro <- ggplot(macro_plot, aes(x = 2, y = value, fill = category)) +
  geom_col(color = chickpea_cols$panel, width = 1, linewidth = 0.8) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3.4,
    fontface = "bold",
    color = chickpea_cols$dark
  ) +
  scale_fill_manual(values = c(
    chickpea_cols$fill,
    chickpea_cols$line,
    chickpea_cols$light,
    "#C89C56",
    "#B5873A"
  )) +
  labs(
    title = "Macro Composition Donut",
    subtitle = "Whole-sample composition",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = chickpea_cols$bg, color = NA),
    plot.title = element_text(face = "bold", size = 15, color = chickpea_cols$dark),
    plot.subtitle = element_text(size = 10.5, color = chickpea_cols$dark)
  )

ggsave("plots/macro_donut.png", p_macro, width = 7, height = 7, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# B) CARBS — CIRCULAR LOLLIPOP
# =========================================================
carb_top <- carb_df %>%
  arrange(desc(value)) %>%
  slice_head(n = 12) %>%
  mutate(category = fct_reorder(category, value))

p_carb <- ggplot(carb_top, aes(x = category, y = value)) +
  geom_segment(
    aes(xend = category, y = 0, yend = value),
    linewidth = 1.1,
    color = chickpea_cols$line
  ) +
  geom_point(
    aes(size = value, fill = value),
    shape = 21,
    color = chickpea_cols$panel,
    stroke = 1
  ) +
  coord_polar() +
  scale_size(range = c(4, 12), guide = "none") +
  scale_fill_gradient(
    low = chickpea_cols$light,
    high = chickpea_cols$line
  ) +
  labs(
    title = "Circular Carbohydrate Lollipop",
    subtitle = "Top carbohydrate-related compounds",
    x = NULL, y = NULL
  ) +
  theme_chickpea() +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 8, face = "bold", color = chickpea_cols$dark)
  )

ggsave("plots/carb_circular_lollipop.png", p_carb, width = 8, height = 8, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# C) MINERALS — BUBBLE STRIP
# =========================================================
miner_plot <- miner_df %>%
  arrange(desc(value)) %>%
  mutate(category = fct_reorder(category, value))

p_miner <- ggplot(miner_plot, aes(x = value, y = category)) +
  geom_segment(
    aes(x = 0, xend = value, yend = category),
    color = chickpea_cols$light,
    linewidth = 1
  ) +
  geom_point(
    aes(size = value, fill = value),
    shape = 21,
    color = chickpea_cols$dark,
    alpha = 0.95
  ) +
  scale_size(range = c(3, 18), guide = "none") +
  scale_fill_gradient(
    low = chickpea_cols$light,
    high = chickpea_cols$line
  ) +
  labs(
    title = "Mineral Bubble Strip",
    subtitle = "Bubble size and color reflect abundance",
    x = "Value",
    y = NULL
  ) +
  theme_chickpea()

ggsave("plots/mineral_bubble_strip.png", p_miner, width = 9, height = 7, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# D) AMINO ACIDS — FLOWER / PETAL PLOT
# =========================================================
amino_plot <- amino_df %>%
  arrange(desc(value)) %>%
  mutate(category = factor(category, levels = category))

p_amino <- ggplot(amino_plot, aes(x = category, y = value, group = 1)) +
  geom_col(
    fill = chickpea_cols$fill,
    color = chickpea_cols$panel,
    width = 1,
    linewidth = 0.8
  ) +
  coord_polar(start = 0) +
  labs(
    title = "Amino Acid Flower Plot",
    subtitle = "Each petal length reflects relative abundance",
    x = NULL, y = NULL
  ) +
  theme_chickpea() +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 7.8, face = "bold", color = chickpea_cols$dark)
  )

ggsave("plots/amino_flower_plot.png", p_amino, width = 8, height = 8, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# E) FAT CLASSES — ROSE CHART
# =========================================================
fat_plot <- fat_df %>%
  arrange(desc(value)) %>%
  mutate(category = factor(category, levels = category))

p_fat <- ggplot(fat_plot, aes(x = category, y = value, fill = category)) +
  geom_col(
    width = 1,
    color = chickpea_cols$panel,
    linewidth = 0.8
  ) +
  coord_polar() +
  scale_fill_manual(values = c(
    chickpea_cols$fill,
    chickpea_cols$line,
    chickpea_cols$light,
    "#C89C56",
    "#B5873A",
    "#E2C898"
  )) +
  labs(
    title = "Fat Class Rose Chart",
    subtitle = "Compact polar summary of lipid classes",
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = chickpea_cols$bg, color = NA),
    plot.title = element_text(face = "bold", size = 15, color = chickpea_cols$dark),
    plot.subtitle = element_text(size = 10.5, color = chickpea_cols$dark)
  )

ggsave("plots/fat_rose_chart.png", p_fat, width = 7.5, height = 7.5, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# F) FINGERPRINT HEATMAP
# =========================================================
fingerprint_df <- bind_rows(
  carb_df %>% slice_max(value, n = 10),
  miner_df %>% slice_max(value, n = 10),
  amino_df,
  fat_df,
  macro_df
) %>%
  group_by(group) %>%
  mutate(norm = value / max(value)) %>%
  ungroup()

p_fingerprint <- ggplot(
  fingerprint_df,
  aes(x = group, y = fct_reorder(category, norm), fill = norm)
) +
  geom_tile(color = chickpea_cols$panel, linewidth = 0.5) +
  scale_fill_gradient(
    low = chickpea_cols$panel,
    high = chickpea_cols$line
  ) +
  labs(
    title = "Metabolomic Fingerprint Heatmap",
    subtitle = "Within each group, values are normalized to that group's maximum",
    x = NULL,
    y = NULL,
    fill = "Normalized\nvalue"
  ) +
  theme_chickpea() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(face = "bold", color = chickpea_cols$dark),
    panel.grid = element_blank()
  )

ggsave("plots/fingerprint_heatmap.png", p_fingerprint, width = 8.5, height = 10, dpi = 300, bg = chickpea_cols$bg)

cat("Saved chickpea-themed creative plots in the 'plots' folder.\n")


library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(tidyr)

dir.create("plots", showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Chickpea theme helpers
# ---------------------------------------------------------
theme_chickpea_void <- function() {
  theme_void(base_size = 12) +
    theme(
      plot.background = element_rect(fill = chickpea_cols$bg, color = NA),
      panel.background = element_rect(fill = chickpea_cols$bg, color = NA),
      plot.title = element_text(face = "bold", size = 15, color = chickpea_cols$dark),
      plot.subtitle = element_text(size = 10.5, color = chickpea_cols$dark),
      plot.margin = margin(12, 14, 12, 14),
      legend.position = "none"
    )
}

circle_path <- function(r, n = 400) {
  tibble(
    t = seq(0, 2 * pi, length.out = n),
    x = r * cos(t),
    y = r * sin(t)
  )
}

# ---------------------------------------------------------
# 2) Classification helpers
# ---------------------------------------------------------
carb_family <- function(x) {
  case_when(
    x %in% c("Fibers", "Starch", "Glycogen", "Sugar and cellulose") ~ "Polymer",
    x %in% c("GalA", "GlcA", "IdoA") ~ "Uronic acid",
    grepl("N|NAc", x) ~ "Amino sugar",
    TRUE ~ "Neutral sugar"
  )
}

amino_family <- function(x) {
  case_when(
    x %in% c("Histidine", "Isoleucine", "Leucine", "Lysine", "Methionine",
             "Phenylalanine", "Threonine", "Tryptophan", "Valine") ~ "Essential",
    x %in% c("Arginine", "Cysteine", "Glutamine", "Glycine", "Proline", "Tyrosine") ~ "Conditional",
    TRUE ~ "Non-essential"
  )
}

mineral_family <- function(x) {
  case_when(
    x %in% c("Calcium", "Magnesium", "Phosphorus", "Potassium", "Sodium", "Sulfur") ~ "Major",
    x %in% c("Iron", "Zinc", "Copper", "Manganese", "Selenium",
             "Chromium", "Cobalt", "Molybdenum", "Nickel", "Boron") ~ "Trace",
    TRUE ~ "Ultra-trace"
  )
}

# ---------------------------------------------------------
# 3) Geometry builders
# ---------------------------------------------------------
build_orbit_layout <- function(profile_df, inner_r = 1.1, outer_scale = 1.1) {
  d <- profile_df %>%
    arrange(desc(value)) %>%
    mutate(id = row_number())

  n <- nrow(d)
  ang <- seq(pi/2, pi/2 - 2*pi + 2*pi/n, length.out = n)

  d %>%
    mutate(
      angle = ang,
      radius = inner_r + rescale(value, to = c(0.20, outer_scale)),
      x0 = inner_r * cos(angle),
      y0 = inner_r * sin(angle),
      x = radius * cos(angle),
      y = radius * sin(angle),
      xlab = (radius + 0.33) * cos(angle),
      ylab = (radius + 0.33) * sin(angle),
      deg = angle * 180 / pi,
      label_angle = ifelse(deg < -90, deg + 180,
                           ifelse(deg > 90, deg - 180, deg)),
      hjust = ifelse(deg < -90 | deg > 90, 1, 0)
    )
}

build_spiral_layout <- function(profile_df, turns = 2.8) {
  d <- profile_df %>%
    arrange(desc(value)) %>%
    mutate(id = row_number())

  n <- nrow(d)

  d %>%
    mutate(
      theta = seq(pi/2, pi/2 + turns * 2*pi, length.out = n),
      base_r = seq(0.35, 2.6, length.out = n),
      radius = base_r + rescale(value, to = c(0.06, 0.55)),
      x0 = base_r * cos(theta),
      y0 = base_r * sin(theta),
      x = radius * cos(theta),
      y = radius * sin(theta),
      xlab = (radius + 0.23) * cos(theta),
      ylab = (radius + 0.23) * sin(theta),
      deg = (theta * 180 / pi) %% 360,
      label_angle = ifelse(deg > 90 & deg < 270, deg + 180, deg),
      hjust = ifelse(deg > 90 & deg < 270, 1, 0)
    )
}

# ---------------------------------------------------------
# 4) Derived palettes
# ---------------------------------------------------------
carb_pal <- c(
  "Neutral sugar" = chickpea_cols$fill,
  "Polymer" = chickpea_cols$line,
  "Amino sugar" = "#C89448",
  "Uronic acid" = "#8B6731"
)

amino_pal <- c(
  "Essential" = chickpea_cols$line,
  "Conditional" = "#C89448",
  "Non-essential" = chickpea_cols$light
)

mineral_pal <- c(
  "Major" = chickpea_cols$line,
  "Trace" = chickpea_cols$fill,
  "Ultra-trace" = chickpea_cols$light
)

fat_pal <- c(
  "Saturated" = chickpea_cols$fill,
  "Monounsaturated" = chickpea_cols$line,
  "Omega-3" = "#8B6731",
  "Omega-6" = "#C89448",
  "Trans" = "#E2C898",
  "Branched" = "#B8893B"
)

# =========================================================
# A) CARBOHYDRATE ORBIT MAP
# =========================================================
carb_orbit <- carb_df %>%
  mutate(family = carb_family(category)) %>%
  arrange(desc(value)) %>%
  slice_head(n = 14) %>%
  build_orbit_layout(inner_r = 1.0, outer_scale = 1.35)

orbit_ring <- circle_path(1.0)

p_carb_orbit <- ggplot() +
  geom_path(
    data = orbit_ring,
    aes(x, y),
    color = chickpea_cols$light,
    linewidth = 0.9
  ) +
  geom_segment(
    data = carb_orbit,
    aes(x = x0, y = y0, xend = x, yend = y, color = family),
    linewidth = 1.1,
    lineend = "round"
  ) +
  geom_point(
    data = carb_orbit,
    aes(x = x, y = y, size = value, fill = family),
    shape = 21,
    color = chickpea_cols$panel,
    stroke = 1.1,
    alpha = 0.95
  ) +
  geom_text(
    data = carb_orbit,
    aes(x = xlab, y = ylab, label = category, angle = label_angle, hjust = hjust),
    size = 3.1,
    color = chickpea_cols$dark,
    fontface = "bold"
  ) +
  annotate(
    "text", x = 0, y = 0,
    label = "Carbs",
    color = chickpea_cols$dark,
    fontface = "bold",
    size = 5
  ) +
  scale_color_manual(values = carb_pal) +
  scale_fill_manual(values = carb_pal) +
  scale_size(range = c(5, 15), guide = "none") +
  coord_equal() +
  labs(
    title = "Carbohydrate Orbit Map",
    subtitle = "Top compounds arranged like satellites around a core"
  ) +
  theme_chickpea_void()

ggsave("plots/carb_orbit_map.png", p_carb_orbit, width = 8.5, height = 8.5, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# B) AMINO ACID HALO WHEEL
# =========================================================
amino_halo <- amino_df %>%
  mutate(class = amino_family(category)) %>%
  build_orbit_layout(inner_r = 1.25, outer_scale = 1.0)

halo_ring_inner <- circle_path(1.25)
halo_ring_outer <- circle_path(2.45)

p_amino_halo <- ggplot() +
  geom_path(
    data = halo_ring_inner,
    aes(x, y),
    color = chickpea_cols$light,
    linewidth = 0.8
  ) +
  geom_path(
    data = halo_ring_outer,
    aes(x, y),
    color = chickpea_cols$light,
    linewidth = 0.5,
    alpha = 0.8
  ) +
  geom_segment(
    data = amino_halo,
    aes(x = x0, y = y0, xend = x, yend = y, color = class),
    linewidth = 1.15,
    alpha = 0.95
  ) +
  geom_point(
    data = amino_halo,
    aes(x = x, y = y, fill = class, size = value),
    shape = 21,
    color = chickpea_cols$panel,
    stroke = 1
  ) +
  geom_text(
    data = amino_halo,
    aes(x = xlab, y = ylab, label = category, angle = label_angle, hjust = hjust),
    size = 3,
    color = chickpea_cols$dark
  ) +
  annotate(
    "label", x = 0, y = 0,
    label = "Amino\nAcids",
    fill = chickpea_cols$panel,
    color = chickpea_cols$dark,
    label.size = 0.25,
    size = 4.5,
    fontface = "bold"
  ) +
  scale_color_manual(values = amino_pal) +
  scale_fill_manual(values = amino_pal) +
  scale_size(range = c(3.5, 10), guide = "none") +
  coord_equal() +
  labs(
    title = "Amino Acid Halo Wheel",
    subtitle = "A ringed profile with essentiality classes"
  ) +
  theme_chickpea_void()

ggsave("plots/amino_halo_wheel.png", p_amino_halo, width = 9, height = 9, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# C) MINERAL SPIRAL CONSTELLATION
# =========================================================
miner_spiral <- miner_df %>%
  mutate(class = mineral_family(category)) %>%
  arrange(desc(value)) %>%
  build_spiral_layout(turns = 3.0)

p_miner_spiral <- ggplot() +
  geom_path(
    data = miner_spiral,
    aes(x = x0, y = y0),
    color = chickpea_cols$light,
    linewidth = 1
  ) +
  geom_segment(
    data = miner_spiral,
    aes(x = x0, y = y0, xend = x, yend = y, color = class),
    linewidth = 1
  ) +
  geom_point(
    data = miner_spiral,
    aes(x = x, y = y, fill = class, size = value),
    shape = 21,
    color = chickpea_cols$dark,
    stroke = 0.8,
    alpha = 0.95
  ) +
  geom_text(
    data = miner_spiral %>% slice(c(1:12)),
    aes(x = xlab, y = ylab, label = category, angle = label_angle, hjust = hjust),
    size = 3,
    color = chickpea_cols$dark
  ) +
  annotate(
    "text", x = 0, y = 0,
    label = "Mineral\nSpiral",
    color = chickpea_cols$dark,
    fontface = "bold",
    size = 5
  ) +
  scale_color_manual(values = mineral_pal) +
  scale_fill_manual(values = mineral_pal) +
  scale_size(range = c(2.5, 14), guide = "none") +
  coord_equal() +
  labs(
    title = "Mineral Spiral Constellation",
    subtitle = "Major and trace elements unfolding outward by abundance"
  ) +
  theme_chickpea_void()

ggsave("plots/mineral_spiral_constellation.png", p_miner_spiral, width = 9, height = 9, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# D) MACRO SOLAR SYSTEM
# =========================================================
macro_solar <- macro_df %>%
  arrange(desc(value)) %>%
  mutate(
    share = value / sum(value),
    orbit = seq(0.8, 2.8, length.out = n()),
    angle = c(0.5, 1.7, 2.9, 4.2, 5.3)[seq_len(n())],
    x = orbit * cos(angle),
    y = orbit * sin(angle),
    xlab = (orbit + 0.34) * cos(angle),
    ylab = (orbit + 0.34) * sin(angle)
  )

orbit_lines <- bind_rows(lapply(macro_solar$orbit, circle_path), .id = "orbit_id")

p_macro_solar <- ggplot() +
  geom_path(
    data = orbit_lines,
    aes(x, y, group = orbit_id),
    color = chickpea_cols$light,
    linewidth = 0.8
  ) +
  annotate(
    "point", x = 0, y = 0,
    shape = 21, size = 16,
    fill = chickpea_cols$fill,
    color = chickpea_cols$line,
    stroke = 1.2
  ) +
  annotate(
    "text", x = 0, y = 0,
    label = "GGB100039",
    color = chickpea_cols$dark,
    fontface = "bold",
    size = 3.7
  ) +
  geom_point(
    data = macro_solar,
    aes(x = x, y = y, size = share),
    shape = 21,
    fill = chickpea_cols$line,
    color = chickpea_cols$panel,
    stroke = 1.1,
    alpha = 0.95
  ) +
  geom_curve(
    data = macro_solar,
    aes(x = 0, y = 0, xend = x, yend = y),
    curvature = 0.15,
    color = chickpea_cols$light,
    linewidth = 0.6,
    alpha = 0.7
  ) +
  geom_label(
    data = macro_solar,
    aes(x = xlab, y = ylab, label = paste0(category, "\n", label_num(share * 100, 0.1), "%")),
    fill = chickpea_cols$panel,
    color = chickpea_cols$dark,
    label.size = 0.2,
    size = 3.1,
    fontface = "bold"
  ) +
  scale_size(range = c(7, 22), guide = "none") +
  coord_equal() +
  labs(
    title = "Macro Solar System",
    subtitle = "Macronutrients shown as planets orbiting the sample core"
  ) +
  theme_chickpea_void()

ggsave("plots/macro_solar_system.png", p_macro_solar, width = 9, height = 9, dpi = 300, bg = chickpea_cols$bg)

# =========================================================
# E) FAT COMPASS
# =========================================================
fat_compass <- fat_df %>%
  arrange(desc(value)) %>%
  mutate(
    angle = seq(pi/2, pi/2 - 2*pi + 2*pi/n(), length.out = n()),
    radius = rescale(value, to = c(1.0, 2.5)),
    x = radius * cos(angle),
    y = radius * sin(angle),
    xlab = (radius + 0.35) * cos(angle),
    ylab = (radius + 0.35) * sin(angle)
  )

compass_rings <- bind_rows(
  circle_path(1.0) %>% mutate(ring = "r1"),
  circle_path(1.5) %>% mutate(ring = "r2"),
  circle_path(2.0) %>% mutate(ring = "r3"),
  circle_path(2.5) %>% mutate(ring = "r4")
)

p_fat_compass <- ggplot() +
  geom_path(
    data = compass_rings,
    aes(x, y, group = ring),
    color = chickpea_cols$light,
    linewidth = 0.6
  ) +
  geom_segment(
    data = fat_compass,
    aes(x = 0, y = 0, xend = x, yend = y, color = category),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_point(
    data = fat_compass,
    aes(x = x, y = y, fill = category),
    shape = 21,
    size = 6,
    color = chickpea_cols$panel,
    stroke = 1
  ) +
  geom_label(
    data = fat_compass,
    aes(x = xlab, y = ylab, label = paste0(category, "\n", label_num(value))),
    fill = chickpea_cols$panel,
    color = chickpea_cols$dark,
    label.size = 0.2,
    size = 3
  ) +
  annotate(
    "label", x = 0, y = 0,
    label = "Fat\nCompass",
    fill = chickpea_cols$panel,
    color = chickpea_cols$dark,
    label.size = 0.25,
    fontface = "bold",
    size = 4.2
  ) +
  scale_color_manual(values = fat_pal) +
  scale_fill_manual(values = fat_pal) +
  coord_equal() +
  labs(
    title = "Fat Compass",
    subtitle = "A directional summary of lipid classes"
  ) +
  theme_chickpea_void()

ggsave("plots/fat_compass.png", p_fat_compass, width = 8.5, height = 8.5, dpi = 300, bg = chickpea_cols$bg)