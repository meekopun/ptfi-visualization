# install.packages(c("ggplot2", "dplyr", "tidyr", "forcats", "scales"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)

# ---------------------------------------------------------
# 1) Read TSV
# ---------------------------------------------------------
file <- "Dashboard_Viewer/Chickpea/chickpea_metabolomics.tsv"
df <- read.delim(
  file,
  sep = "\t",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

id_cols <- c("sample_name", "sample_id")

for (nm in names(df)) {
  if (!nm %in% id_cols) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
}

df$sample_name <- trimws(df$sample_name)

chickpea_name <- "chickpea"
if (!chickpea_name %in% df$sample_name) {
  stop("Could not find sample_name == 'chickpea'")
}

dir.create("plots", showWarnings = FALSE)

# ---------------------------------------------------------
# 2) Category groups
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

feature_groups <- list(
  Macros = macro_cols,
  `Fat classes` = fat_class_cols,
  `Amino acids` = amino_cols,
  Carbohydrates = carb_cols,
  Minerals = mineral_cols
)

feature_group_map <- bind_rows(lapply(names(feature_groups), function(g) {
  data.frame(
    feature = feature_groups[[g]],
    group = g,
    stringsAsFactors = FALSE
  )
})) %>%
  distinct() %>%
  filter(feature %in% names(df))

all_compare_cols <- unique(feature_group_map$feature)

# ---------------------------------------------------------
# 3) Chickpea theme system
# ---------------------------------------------------------
chickpea_cols <- list(
  bg      = "#FFF8E8",
  panel   = "#FFFDF7",
  light   = "#EADFC8",
  fill    = "#D8B67A",
  line    = "#A37A35",
  dark    = "#5E4724"
)

group_cols <- c(
  Macros = chickpea_cols$line,
  `Fat classes` = "#B8893B",
  `Amino acids` = "#8B6731",
  Carbohydrates = "#C89448",
  Minerals = chickpea_cols$fill
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

theme_set(theme_chickpea())

label_num <- function(x, acc = 0.001) {
  number(x, accuracy = acc, trim = TRUE)
}

circle_path <- function(r, n = 360) {
  data.frame(
    t = seq(0, 2 * pi, length.out = n),
    x = r * cos(seq(0, 2 * pi, length.out = n)),
    y = r * sin(seq(0, 2 * pi, length.out = n))
  )
}

# ---------------------------------------------------------
# 4) Long table
# ---------------------------------------------------------
all_long <- df %>%
  select(sample_name, sample_id, all_of(all_compare_cols)) %>%
  pivot_longer(
    cols = all_of(all_compare_cols),
    names_to = "feature",
    values_to = "value"
  ) %>%
  left_join(feature_group_map, by = "feature") %>%
  mutate(
    is_chickpea = sample_name == chickpea_name
  )

# ---------------------------------------------------------
# 5) Plot 1 — Macro spotlight range
# Chickpea sits against the range of other foods
# ---------------------------------------------------------
macro_long <- all_long %>%
  filter(group == "Macros") %>%
  mutate(feature = factor(feature, levels = rev(c("Water", "Carbohydrate", "Protein", "Lipid", "Ash"))))

macro_ref <- macro_long %>%
  filter(!is_chickpea) %>%
  group_by(feature) %>%
  summarise(
    min_other = min(value, na.rm = TRUE),
    max_other = max(value, na.rm = TRUE),
    med_other = median(value, na.rm = TRUE),
    .groups = "drop"
  )

macro_chickpea <- macro_long %>%
  filter(is_chickpea) %>%
  select(feature, chickpea_value = value)

macro_plot_df <- macro_ref %>%
  left_join(macro_chickpea, by = "feature")

p_macro_spotlight <- ggplot() +
  geom_segment(
    data = macro_plot_df,
    aes(x = min_other, xend = max_other, y = feature, yend = feature),
    color = chickpea_cols$light,
    linewidth = 5,
    lineend = "round"
  ) +
  geom_point(
    data = macro_long %>% filter(!is_chickpea),
    aes(x = value, y = feature),
    color = chickpea_cols$line,
    alpha = 0.35,
    size = 3
  ) +
  geom_point(
    data = macro_plot_df,
    aes(x = med_other, y = feature),
    color = chickpea_cols$dark,
    fill = chickpea_cols$panel,
    shape = 21,
    stroke = 0.8,
    size = 3
  ) +
  geom_curve(
    data = macro_plot_df,
    aes(x = med_other, y = feature, xend = chickpea_value, yend = feature),
    curvature = 0.18,
    color = chickpea_cols$line,
    linewidth = 0.8,
    alpha = 0.7
  ) +
  geom_point(
    data = macro_plot_df,
    aes(x = chickpea_value, y = feature),
    fill = chickpea_cols$fill,
    color = chickpea_cols$dark,
    shape = 21,
    stroke = 1.1,
    size = 6
  ) +
  geom_text(
    data = macro_plot_df,
    aes(x = chickpea_value, y = feature, label = label_num(chickpea_value, 0.1)),
    nudge_x = max(macro_long$value, na.rm = TRUE) * 0.03,
    color = chickpea_cols$dark,
    fontface = "bold",
    size = 3.4,
    hjust = 0
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = "Chickpeas among legumes",
    subtitle = "Each band shows the range of the other foods; the gold point is chickpea",
    x = "Value",
    y = NULL
  ) +
  theme_chickpea()

ggsave("plots/chickpea_macro_spotlight.png", p_macro_spotlight, width = 9, height = 5.5, dpi = 300, bg = chickpea_cols$bg)

getwd()
list.files()
list.files("plots")

# ---------------------------------------------------------
# 6) Plot 2 — Amino halo comparison
# Other foods are ghosted; chickpea gets the spotlight
# ---------------------------------------------------------
amino_long <- all_long %>%
  filter(group == "Amino acids") %>%
  mutate(feature = factor(feature, levels = amino_cols)) %>%
  group_by(feature) %>%
  mutate(norm = ifelse(max(value, na.rm = TRUE) > 0, value / max(value, na.rm = TRUE), 0)) %>%
  ungroup()

p_amino_halo <- ggplot() +
  geom_polygon(
    data = amino_long %>% filter(!is_chickpea),
    aes(x = feature, y = norm, group = sample_name),
    fill = NA,
    color = chickpea_cols$light,
    linewidth = 0.7,
    alpha = 0.8
  ) +
  geom_line(
    data = amino_long %>% filter(!is_chickpea),
    aes(x = feature, y = norm, group = sample_name),
    color = chickpea_cols$line,
    linewidth = 0.45,
    alpha = 0.35
  ) +
  geom_polygon(
    data = amino_long %>% filter(is_chickpea),
    aes(x = feature, y = norm, group = sample_name),
    fill = alpha(chickpea_cols$fill, 0.35),
    color = chickpea_cols$dark,
    linewidth = 1.2
  ) +
  geom_line(
    data = amino_long %>% filter(is_chickpea),
    aes(x = feature, y = norm, group = sample_name),
    color = chickpea_cols$dark,
    linewidth = 1.2
  ) +
  geom_point(
    data = amino_long %>% filter(is_chickpea),
    aes(x = feature, y = norm),
    size = 2,
    color = chickpea_cols$dark
  ) +
  coord_polar() +
  labs(
    title = "Chickpea Amino Halo",
    subtitle = "Chickpea is filled in gold; all other foods form the background halo",
    x = NULL, y = NULL
  ) +
  theme_chickpea() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 7.5, face = "bold", color = chickpea_cols$dark)
  )

ggsave("plots/chickpea_amino_halo.png", p_amino_halo, width = 8.5, height = 8.5, dpi = 300, bg = chickpea_cols$bg)

# ---------------------------------------------------------
# 7) Plot 3 — Similarity constellation
# Foods orbit chickpea by overall profile distance
# ---------------------------------------------------------
dist_wide <- df %>%
  select(sample_name, all_of(all_compare_cols))

dist_mat <- as.matrix(dist_wide[, -1, drop = FALSE])

col_max <- apply(dist_mat, 2, max, na.rm = TRUE)
col_max[!is.finite(col_max) | col_max == 0] <- 1

dist_norm <- sweep(dist_mat, 2, col_max, "/")
rownames(dist_norm) <- dist_wide$sample_name

chick_vec <- dist_norm[chickpea_name, ]

dist_tbl <- data.frame(
  sample_name = rownames(dist_norm),
  distance = apply(dist_norm, 1, function(x) sqrt(sum((x - chick_vec)^2, na.rm = TRUE))),
  stringsAsFactors = FALSE
) %>%
  left_join(df %>% select(sample_name, Protein, Carbohydrate), by = "sample_name") %>%
  mutate(is_chickpea = sample_name == chickpea_name)

others <- dist_tbl %>%
  filter(!is_chickpea) %>%
  arrange(distance) %>%
  mutate(
    angle = seq(pi/2, pi/2 - 2*pi + 2*pi/n(), length.out = n()),
    x = distance * cos(angle),
    y = distance * sin(angle),
    xlab = (distance + max(distance) * 0.12) * cos(angle),
    ylab = (distance + max(distance) * 0.12) * sin(angle)
  )

ring_vals <- pretty(c(0, max(others$distance)), n = 4)
ring_vals <- ring_vals[ring_vals > 0]

rings <- bind_rows(lapply(ring_vals, function(r) {
  circle_path(r) %>% mutate(ring = r)
}))

p_constellation <- ggplot() +
  geom_path(
    data = rings,
    aes(x = x, y = y, group = ring),
    color = chickpea_cols$light,
    linewidth = 0.7
  ) +
  geom_curve(
    data = others,
    aes(x = 0, y = 0, xend = x, yend = y),
    curvature = 0.12,
    color = chickpea_cols$light,
    linewidth = 0.6
  ) +
  annotate(
    "point", x = 0, y = 0,
    shape = 21, size = 11,
    fill = chickpea_cols$fill,
    color = chickpea_cols$dark,
    stroke = 1.2
  ) +
  annotate(
    "text", x = 0, y = 0,
    label = "chickpea",
    color = chickpea_cols$dark,
    fontface = "bold",
    size = 3.8
  ) +
  geom_point(
    data = others,
    aes(x = x, y = y, size = Protein),
    shape = 21,
    fill = chickpea_cols$line,
    color = chickpea_cols$panel,
    stroke = 1
  ) +
  geom_label(
    data = others,
    aes(x = xlab, y = ylab, label = sample_name),
    fill = chickpea_cols$panel,
    color = chickpea_cols$dark,
    label.size = 0.2,
    size = 3
  ) +
  scale_size(range = c(6, 14), guide = "none") +
  coord_equal() +
  labs(
    title = "Chickpea Similarity Constellation",
    subtitle = "Foods closer to the center are more profile-similar to chickpea"
  ) +
  theme_chickpea_void()

ggsave("plots/chickpea_similarity_constellation.png", p_constellation, width = 8.5, height = 8.5, dpi = 300, bg = chickpea_cols$bg)

# ---------------------------------------------------------
# 8) Plot 4 — Chickpea signature skyline
# Features where chickpea stands above or below peers
# ---------------------------------------------------------
eps <- 1e-6

advantage <- all_long %>%
  group_by(feature, group) %>%
  summarise(
    chickpea_value = value[sample_name == chickpea_name][1],
    peers_median = median(value[sample_name != chickpea_name], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    score = log2((chickpea_value + eps) / (peers_median + eps)),
    direction = ifelse(score >= 0, "Higher in chickpea", "Lower in chickpea"),
    abs_score = abs(score)
  ) %>%
  arrange(desc(abs_score)) %>%
  slice_head(n = 18) %>%
  mutate(feature = fct_reorder(feature, score))

p_skyline <- ggplot(advantage, aes(x = score, y = feature, color = group)) +
  geom_vline(xintercept = 0, color = chickpea_cols$light, linewidth = 0.8) +
  geom_segment(
    aes(x = 0, xend = score, y = feature, yend = feature),
    linewidth = 1.6,
    lineend = "round"
  ) +
  geom_point(size = 4) +
  scale_color_manual(values = group_cols) +
  labs(
    title = "Chickpea Signature Skyline",
    subtitle = "Positive values mean chickpea is higher than the median of the comparison foods",
    x = "log2 ratio vs peer median",
    y = NULL
  ) +
  theme_chickpea() +
  theme(legend.position = "right")

ggsave("plots/chickpea_signature_skyline.png", p_skyline, width = 9.5, height = 7.5, dpi = 300, bg = chickpea_cols$bg)

# ---------------------------------------------------------
# 9) Plot 5 — Chickpea spotlight heatmap
# Top discriminatory features across all foods
# ---------------------------------------------------------
top_features <- advantage %>%
  arrange(desc(abs_score)) %>%
  pull(feature)

heat_df <- all_long %>%
  filter(feature %in% top_features) %>%
  group_by(feature) %>%
  mutate(
    scaled = ifelse(max(value, na.rm = TRUE) > min(value, na.rm = TRUE),
                    (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)),
                    0.5),
    signed_scaled = scaled * 2 - 1
  ) %>%
  ungroup() %>%
  mutate(
    sample_name = factor(sample_name, levels = c(chickpea_name, setdiff(unique(df$sample_name), chickpea_name))),
    feature = factor(feature, levels = rev(top_features)),
    tile_outline = ifelse(sample_name == chickpea_name, chickpea_cols$dark, chickpea_cols$panel)
  )

p_heatmap <- ggplot(heat_df, aes(x = feature, y = sample_name, fill = signed_scaled, color = tile_outline)) +
  geom_tile(linewidth = 0.9) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = chickpea_cols$light,
    mid = chickpea_cols$panel,
    high = chickpea_cols$line,
    midpoint = 0
  ) +
  labs(
    title = "Chickpea Spotlight Heatmap",
    subtitle = "The chickpea row is outlined; darker tiles indicate relatively higher values within each feature",
    x = NULL,
    y = NULL,
    fill = "Relative\nlevel"
  ) +
  theme_chickpea() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggsave("plots/chickpea_spotlight_heatmap.png", p_heatmap, width = 12, height = 5.8, dpi = 300, bg = chickpea_cols$bg)

cat("Saved 5 chickpea-centered comparison plots in the 'plots' folder.\n")


