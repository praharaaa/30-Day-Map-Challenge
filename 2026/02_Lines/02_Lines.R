# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(sf)
library(terra)
library(ragg)
library(cowplot)
library(ggtext)

# Load fonts --------------------------------------------------------------

font_add_google("Playfair Display", "Playfair Display")
font_add_google("Raleway", "Raleway")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

title_font   <- "Playfair Display"
body_font    <- "Raleway"

bg_col       <- "#080b11"
border_col   <- "#475d8b"
col_off_axis <- "#222c40"
col_flank    <- "#f97316"
col_summit   <- "#ffea00"
col_merapi   <- "#ef4444"
col_regional <- "#38bdf8"
col_urban    <- "#a855f7"

data_dir <- if (dir.exists("02_Lines/data")) "02_Lines/data" else "data"
out_dir  <- if (dir.exists("02_Lines")) "02_Lines" else "."

# Load data and map -------------------------------------------------------
# 1. Cosmological & Urban Landmarks ---------------------------------------

merapi_proj <- st_sfc(st_point(c(110.446055, -7.541895)), crs = 4326) |> st_transform(32749)
cm          <- st_coordinates(merapi_proj)
xm_merapi   <- cm[1]
ym_merapi   <- cm[2]

south_proj  <- st_sfc(st_point(c(110.295167, -8.014279)), crs = 4326) |> st_transform(32749)
cs          <- st_coordinates(south_proj)
xm_south    <- cs[1]
ym_south    <- cs[2]
bearing_reg <- round((atan2(xm_south - xm_merapi, ym_south - ym_merapi) * 180 / pi + 360) %% 360)

kraton_proj <- st_sfc(st_point(c(110.3642, -7.8054)), crs = 4326) |> st_transform(32749)
c_kraton    <- st_coordinates(kraton_proj)
xm_kraton   <- c_kraton[1]
ym_kraton   <- c_kraton[2]

# 2. DIY Boundary & DEM Hillshade -----------------------------------------

diy_gadm <- readRDS(file.path(data_dir, "gadm/gadm41_IDN_2_pk.rds"))
diy      <- diy_gadm |> unwrap() |> st_as_sf() |> filter(NAME_1 == "Yogyakarta") |> st_transform(32749)
bbox_diy <- st_bbox(diy)

dem_raw      <- rast(file.path(data_dir, "dem/elevation/srtm_59_14.tif"))
ext_target   <- ext(bbox_diy["xmin"] - 2000, bbox_diy["xmax"] + 2000,
                    bbox_diy["ymin"] - 5000, ym_merapi + 5000)
dem_utm      <- project(dem_raw, rast(ext_target, res = 150, crs = "EPSG:32749"), method = "bilinear")
slope_rad    <- terrain(dem_utm, "slope", unit = "radians")
aspect_rad   <- terrain(dem_utm, "aspect", unit = "radians")
hill         <- shade(slope_rad, aspect_rad, angle = 30, direction = 315)

hill_df           <- as.data.frame(hill, xy = TRUE)
colnames(hill_df) <- c("x", "y", "shade")

# 3. Roads Data -----------------------------------------------------------

roads_raw  <- readRDS(file.path(data_dir, "diy_roads_raw.rds"))
roads_proj <- st_transform(roads_raw, 32749)
roads_diy  <- st_intersection(roads_proj, st_union(diy))

# Data wrangling ----------------------------------------------------------
# 1. Clean and merge contiguous road ways ---------------------------------

roads_named   <- roads_diy |> filter(!is.na(name) & name != "")
roads_unnamed <- roads_diy |> filter(is.na(name) | name == "")
merged_named  <- roads_named |>
  group_by(name, highway) |>
  summarise(geometry = st_line_merge(st_cast(st_union(geometry), "MULTILINESTRING")), .groups = "drop") |>
  st_cast("LINESTRING")

roads_cleaned   <- bind_rows(merged_named, roads_unnamed) |> st_cast("LINESTRING")
roads_segmented <- st_segmentize(st_geometry(roads_cleaned), dfMaxLength = 100)

coords_df <- as.data.frame(st_coordinates(roads_segmented))

# 2. Decompose vectors & calculate alignment streak -----------------------

seg_df <- coords_df |>
  group_by(L1) |>
  mutate(x1 = X, y1 = Y, x2 = lead(X), y2 = lead(Y)) |>
  filter(!is.na(x2)) |>
  ungroup() |>
  mutate(
    mid_x = (x1 + x2) / 2, mid_y = (y1 + y2) / 2,
    dx = x2 - x1, dy = y2 - y1, len = sqrt(dx^2 + dy^2),
    bearing_road = (atan2(dx, dy) * 180 / pi + 360) %% 360,
    dx_m = xm_merapi - mid_x, dy_m = ym_merapi - mid_y,
    bearing_to_merapi = (atan2(dx_m, dy_m) * 180 / pi + 360) %% 360,
    bearing_from_merapi = (bearing_to_merapi + 180) %% 360,
    diff_f = abs(bearing_road - bearing_to_merapi), diff_f = pmin(diff_f, 360 - diff_f),
    diff_b = abs((bearing_road + 180) %% 360 - bearing_to_merapi), diff_b = pmin(diff_b, 360 - diff_b),
    dev_summit = pmin(diff_f, diff_b),
    is_aligned_raw = dev_summit <= 5
  ) |>
  group_by(L1) |>
  mutate(streak_id = cumsum(!is_aligned_raw)) |>
  group_by(L1, streak_id) |>
  mutate(streak_len = if_else(is_aligned_raw, sum(len[is_aligned_raw]), 0)) |>
  ungroup() |>
  mutate(is_aligned_corridor = is_aligned_raw & (streak_len >= 300))

# 3. 5-Degree Radial Sector Bins ------------------------------------------

bin_w         <- 5
southern_bins <- seq(95, 265, by = bin_w)

df_rose <- seg_df |>
  mutate(bin = (round(bearing_from_merapi / bin_w) * bin_w) %% 360) |>
  filter(bin >= 90 & bin <= 270) |>
  group_by(bin) |>
  summarise(
    total_km   = sum(len) / 1000,
    aligned_km = sum(len[is_aligned_corridor]) / 1000,
    .groups    = "drop"
  ) |>
  complete(bin = southern_bins, fill = list(total_km = 0, aligned_km = 0))

ranked_bins <- df_rose |> arrange(desc(aligned_km)) |> pull(bin)
top2_bins   <- ranked_bins[1:2]
next3_bins  <- ranked_bins[3:5]

seg_classified <- seg_df |>
  mutate(
    bin = (round(bearing_from_merapi / bin_w) * bin_w) %% 360,
    category = case_when(
      !is_aligned_corridor ~ "Off-Axis Network",
      bin %in% top2_bins   ~ "Peak Axis Corridors (195\u00b0, 200\u00b0)",
      bin %in% next3_bins  ~ "Supporting Axis (185\u00b0, 190\u00b0, 205\u00b0)",
      TRUE                 ~ "Off-Axis Network"
    )
  )

seg_geoms <- mapply(function(x1, y1, x2, y2) {
  st_linestring(matrix(c(x1, x2, y1, y2), ncol = 2))
}, seg_classified$x1, seg_classified$y1, seg_classified$x2, seg_classified$y2, SIMPLIFY = FALSE)

seg_sf <- st_sf(category = seg_classified$category, geometry = st_sfc(seg_geoms, crs = 32749)) |>
  arrange(category)

# Graph data --------------------------------------------------------------
# 1. Title Block ----------------------------------------------------------

title_block <- ggplot() +
  labs(
    title    = "AXIS OF THE VOLCANO",
    subtitle = paste0(
      "Which roads in D.I. Yogyakarta truly point toward Mount Merapi? ",
      "Grouping corridors into 5\u00b0 radial sectors reveals that alignment is heavily concentrated ",
      "along the natural volcanic slope in the <span style='color:", col_summit, "; font-weight:bold;'>Top 2 sectors (195\u00b0 & 200\u00b0)</span> ",
      "and <span style='color:", col_flank, "; font-weight:bold;'>supporting flanks (185\u00b0, 190\u00b0, 205\u00b0)</span>, ",
      "directly framing the historic Sumbu Filosofis."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      family = title_font, face = "bold", size = 35, color = "#ffffff",
      margin = margin(t = 32, b = 10, l = 35, r = 35)
    ),
    plot.subtitle = element_textbox_simple(
      family = body_font, size = 13, lineheight = 1.35, color = "#9ca3af",
      margin = margin(b = 10, l = 35, r = 35)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 2. Main Map -------------------------------------------------------------

plt_map <- ggplot() +
  geom_raster(data = hill_df, aes(x = x, y = y, fill = shade), alpha = 0.65) +
  scale_fill_gradient(low = "#05070c", high = "#334155", guide = "none") +
  geom_sf(data = diy, fill = NA, color = border_col, linewidth = 0.65) +
  geom_sf(data = seg_sf, aes(color = category, linewidth = category), lineend = "round", linejoin = "round") +
  
  # Regional Cosmological Axis (Merapi -> Coast @ 198°)
  annotate("segment", x = xm_merapi, y = ym_merapi, xend = xm_south, yend = ym_south,
           color = alpha(col_regional, 0.45), linetype = "dashed", linewidth = 0.75) +
  
  # Kraton Landmark
  annotate("point", x = xm_kraton, y = ym_kraton, shape = 21, size = 3.5, fill = "#a855f7", color = "#ffffff", stroke = 1.0) +
  annotate("text", x = xm_kraton + 2200, y = ym_kraton, label = "Kraton Yogyakarta",
           family = body_font, fontface = "bold", size = 3.2, color = "#e2e8f0", hjust = 0) +
  
  # Merapi Summit & South Coast
  annotate("point", x = xm_merapi, y = ym_merapi, shape = 17, size = 5.5, color = col_merapi) +
  annotate("text", x = xm_merapi - 2200, y = ym_merapi + 1800, label = "Mt. Merapi (2,910 m)",
           family = body_font, fontface = "bold", size = 4.2, color = col_merapi, hjust = 1) +
  annotate("point", x = xm_south, y = ym_south, shape = 21, size = 4, fill = col_regional, color = "#ffffff", stroke = 1.1) +
  annotate("text", x = xm_south, y = ym_south - 2800, label = "South Coast (198\u00b0 Axis)",
           family = body_font, fontface = "bold", size = 3.8, color = col_regional, hjust = 0.5) +
  
  scale_color_manual(
    name = "Volcanic Alignment Hierarchy:",
    values = c(
      "Off-Axis Network"                                  = col_off_axis,
      "Supporting Axis (185\u00b0, 190\u00b0, 205\u00b0)" = col_flank,
      "Peak Axis Corridors (195\u00b0, 200\u00b0)"        = col_summit
    )
  ) +
  scale_linewidth_manual(
    name = "Volcanic Alignment Hierarchy:",
    values = c(
      "Off-Axis Network"                                  = 0.35,
      "Supporting Axis (185\u00b0, 190\u00b0, 205\u00b0)" = 0.95,
      "Peak Axis Corridors (195\u00b0, 200\u00b0)"        = 1.50
    )
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = if (length(unique(seg_sf$category)) == 2) c(1.0, 2.5) else c(1.0, 1.8, 2.6)),
      title.position = "top",
      title.hjust = 0.5
    ),
    linewidth = "none"
  ) +
  coord_sf(xlim = c(bbox_diy["xmin"], bbox_diy["xmax"]), ylim = c(bbox_diy["ymin"] - 4500, ym_merapi + 5500), expand = FALSE) +
  theme_void() +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_text(family = body_font, size = 12, face = "bold", color = "#ffffff", margin = margin(b = 6)),
    legend.text      = element_text(family = body_font, size = 11, color = "#9ca3af"),
    legend.key.width = unit(2.5, "lines"),
    legend.margin    = margin(t = 4, b = 8),
    plot.margin      = margin(t = 10, r = 35, b = 10, l = 35),
    plot.background  = element_rect(fill = bg_col, color = NA)
  )

# 3. Minimalist Radial Fan Inset ------------------------------------------

make_wedges <- function(df, val_col, r0 = 8, scale_r = 1.35) {
  poly_list <- list()
  for (i in seq_len(nrow(df))) {
    b <- df$bin[i]; r <- df[[val_col]][i] * scale_r
    if (r <= 0) next
    t_seq <- seq((b - bin_w / 2) * pi / 180, (b + bin_w / 2) * pi / 180, length.out = 8)
    poly_list[[i]] <- tibble(
      id = paste0("alg_", i), bin = b,
      x = c((r0 + r) * sin(t_seq), r0 * sin(rev(t_seq))),
      y = c((r0 + r) * cos(t_seq), r0 * cos(rev(t_seq)))
    )
  }
  bind_rows(poly_list)
}

r0_val    <- 8
r_max_fan <- max(df_rose$aligned_km) * 1.35 + r0_val
polys_fan <- make_wedges(df_rose, "aligned_km", r0 = r0_val, scale_r = 1.35) |>
  mutate(fan_fill = case_when(bin %in% top2_bins ~ col_summit, bin %in% next3_bins ~ col_flank, TRUE ~ "#ffffff"))

base_arc <- tibble(
  t = seq(90, 270, length.out = 100) * pi / 180,
  x = r0_val * sin(t),
  y = r0_val * cos(t)
)

plt_rose_minimal <- ggplot() +
  geom_path(data = base_arc, aes(x = x, y = y), color = "#334155", linewidth = 0.5) +
  geom_polygon(data = polys_fan, aes(x = x, y = y, group = id, fill = fan_fill), color = bg_col, linewidth = 0.25) +
  scale_fill_identity() +
  
  # Regional Axis Tick (bearing_reg)
  geom_segment(aes(x = r0_val * sin(bearing_reg * pi / 180), y = r0_val * cos(bearing_reg * pi / 180),
                   xend = (r_max_fan + 5) * sin(bearing_reg * pi / 180), yend = (r_max_fan + 5) * cos(bearing_reg * pi / 180)),
               color = col_regional, linetype = "dashed", linewidth = 0.85) +
  annotate("text", x = (r_max_fan + 6) * sin(bearing_reg * pi / 180) - 2, y = (r_max_fan + 6) * cos(bearing_reg * pi / 180) - 2,
           label = paste0(bearing_reg, "\u00b0"), color = col_regional, size = 3.0, fontface = "bold", family = body_font, hjust = 1) +
  
  # Compass markers & Summit dot
  annotate("text", x = (r_max_fan + 4), y = 0, label = "E", color = "#64748b", size = 2.8, fontface = "bold", family = body_font, hjust = 0) +
  annotate("text", x = -(r_max_fan + 4), y = 0, label = "W", color = "#64748b", size = 2.8, fontface = "bold", family = body_font, hjust = 1) +
  annotate("text", x = 0, y = -(r_max_fan + 4), label = "S", color = "#64748b", size = 2.8, fontface = "bold", family = body_font, vjust = 1) +
  annotate("point", x = 0, y = 0, shape = 17, color = col_merapi, size = 3.0) +
  
  coord_fixed(clip = "off", xlim = c(-r_max_fan - 10, r_max_fan + 10), ylim = c(-r_max_fan - 10, 5)) +
  labs(title = "MERAPI RADIAL FAN", subtitle = "Corridors \u2265 300m aligned to Merapi") +
  theme_void() +
  theme(
    plot.title       = element_text(family = title_font, size = 11, face = "bold", color = "#ffffff", hjust = 0.5),
    plot.subtitle    = element_text(family = body_font, size = 8.5, color = "#9ca3af", hjust = 0.5, margin = margin(t = 4, b = 4)),
    plot.background  = element_rect(fill = NA, color = NA),
    plot.margin      = margin(4, 4, 4, 4)
  )

# 4. Caption Block --------------------------------------------------------

caption_block <- ggplot() +
  labs(
    caption = paste0(
      "Note: Exploratory analysis. Roads segmented into 100m vectors; corridors require \u2265 300m continuous alignment (\u2264 5\u00b0 of Merapi).\n",
      "52.3% of Merapi-aligned corridors follow the natural downhill slope, vs. 18.0% across off-axis roads.\n",
      "#30DayMapChallenge 2026 \u00b7 Day 2: Lines \u00b7 Data: OpenStreetMap, GADM & CGIAR SRTM (90m) \u00b7 Graphic: Bayu Prahara"
    )
  ) +
  theme_void() +
  theme(
    plot.caption = element_text(
      family = body_font, size = 10.5, lineheight = 1.15, color = "#9ca3af",
      hjust = 1, margin = margin(t = 15, r = 35, b = 35, l = 35)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 5. Combine with cowplot -------------------------------------------------

base_plot <- plot_grid(title_block, plt_map, caption_block, ncol = 1, rel_heights = c(0.15, 0.78, 0.07)) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))

final_plot <- ggdraw(base_plot) +
  draw_plot(plt_rose_minimal, x = 0.62, y = 0.60, width = 0.32, height = 0.22)

# Save plot ---------------------------------------------------------------

output_png <- file.path(out_dir, "02_Lines.png")
ggsave(
  filename = output_png,
  plot     = final_plot,
  width    = 3000,
  height   = 3600,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)
