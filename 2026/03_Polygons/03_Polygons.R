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

col_under15  <- "#ffea00"
col_15to30   <- "#f97316"
col_30to45   <- "#e11d48"
col_45to60   <- "#8b5cf6"
col_over60   <- "#222c40"

col_hosp     <- "#ffffff"

iso_levels <- c("< 15 minutes", "15 – 30 minutes", "30 – 45 minutes", "45 – 60 minutes", "> 60 minutes")
iso_colors <- c(
  "< 15 minutes"     = col_under15,
  "15 – 30 minutes"  = col_15to30,
  "30 – 45 minutes"  = col_30to45,
  "45 – 60 minutes"  = col_45to60,
  "> 60 minutes"     = col_over60
)

data_dir     <- if (dir.exists("03_Polygons/data")) "03_Polygons/data" else if (dir.exists("data")) "data" else "."
out_dir      <- if (dir.exists("03_Polygons")) "03_Polygons" else "."
ref_data_dir <- if (dir.exists("02_Lines/data")) "02_Lines/data" else if (dir.exists("../02_Lines/data")) "../02_Lines/data" else "data"

# Load data and map -------------------------------------------------------
# 1. DIY Boundary & DEM Hillshade -----------------------------------------

diy_gadm  <- readRDS(file.path(ref_data_dir, "gadm/gadm41_IDN_2_pk.rds"))
diy       <- diy_gadm |> unwrap() |> st_as_sf() |> dplyr::filter(NAME_1 == "Yogyakarta") |> st_transform(32749)
diy_union <- st_union(diy)
bbox_diy  <- st_bbox(diy)

hill_file <- file.path(data_dir, "hillshade_diy.tif")
if (!file.exists(hill_file)) {
  dem_raw    <- rast(file.path(ref_data_dir, "dem/elevation/srtm_59_14.tif"))
  ext_target <- ext(bbox_diy["xmin"] - 2000, bbox_diy["xmax"] + 2000,
                    bbox_diy["ymin"] - 5000, bbox_diy["ymax"] + 5000)
  dem_utm    <- project(dem_raw, rast(ext_target, res = 150, crs = "EPSG:32749"), method = "bilinear")
  slope_rad  <- terrain(dem_utm, "slope", unit = "radians")
  aspect_rad <- terrain(dem_utm, "aspect", unit = "radians")
  hill       <- shade(slope_rad, aspect_rad, angle = 30, direction = 315)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  writeRaster(hill, hill_file, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))
} else {
  hill <- rast(hill_file)
}

hill_df           <- as.data.frame(hill, xy = TRUE)
colnames(hill_df) <- c("x", "y", "shade")
s_min             <- min(hill_df$shade, na.rm = TRUE)
s_max             <- max(hill_df$shade, na.rm = TRUE)
hill_df$alpha_val <- 0.05 + 0.50 * (hill_df$shade - s_min) / (s_max - s_min)

# 3. Hospitals & Road Network ---------------------------------------------

hosp_path     <- if (file.exists(file.path(out_dir, "rumah_sakit_diy.geojson"))) file.path(out_dir, "rumah_sakit_diy.geojson") else "rumah_sakit_diy.geojson"
hospitals_raw <- st_read(hosp_path, quiet = TRUE) |> st_transform(32749)
hospitals     <- st_intersection(hospitals_raw, diy_union) |>
  mutate(
    hosp_class = case_when(
      kelas == "A" ~ "Class A",
      kelas == "B" ~ "Class B",
      kelas == "C" ~ "Class C",
      kelas == "D" ~ "Class D",
      TRUE         ~ "Class D"
    ),
    hosp_class = factor(hosp_class, levels = c("Class A", "Class B", "Class C", "Class D"))
  )

iso_cache <- file.path(data_dir, "isochrones_diy.rds")
if (!file.exists(iso_cache)) {
  roads_raw   <- readRDS(file.path(ref_data_dir, "diy_roads_raw.rds")) |> st_transform(32749)
  r_template  <- rast(ext(st_bbox(diy)), res = 150, crs = "EPSG:32749")
  
  roads_speed <- roads_raw |>
    mutate(speed_kmh = case_when(
      highway == "trunk"     ~ 55,
      highway == "primary"   ~ 45,
      highway == "secondary" ~ 35,
      highway == "tertiary"  ~ 25,
      TRUE                   ~ 20
    )) |>
    mutate(friction = 60 / (speed_kmh * 1000))
  
  r_friction <- rasterize(vect(roads_speed), r_template, field = "friction", fun = "min", background = 60 / (8 * 1000))
  r_hosp     <- rasterize(vect(hospitals), r_template, field = 1)
  r_friction[r_hosp == 1] <- 0
  
  acc_time     <- costDist(r_friction, target = 0)
  acc_time_diy <- mask(acc_time, vect(diy_union))
  
  rcl <- matrix(c(
    0, 15, 1,
    15, 30, 2,
    30, 45, 3,
    45, 60, 4,
    60, 9999, 5
  ), ncol = 3, byrow = TRUE)
  
  r_classes <- classify(acc_time_diy, rcl, include.lowest = TRUE)
  poly_iso  <- as.polygons(r_classes) |> st_as_sf()
  colnames(poly_iso)[1] <- "class_id"
  
  poly_iso <- poly_iso |>
    mutate(
      class_id  = as.integer(class_id),
      time_band = factor(
        class_id,
        levels = 1:5,
        labels = iso_levels
      ),
      area_km2  = as.numeric(st_area(geometry)) / 1e6,
      pct_area  = area_km2 / sum(area_km2) * 100
    )
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(poly_iso, iso_cache)
} else {
  poly_iso <- readRDS(iso_cache)
}

# Graph data --------------------------------------------------------------
# 1. Title Block ----------------------------------------------------------

title_block <- ggplot() +
  labs(
    title    = "95% of Yogyakarta Is Within 45 Minutes <br>of a Hospital",
    subtitle = "Yogyakarta has 84 hospitals, and most of the province is a short drive from one. Almost half of the land is within 15 minutes. Colors show how long it takes to drive to the nearest hospital."
  ) +
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      family = title_font, face = "bold", size = 32, color = "#ffffff",
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
  geom_raster(data = hill_df, aes(x = x, y = y, alpha = alpha_val), fill = "#94a3b8") +
  scale_alpha_identity() +
  
  geom_sf(data = poly_iso, aes(fill = time_band), color = alpha("#ffffff", 0.15), linewidth = 0.25, alpha = 0.70) +
  geom_sf(data = diy, fill = NA, color = border_col, linewidth = 0.65) +
  geom_sf(data = hospitals, aes(shape = hosp_class), color = col_hosp, size = 1.8, stroke = 0.45, alpha = 0.95) +
  
  scale_fill_manual(
    name = "Drive-Time to Nearest Hospital (84 Facilities):",
    values = iso_colors
  ) +
  scale_shape_manual(
    name = "Hospital Classification:",
    values = c(
      "Class A" = 18,
      "Class B" = 15,
      "Class C" = 17,
      "Class D" = 16
    )
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(alpha = 0.95, color = NA),
      order = 1
    ),
    shape = guide_legend(
      nrow = 1,
      title.position = "top",
      title.hjust = 0.5,
      keywidth = unit(0.8, "lines"),
      override.aes = list(size = c(3.2, 2.5, 2.5, 2.5), color = "#ffffff"),
      order = 2
    )
  ) +
  coord_sf(xlim = c(bbox_diy["xmin"], bbox_diy["xmax"]), ylim = c(bbox_diy["ymin"] - 4500, bbox_diy["ymax"] + 5500), expand = FALSE) +
  theme_void() +
  theme(
    legend.position   = "bottom",
    legend.box        = "vertical",
    legend.direction  = "horizontal",
    legend.box.just   = "center",
    legend.spacing.y  = unit(0.4, "lines"),
    legend.title      = element_text(family = body_font, size = 11, face = "bold", color = "#ffffff", margin = margin(b = 3)),
    legend.text       = element_text(family = body_font, size = 10.5, color = "#9ca3af"),
    legend.key.width  = unit(2.0, "lines"),
    legend.key.height = unit(0.8, "lines"),
    legend.margin     = margin(t = 2, b = 6),
    plot.margin       = margin(t = 10, r = 35, b = 10, l = 35),
    plot.background   = element_rect(fill = bg_col, color = NA)
  )

# 3. Territorial Coverage Inset -------------------------------------------

df_bars <- poly_iso |>
  st_drop_geometry() |>
  filter(!is.na(time_band)) |>
  arrange(desc(class_id))

plt_inset <- ggplot(df_bars, aes(x = pct_area, y = time_band, fill = time_band)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_area)),
    hjust = -0.15,
    family = body_font,
    size = 2.8,
    fontface = "bold",
    color = "#e2e8f0"
  ) +
  scale_fill_manual(values = iso_colors) +
  scale_x_continuous(limits = c(0, 58), expand = c(0, 0)) +
  labs(title = "TERRITORIAL COVERAGE", subtitle = "Land area share by drive-time tier") +
  theme_void() +
  theme(
    plot.title       = element_text(family = title_font, size = 11, face = "bold", color = "#ffffff", hjust = 0),
    plot.subtitle    = element_text(family = body_font, size = 8.5, color = "#9ca3af", hjust = 0, margin = margin(t = 2, b = 6)),
    axis.text.y      = element_text(family = body_font, size = 8, color = "#cbd5e1", hjust = 1, margin = margin(r = 6)),
    plot.margin      = margin(10, 14, 10, 10)
  )

# 4. Caption Block --------------------------------------------------------

caption_block <- ggplot() +
  labs(
    caption = paste0(
      "Note: Multi-tier drive-time isochrones modeled via road network hierarchy & friction raster across 84 hospitals in DIY.\n",
      "#30DayMapChallenge 2026 \u00b7 Day 3: Polygons \u00b7 Data: OpenStreetMap, GADM, DIY Health Dept & CGIAR SRTM \u00b7 Graphic: Bayu Prahara"
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

base_plot <- plot_grid(title_block, plt_map, caption_block, ncol = 1, rel_heights = c(0.16, 0.9, 0.05)) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))

final_plot <- ggdraw(base_plot) +
  draw_plot(plt_inset, x = 0.6, y = 0.6, width = 0.34, height = 0.20)

# Save plot ---------------------------------------------------------------

output_png <- file.path(out_dir, "03_Polygons.png")
ggsave(
  filename = output_png,
  plot     = final_plot,
  width    = 3000,
  height   = 3600,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)
