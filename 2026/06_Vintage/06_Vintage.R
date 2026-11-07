# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(sf)
library(cowplot)
library(ragg)
library(ggtext)

# Load fonts --------------------------------------------------------------

font_add_google("Brygada 1918", "Brygada 1918")
font_add_google("Barlow Condensed", "Barlow Condensed")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

title_font <- "Brygada 1918"
area_font  <- "Brygada 1918"
poi_font   <- "Barlow Condensed"
body_font  <- "Barlow Condensed"
mono_font  <- "Roboto Mono"

bg_col            <- "#E6DDC5"  # Warm colonial paper
col_kampung       <- "#CCD0A1"  # Settlement mass envelope
col_buildings     <- "#976451"  # Individual brick texture
col_water         <- "#C6D0CC"  # Pale blue-grey water
col_water_edge    <- "#858F94"  # Waterway edge
col_railway       <- "#4A5055"  # Dark railway line
col_road_major_bg <- "#CD927C"  # Pink / coral base for major roads
col_road_major_fg <- "#De6D60"  # Light crimson dash for candy-cane pattern
col_road_minor    <- "#D1A394"  # Plain pink/coral for minor streets
col_text          <- "#2E2724"  # Dark sepia ink
col_sub           <- "#5C524C"  # Medium sepia
col_caption       <- "#7A706A"  # Muted caption text
col_poi           <- "#2E2724"  # Dark sepia for historic landmarks
border_frame      <- "#6E655F"  # Neatline frame

# Configuration & Data Directories ----------------------------------------

data_dir <- if (dir.exists("OSM_data")) "OSM_data" else "../OSM_data"
out_dir  <- if (dir.exists("06_Vintage")) "06_Vintage" else "."

# Load cached spatial layers ----------------------------------------------

extent_info <- readRDS(file.path(data_dir, "kridosono_extent.rds"))
bbox_utm    <- extent_info$bbox_utm
center_pt   <- extent_info$center_coords

roads_sf    <- readRDS(file.path(data_dir, "kridosono_roads.rds"))
railway_sf  <- readRDS(file.path(data_dir, "kridosono_railway.rds"))
water_sf    <- readRDS(file.path(data_dir, "kridosono_water.rds"))
settle_sf   <- readRDS(file.path(data_dir, "kridosono_settlement_blocks.rds"))
kelurahan_sf<- readRDS(file.path(data_dir, "kridosono_kelurahan_labels.rds"))
poi_sf      <- readRDS(file.path(data_dir, "kridosono_poi_labels.rds"))

# Prepare road layers -----------------------------------------------------

roads_primary <- roads_sf |> filter(road_class == "primary")
roads_minor   <- roads_sf |> filter(road_class == "minor")

# Prepare curated kelurahan labels (avoid cluttered overlap) --------------

kelurahan_labels <- kelurahan_sf |>
  st_drop_geometry() |>
  filter(
    clean_name %in% c(
      "Kotabaru", "Terban", "Bausasran", "Baciro", "Demangan",
      "Klitren", "Gowongan", "Cokrodiningratan", "Karangwaru",
      "Tegalrejo", "Bumijo", "Pringgokusuman", "Sosromenduran",
      "Suryatmajan", "Ngupasan", "Prawirodirjan", "Purwokinanti",
      "Gunungketur", "Wirogunan", "Tahunan", "Semaki", "Muja Muju",
      "Keparakan", "Panembahan", "Kadipaten", "Patehan", "Notoprajan",
      "Ngampilan", "Wirobrajan", "Pakuncen", "Bener", "Warungboto"
    )
  ) |>
  mutate(
    # Nudge coordinates to avoid overlapping roads, landmarks, and other labels
    x = case_when(
      clean_name == "Cokrodiningratan" ~ x - 130,
      clean_name == "Demangan"         ~ x + 120,
      clean_name == "Ngupasan"         ~ x - 160,
      clean_name == "Kotabaru"         ~ x - 140,
      clean_name == "Panembahan"       ~ x + 80,
      clean_name == "Wirobrajan"       ~ x + 160,
      clean_name == "Patehan"          ~ x - 140,
      clean_name == "Purwokinanti"     ~ x - 60,
      clean_name == "Gunungketur"      ~ x - 80,
      TRUE ~ x
    ),
    y = case_when(
      clean_name == "Kotabaru"         ~ y + 140,
      clean_name == "Ngupasan"         ~ y - 70,
      clean_name == "Demangan"         ~ y - 80,
      clean_name == "Muja Muju"        ~ y - 90,
      clean_name == "Terban"           ~ y - 60,
      clean_name == "Pringgokusuman"   ~ y - 80,
      clean_name == "Patehan"          ~ y - 30,
      clean_name == "Purwokinanti"     ~ y + 70,
      clean_name == "Gunungketur"      ~ y + 15,
      clean_name == "Notoprajan"       ~ y + 120,
      TRUE ~ y
    )
  )

# Curate POI labels coordinates & placement -------------------------------

poi_labels <- poi_sf |>
  filter(name != "Sungai Code") |>
  mutate(
    name = if_else(grepl("Keraton", name), "Keraton Ngayogyakarta\nHadiningrat", name),
    text_x = case_when(
      grepl("Tugu\\)", name)     ~ x - 40,
      grepl("Panti Rapih", name) ~ x + 65,
      grepl("Gadjah Mada", name) ~ x + 30,
      grepl("Negeri Yogyakarta", name) ~ x - 20,
      TRUE                       ~ x
    ),
    text_y = case_when(
      grepl("Lempuyangan", name) ~ y - 75,
      grepl("Balai Yasa", name)  ~ y + 120,
      grepl("Tugu", name)        ~ y + 75,
      grepl("Kridosono", name)   ~ y - 80,
      grepl("Keraton", name)     ~ y - 85,
      grepl("Alun-alun", name)   ~ y + 75,
      grepl("Panti Rapih", name) ~ y + 10,
      TRUE                       ~ y + 75
    ),
    v_just = case_when(
      grepl("Lempuyangan|Kridosono|Keraton", name) ~ 1.2,
      grepl("Panti Rapih", name) ~ 0.5,
      TRUE                       ~ -0.2
    ),
    h_just = case_when(
      grepl("Panti Rapih", name) ~ -0.1,
      TRUE                       ~ 0.5
    )
  )

# River text annotations --------------------------------------------------

river_annotations <- data.frame(
  name = c("~ Kali Code ~", "~ Kali Winongo ~", "~ Kali Belik ~"),
  x = c(430600, 428760, 431780),
  y = c(9140790, 9140700, 9140050),
  angle = c(75, 78, 72)
)

# Kridosono Stadium geometry & marker -------------------------------------

kridosono_center <- data.frame(
  name = "Sportpark Kridosono\n(1920s)",
  x = center_pt[1],
  y = center_pt[2]
)

# Graph data --------------------------------------------------------------
# 1. Title Block (Storytelling narrative) ----------------------------------

title_block <- ggplot() +
  labs(
    title    = "Yogyakarta in 1920s Cartographic Garb",
    subtitle = paste0(
      "Centered around the green oval of Sportpark Kridosono and the leafy streets of Kotabaru, ",
      "central Yogyakarta grew in the 1920s as a colonial garden suburb and an important railway hub. ",
      "Drawn in the visual style of the Dutch East Indies Topografische Dienst (1923–1924)."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      family  = title_font, face = "bold", size = 25, color = col_text,
      lineheight = 1.2,
      padding = margin(0, 0, 0, 0),
      margin  = margin(t = 45, b = 10, l = 45, r = 45)
    ),
    plot.subtitle = element_textbox_simple(
      family     = body_font, size = 12.5, lineheight = 1.35, color = col_sub,
      padding    = margin(0, 0, 0, 0),
      margin     = margin(b = 20, l = 45, r = 45)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 2. Main Map -------------------------------------------------------------

map_plot <- ggplot() +
  # Paper background canvas
  geom_rect(
    aes(xmin = bbox_utm["xmin"], xmax = bbox_utm["xmax"],
        ymin = bbox_utm["ymin"], ymax = bbox_utm["ymax"]),
    fill = bg_col, color = NA
  ) +
  
  # Settlement masses (Kampung envelopes)
  geom_sf(
    data = settle_sf,
    fill = col_kampung,
    color = NA
  ) +
  
  # Waterways (Kali Code, Winongo, Gajahwong)
  geom_sf(
    data = water_sf,
    color = col_water,
    linewidth = 2.4,
    lineend = "round"
  ) +
  geom_sf(
    data = water_sf,
    color = col_water_edge,
    linewidth = 0.4
  ) +
  
  # Minor roads (single thin pink/coral line)
  geom_sf(
    data = roads_minor,
    color = col_road_minor,
    linewidth = 0.38
  ) +
  
  # Major roads: Layer 1 (Pink/coral base ribbon)
  geom_sf(
    data = roads_primary,
    color = col_road_major_bg,
    linewidth = 0.85,
    lineend = "butt"
  ) +
  
  # Major roads: Layer 2 (Candy-cane alternating dark crimson dashed segments)
  geom_sf(
    data = roads_primary,
    color = col_road_major_fg,
    linewidth = 0.85,
    linetype = "11",
    lineend = "butt"
  ) +
  
  # Railway corridors (Dark iron lines)
  geom_sf(
    data = railway_sf,
    color = col_railway,
    linewidth = 0.75
  ) +
  geom_sf(
    data = railway_sf,
    color = bg_col,
    linewidth = 0.25,
    linetype = "33"
  ) +
  
  # Administrative labels: Kelurahan / Desa only (Brygada 1918 Italic)
  geom_text(
    data = kelurahan_labels,
    aes(x = x, y = y, label = clean_name),
    family = area_font,
    fontface = "italic",
    size = 3.2,
    color = col_text,
    alpha = 0.88
  ) +
  
  # Important Landmarks & Historical POIs (Barlow Condensed Regular)
  geom_point(
    data = poi_labels,
    aes(x = x, y = y),
    shape = 18, size = 2.4, color = col_poi
  ) +
  geom_text(
    data = poi_labels,
    aes(x = text_x, y = text_y, label = name, vjust = v_just, hjust = h_just),
    family = poi_font,
    fontface = "plain",
    size = 2.5,
    lineheight = 0.9,
    color = col_poi
  ) +
  
  # Waterway text annotations (Kali Code, Winongo, Gajahwong, Belik)
  geom_text(
    data = river_annotations,
    aes(x = x, y = y, label = name, angle = angle),
    family = area_font,
    fontface = "italic",
    size = 2.9,
    color = "#4E6773"
  ) +
  
  # Center alignment & extent boundary
  coord_sf(
    xlim = c(bbox_utm["xmin"], bbox_utm["xmax"]),
    ylim = c(bbox_utm["ymin"], bbox_utm["ymax"]),
    expand = FALSE
  ) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    panel.border = element_rect(color = border_frame, fill = NA, linewidth = 1.2),
    plot.margin = margin(5, 45, 10, 45)
  )

# 3. Caption Block --------------------------------------------------------

caption_block <- ggplot() +
  labs(
    caption = paste0(
      "#30DayMapChallenge 2026 \u00b7 Day 6: Vintage\n",
      "Data: OpenStreetMap \u00b7 Graphic: Bayu Prahara"
    )
  ) +
  theme_void() +
  theme(
    plot.caption = element_text(
      family     = mono_font, size = 8.5, color = col_caption,
      lineheight = 1.35,
      hjust      = 1,
      margin     = margin(t = 10, b = 45, r = 45, l = 45)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 4. Combine with cowplot -------------------------------------------------

final_plot <- plot_grid(
  title_block,
  map_plot,
  caption_block,
  ncol        = 1,
  rel_heights = c(0.20, 0.72, 0.08)
) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))

# 5. Save Plot ------------------------------------------------------------

output_png <- file.path(out_dir, "06_Vintage.png")

ggsave(
  filename = output_png,
  plot     = final_plot,
  width    = 2400,
  height   = 3000,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)