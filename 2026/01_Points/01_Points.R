# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(scales)
library(sf)
library(geodata)
library(cowplot)

# Load data ---------------------------------------------------------------
df <- read.csv("olive_outlets.csv")

# Load fonts --------------------------------------------------------------

font_add_google("Caprasimo")
font_add_google("Open Sans")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

bg_col <- "#ffe6e6"
text_col <- "#181D25"
body_font <- "Open Sans"
title_font <- "Caprasimo"

# Load map ---------------------------------------------------------------
# 1. Olive points --------------------------------------------------------

df_sf <- st_as_sf(
  df,
  coords = c("longitude", "latitude"),  # x = lon, y = lat
  crs = 4326
)

# 2. DIY Province ---------------------------------------------------------

indo <- gadm(country = "IDN", level = 2, path = tempdir()) |> st_as_sf()
diy <- indo |> 
  filter(NAME_1 == "Yogyakarta") |> 
  st_transform(32749)

jawa <- indo |> filter(str_detect(NAME_1,"Jawa Tengah|Yogyakarta")) |> 
  st_crop(c(xmin = 108.5, ymin = -8.5, xmax = 111.8, ymax = -6.35)) |> 
  st_transform(32749)

df_sf_proj <- st_transform(df_sf, 32749)

# 3. Grid from the province boundary after shifting

grid_jawa <- st_make_grid(jawa, cellsize = 5000, square = TRUE)
grid_jawa <- st_sf(grid_id = seq_along(grid_jawa), geometry = grid_jawa)

# Data wrangling ----------------------------------------------------------
# 1. Java Island ----------------------------------------------------------
df_grid_jawa <- st_join(df_sf_proj, grid_jawa, join = st_within)

grid_summary_jawa <- df_grid_jawa |>
  st_drop_geometry() |>
  filter(!is.na(grid_id)) |>
  group_by(grid_id) |>
  summarise(store_count = n(), .groups = "drop") |>
  mutate(category = if_else(store_count <= 1, "One store", "More than one"))

grid_points_jawa <- grid_jawa |> inner_join(grid_summary_jawa, by = "grid_id") |> st_centroid()

# 2. DIY Province ---------------------------------------------------------
df_sf_diy <- st_filter(df_sf_proj, diy)

# Graph data --------------------------------------------------------------
# 1. Title, Subtitle, and Subheadings Block -------------------------------

title_block <- ggplot() +
  labs(
    title    = "Where Can You Find Your Nearest \nOlive Fried Chicken?",
    subtitle = "Olive Fried Chicken is a source of pride for the people of Yogyakarta, and many say it is as iconic as gudeg. Today, there are nearly 120 outlets across Yogyakarta and several regencies in Central Java."
  ) +
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      family = title_font, face = "bold", size = 36, color = "#ff0000",
      padding = margin(0, 0, 0, 0), margin = margin(t = 30, b = 10, l = 30, r = 30)
    ),
    plot.subtitle = element_textbox_simple(
      family = body_font, size = 18, lineheight = 1.3, color = "grey30",
      padding = margin(0, 0, 0, 0), margin = margin(b = 10, l = 30, r = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )
subheading_diy <- ggplot() +
    labs(
      subtitle = "D.I. Yogyakarta (Exact Store Locations · 1 Dot = 1 Outlet)") +
      theme_void() +
      theme(
    plot.subtitle = element_textbox_simple(
      family = body_font, face = "bold", size = 18, lineheight = 1.3, color = "#7c2d12",
      padding = margin(0, 0, 0, 0), margin = margin(t = 10, b = 10, l = 30, r = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)  
      )

subheading_jawa <- ggplot() +
    labs(
      subtitle = "Central Java & D.I. Yogyakarta (5-km binned-area)") +
      theme_void() +
      theme(
    plot.subtitle = element_textbox_simple(
      family = body_font, face = "bold", size = 18, lineheight = 1.3, color = "#7c2d12",
      padding = margin(0, 0, 0, 0), margin = margin(b = 10, l = 30, r = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)  
      )

# 2. DIY Province Map (Exact Points) --------------------------------------

plt_diy <- ggplot() +
  geom_sf(data = diy, fill = "#fff3ddff", color = "#7c2d12", linewidth = 0.5) +
  geom_sf(data = df_sf_diy, color = "#7c2d12", size = 4, alpha = 0.8) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.margin = margin(5, 20, 5, 20),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 3. Java Island Map (5-km Grid Points + Legend) -------------------------

plt_jawa <- ggplot() +
  geom_sf(data = jawa, fill = "#fff3ddff", color = "#7c2d12", linewidth = 0.3) +
  geom_sf(data = grid_points_jawa, aes(color = category, size = category), stroke = 0) +
  scale_color_manual(values = c(
    "One store"     = alpha("#d97706"),
    "More than one" = alpha("#7f1d1d")
  )) +
  scale_size_manual(values = c(
  "One store"     = 2.5,       
  "More than one" = 4        
  )) +
  coord_sf(expand = FALSE) +
  labs(
    color = "Store Density :"
  ) +
  guides(
    color = guide_legend(title = "Store Density :", override.aes = list(size = c(4, 2.5))), size  = "none"
  )+
  theme_void(base_family = body_font) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 14, face = "bold", color = text_col),
    legend.text = element_text(size = 14, color = text_col),
    plot.margin = margin(5, 20, 5, 20),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 4. Caption Block --------------------------------------------------------

caption_block <- ggplot() +
  labs(caption = "#30DayMapChallenge 2026 · Day 1: Points\nSource: carikulinerindonesia.com · Graphic: Bayu Prahara") +
  theme_void() +
  theme(
    plot.caption = element_text(family = body_font, size = 14, color = "grey40", hjust = 1, margin = margin(b = 30, r = 30)),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 5. Combine Vertically with cowplot -------------------------------------

final_plot <- plot_grid(
  title_block,
  subheading_diy,
  plt_diy,
  subheading_jawa,
  plt_jawa,
  caption_block,
  ncol = 1,
    rel_heights = c(0.15, 0.03, 0.42, 0.03, 0.38, 0.04)
) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))
