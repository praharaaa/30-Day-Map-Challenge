# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(sf)
library(cowplot)
library(ragg)
library(ggtext)

# Load fonts --------------------------------------------------------------

font_add_google("Rubik")
font_add_google("Roboto Mono")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts ------------------------------------------------

title_font   <- "Rubik"
body_font    <- "Roboto Mono"

bg_col      <- "#fafaf9"  
col_other   <- "#e7e5e4"  
border_col  <- "#ffffff"
col_text    <- "#202020"  

col_harjo   <- "#F1535A"  
col_rejo    <- "#28828C"  

# Load data and map -------------------------------------------------------

df <- read.csv("harjo_vs_rejo_villages.csv")

map_diy <- st_read("Provinsi Daerah Istimewa Yogyakarta-KEL_DESA.geojson")

map_clusters <- map_diy |> left_join(df, by = "kode_kd")

# Graph data --------------------------------------------------------------
# 1. Title, Subtitle, and Subheadings Block -------------------------------

title_block <- ggplot() +
  labs(
    title    = "Divided by Suffix: -harjo vs -rejo Villages Across Yogyakarta",
    subtitle = paste0(
      "In the Special Region of Yogyakarta, 83 villages share traditional Javanese naming suffixes associated with prosperity and welfare. ",
      "Villages ending in <span style='color:", col_harjo, "'><b>-harjo</b></span> (41 villages) and ",
      "<span style='color:", col_rejo, "'><b>-rejo</b></span> (42 villages) appear evenly distributed across every regency, ",
      "showing no clear geographic clustering or regional preference."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_textbox_simple(
      family  = title_font, face = "bold", size = 26, color = col_text,
      padding = margin(0, 0, 0, 0),
      margin  = margin(t = 30, b = 10, l = 30, r = 30)
    ),
    plot.subtitle = element_textbox_simple(
      family     = body_font, size = 11.5, lineheight = 1.4, color = "#57534e",
      padding    = margin(0, 0, 0, 0),
      margin     = margin(b = 10, l = 30, r = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 2. Map Plot -------------------------------------------------------------

map_plot <- ggplot(data = map_clusters) +
  geom_sf(
    aes(fill = end_name),
    color     = border_col,
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values   = c("harjo" = col_harjo, "rejo" = col_rejo),
    na.value = col_other,
    guide    = "none"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin     = margin(10, 30, 10, 30)
  )

# 3. Caption Block --------------------------------------------------------

caption_block <- ggplot() +
  labs(
    caption = "#30DayMapChallenge 2026 · Day 4: Clusters\nData: edwardsamuel & Alf-Anas (GitHub) · Graphic: Bayu Prahara") +
  theme_void() +
  theme(
    plot.caption = element_text(
      family = body_font, size = 9, color = "#78716c",
      hjust  = 1,
      margin = margin(t = 5, b = 30, r = 30, l = 30)
    ),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# 4. Combine with cowplot -------------------------------------------------

final_plot <- plot_grid(
  title_block,
  map_plot,
  caption_block,
  ncol        = 1,
  rel_heights = c(0.2, 0.6, 0.06)
) +
  theme(plot.background = element_rect(fill = bg_col, color = NA))

# 5. Save Plot ------------------------------------------------------------

out_dir    <- if (dir.exists("04_Clusters")) "04_Clusters" else "."
output_png <- file.path(out_dir, "04_clusters.png")

ggsave(
  filename = output_png,
  plot     = final_plot,
  width    = 2400,
  height   = 3000,
  units    = "px",
  dpi      = 300,
  device   = agg_png
)

