rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("something-something.R")

# ---- load-packages -----------------------------------------------------------
library(ggplot2)  # For graphing
# library(lme4)     # For mlms
# import::from("magrittr", "%>%")
requireNamespace("dplyr")
requireNamespace("TabularManifest") # remotes::install_github("Melinae/TabularManifest")
# requireNamespace("RColorBrewer")
# requireNamespace("scales") #For formating values in graphs
# requireNamespace("mgcv) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- declare-globals ---------------------------------------------------------
options(show.signif.stars = FALSE) #Turn off the annotations on p-values
config                      <- config::get()

# OuhscMunge::readr_spec_aligned(config$path_candle_raw)
col_types <- readr::cols_only(
  `color`       = readr::col_character(),   # candle color
  `length`      = readr::col_integer(),     # wick length
  `duration`    = readr::col_integer()      # burn duration
)

palette_dark <-
  c(
    "blue" = "blue",
    "pink" = "pink",
    "white" = "white",
    "yellow" = "yellow"
  )
shapes <-
  c(
    "blue"    = 25,
    "pink"    = 22,
    "white"   = 24,
    "yellow"  = 23
  )
# ---- load-data ---------------------------------------------------------------
ds      <- readr::read_csv(config$path_candle_raw, col_types = col_types)

rm(col_types)
# ---- tweak-data --------------------------------------------------------------
ds <-
  ds |>
  dplyr::mutate(
    color     = factor(color),
    # duration  = sample(100:200, size = nrow(ds), replace = TRUE),
  )
# ---- graphs ---------------------------------------------------------------
TabularManifest::histogram_continuous(ds, variable_name="duration")

ggplot(ds, aes(x=length, y=duration, group=1, fill=color, shape=color)) + #, ymin=0)) +
  geom_point(size = 3, alpha = .5) +
  geom_smooth(aes(group = 1), method = "lm", color = "gray20", linetype = "88", show.legend = FALSE, se = T) +
  # geom_smooth(aes(group = 1), method = "loess", color = "gray20", linetype = "22", show.legend = FALSE, se = F) +
  # geom_point(position=position_jitter(height=.05, width=5), size=4, na.rm = TRUE) +
  # geom_text(aes(label=county_month_id)) +
  # geom_line(position=position_jitter(height=.1, width=5)) +
  scale_fill_manual(values=palette_dark) +
  scale_shape_manual(values = shapes) +
  coord_cartesian(xlim = c(0, 20)) +
  theme_light() +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
  # guides(fill = guide_legend(ncol=4)) + #, override.aes = list( alpha = 1))) +
  labs(
    title = "Relationship between Wick Length and Burn Duration",
    x     = "Wick Length (mm)",
    y     = "Burn Duration (sec)",
    fill  = "Candle Color",
    shape  = "Candle Color"
  )
ggsave("candle/scatter.png", dpi = 300)

# Basic dot plot
ggplot(ds, aes(x=duration, fill= color)) +
  geom_dotplot(stackratio=1, dotsize=1.2, alpha = .5) + #, stackdir='center'
  # scale_fill_manual(values=palette_dark) +
  # coord_flip() +
  scale_fill_manual(values=palette_dark) +
  theme_light() +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
  # guides(fill = guide_legend(ncol=4)) + #, override.aes = list( alpha = 1))) +
  labs(
    title = "Relationship between Wick and Burn Duration",
    x     = "Burn Duration (sec)",
    y     = "Candle Count"
    # fill  = "Candle Color"
  )
