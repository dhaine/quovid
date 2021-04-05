# An optional custom script to run before Hugo builds your site.
# You can delete it if you do not need it.

## Load packages
packages <- c("tidyverse", "lubridate", "ggplot2", "ggtext", "ggforce",
              "scales", "patchwork", "gganimate", "openxlsx", "zoo",
              "paletteer", "stringr", "httr", "jsonlite", "glue", "ragg")
zzz <- lapply(packages, function(xxx) suppressMessages(require(xxx,
                                                               character.only = TRUE,
                                                               quietly = TRUE,
                                                               warn.conflicts = FALSE)))

source("R/load.R")

source("R/cases.R")

source("R/hospi.R")
