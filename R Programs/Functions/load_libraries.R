# Set libraries --------------------------------------------#


library <- function (...) {
  packages <- as.character(match.call(expand.dots = FALSE)[[2]])
  suppressWarnings(suppressMessages(lapply(packages, base::library, character.only = TRUE)))
  return(invisible())
}

library(tidyverse, dplyr, ggplot2, devtools, httr, jsonlite, grid, readxl, writexl, openxlsx, zoo, gridExtra, 
        cowplot, ggforce, date, gridExtra, scales, ggtext, timeDate, kableExtra, lubridate, patchwork, 
        purrr, staplr)

# library(tidyverse)
# library(dplyr)
# library(ggplot2)
# library(devtools)
# 
# library(httr)
# library(jsonlite)
# library(grid)
# 
# library(readxl)
# library(writexl)
# library(openxlsx)ti
# library(zoo)
# library(gridExtra)
# library(cowplot)
# library(ggforce)
# library(date)
# library(gridExtra)
# library(scales)
# library(ggtext)
# library(timeDate)
# 
# library(kableExtra)
# 
# library(lubridate)
# 
# library(patchwork)
# 
# library(purrr)
# library(staplr)
