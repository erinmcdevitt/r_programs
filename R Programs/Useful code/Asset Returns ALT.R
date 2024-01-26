

#############################################################################
# Set Up                                                                    #
#############################################################################

rm(list = ls())
#comment out if using main

source(paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Research/R/install_packages.R', sep = ''))
source(paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Research/R/load_libraries.R', sep = ''))
source(paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Research/R/api_connections.R', sep = ''))
source(paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Research/R/bloomberg_api.R', sep = ''))
source(paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Quarterly Letters/Set Up/Visuals/rcg_quarterly_style.R', sep = ""))

current_quarter <- "Q4 2023"

quarterly_folder <- paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Quarterly Letters/', sep = "")

code_folder <- paste0(quarterly_folder, current_quarter, "/Visuals/code", sep = "")
data_folder <- paste0(quarterly_folder, current_quarter,"/Visuals/data", sep = "")
output_folder <- paste0(quarterly_folder, current_quarter,"/Visuals/output", sep = "")


visuals_folder <- paste0(quarterly_folder, current_quarter,"/Final Version and Design/Visuals", sep = "")



#############################################################################
# Asset Returns Table                                                       #
#############################################################################


returns <-  read_excel(paste(data_folder, "/data.xlsx", sep = ""), skip = 1, sheet = "returns") 

returns1 <- returns %>%
  filter(category != "hf") %>%
  rename(spacer1 = `...4`, 
         spacer2 = `...5`) %>%
  mutate(`Q4 2023` = round(`Q4 2023`, digits = 1), 
         `2023` = round(`2023`, digits = 1)) %>%
  mutate(`Q4 2023` = case_when(category != "bond" ~ paste(`Q4 2023`, "%", sep = ""), 
                               TRUE ~ as.character(`Q4 2023`)), 
         `2023` = case_when(category != "bond" ~ paste(`2023`, "%", sep = ""), 
                               TRUE ~ as.character(`2023`))) %>%
  mutate(`Q4 2023` = case_when(name %in% c("Equity Markets", "Bond Markets (bps)", "Currency Markets", "Commodity Markets") ~ paste("Q4 2023"),
                               TRUE ~ `Q4 2023`),
         `2023` = case_when(name %in% c("Equity Markets", "Bond Markets (bps)", "Currency Markets", "Commodity Markets") ~ paste("2023"), 
         TRUE ~ `2023`), 
         spacer1 = paste("  "), 
         spacer2 = paste("  ")) %>%
  select(c(name, category, spacer1, spacer2, `Q4 2023`, `2023`)) 


returns <- function(asset) {
  
  df <- returns1 %>% filter(category == asset) %>%
    select(-c(category))
  
  df1 <- as.data.frame(df)
  rownames(df1) <- df1[,1]
  df1 <- df1[,-1]
  colnames(df1) <- c(" ", " ", " ", " ")

  return(df1)
}


equity <- returns("equity")
bond <- returns("bond")
currency <- returns("currency")
commodity <- returns("commodity")


t1 <- ttheme_minimal(
  core=list(
    fg_params=list(fontface=rep("plain"), fontfamily="Roboto", fontsize = 10)),
  rowhead=list(
    padding = unit.c(unit(0, "mm"), unit(1, "mm")),
    fg_params=list(fontface = c(rep("bold", 2), rep("plain", 10)), #fontface=c(rep("bold",2), rep("plain", 7), "bold", rep("plain", 9), "bold", rep("plain", 5), "bold", rep("plain", 5)), 
                   fontfamily="Roboto", x = 0, hjust = 0, fontsize = 10),
    bg_params = list(
      fill=c("white"),
      col=c("white"))),
  colhead=list(
    fg_params=list(fontface="bold",fontfamily="Roboto", fontsize = 10),
    bg_params = list(
      fill="white",
      col="white")
  )
)

table <-  function(asset) {
  table <- tableGrob(asset, theme = t1)
  
  table <- gtable_add_grob(table,
                           grobs = segmentsGrob( # line across the bottom
                             x0 = unit(0,"npc"),
                             y0 = unit(0,"npc"),
                             x1 = unit(1,"npc"),
                             y1 = unit(0, "npc"),
                             gp = gpar(lwd = 1.0)),
                           t = 1, b = 2, l = 1, r = ncol(table))
  
  
  table <- gtable_add_grob(table,
                           grobs = segmentsGrob( # line across the bottom
                             x0 = unit(0,"npc"),
                             y0 = unit(0,"npc"),
                             x1 = unit(1,"npc"),
                             y1 = unit(0, "npc"),
                             gp = gpar(lwd = 1.0)),
                           t = 1, b = nrow(table), l = 1, r=ncol(table))
  
  return(table)
}

equity_table <- table(equity)
bond_table <- table(bond)
currency_table <- table(currency)
commodity_table <- table(commodity)


#Top table title

tt <- textGrob("Asset Returns", hjust = 0.7,
               gp = gpar(fontsize =  12, fontface = "bold", fontfamily = "Roboto"))

#bottom table footnote
footnote <- textGrob("Source: Bloomberg.", hjust = 1.77,
                     gp = gpar(fontsize =  8, fontface = "plain", fontfamily = "Roboto"))

padding <- unit(3, "mm")

equity_table <- gtable_add_rows(
  equity_table, heights = grobHeight(tt) + padding, pos = 0)

equity_table <- gtable_add_grob(equity_table, tt, 
                                t= 1, l = 1)

grid.draw(equity_table)
dev.off()












table <- gtable_add_rows(
  table, heights = grobHeight(footnote)+ padding)


table <- gtable_add_grob(table, list(tt, footnote), 
                         t= c(1, nrow(table)), l=c(1,1), 
                         r = ncol(table))

grid.draw(table)

dev.off() 

title <- "Asset Returns 2023"

ggsave(paste0(output_folder, '/', title, '.png', sep = ''), table, width = 7.5, height = 8.5, unit = 'in')
ggsave(paste0(visuals_folder, '/', title, '.svg', sep = ''), table, width = 7.5, height = 11, unit = 'in')

# 
# table <- gtable_add_grob(table,
#                          grobs = segmentsGrob( # line across the bottom
#                            x0 = unit(0,"npc"),
#                            y0 = unit(0,"npc"),
#                            x1 = unit(1,"npc"),
#                            y1 = unit(0, "npc"),
#                            gp = gpar(lwd = 1.0)),
#                          t = 1, b = 10, l = 1, r = ncol(table))
# 
# table <- gtable_add_grob(table,
#                          grobs = segmentsGrob( # line across the bottom
#                            x0 = unit(0,"npc"),
#                            y0 = unit(0,"npc"),
#                            x1 = unit(1,"npc"),
#                            y1 = unit(0, "npc"),
#                            gp = gpar(lwd = 1.0)),
#                          t = 1, b = 20, l = 1, r = ncol(table))
# 
# 
# table <- gtable_add_grob(table,
#                          grobs = segmentsGrob( # line across the bottom
#                            x0 = unit(0,"npc"),
#                            y0 = unit(0,"npc"),
#                            x1 = unit(1,"npc"),
#                            y1 = unit(0, "npc"),
#                            gp = gpar(lwd = 1.0)),
#                          t = 1, b = 26, l = 1, r = ncol(table))

