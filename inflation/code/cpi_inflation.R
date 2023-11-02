#### CPI Chartpack
### Created: July 2023

rm(list = ls())

# Update CPI Data -----------------------------------#

source(paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Data Bank/r_program_set_up/set_up.R', sep = ""))


source(paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank/macro/code/inflation_data.R", sep = ""))


# ----------------------------------------------------------#

current_month <- "September"
current_year <- "2023"
prev_month <- "July"

data_folder <- paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank/macro/", sep = "")

macro_folder <- paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Ad Hoc/US Macroeconomic Developments/", sep = "")

# Save previous data in archive folder ---------------------#

files <- list.files(paste(macro_folder, "inflation/output/underlying_figures", sep = ""), pattern = "\\.pdf$", full.names = TRUE)

for (file in files) {
  if (!file.exists(file)) {
    new_file_name <- paste0(macro_folder, "inflation/output/underlying_figures/archive/", basename(file), "_", prev_month, " ", current_year, ".pdf", sep = "" )
    file.copy(file, new_file_name)
    print('New file created.')
  }else{
    print('File already exists.')
  }
}

chartpack <- list.files(paste(macro_folder, "inflation/output/", sep = ""), pattern = "\\.pdf$", full.names = TRUE)

for (file in chartpack) {
  if (!file.exists(file)) {
    new_file_name <- paste0(macro_folder, "inflation/output/archive/", basename(file), "_", prev_month, " ", current_year, ".pdf", sep = "" )
    file.copy(file, new_file_name)
    print('New file created.')
  }else{
    print('File already exists.')
  }  
}


# Data  ----------------------------------------------------#

uscpi_yoy <-  read_excel(paste0(data_folder, "data/us_cpi_bloomberg.xlsx", sep = ""), sheet = "BBG Values") %>%
  mutate(date = as.Date(as.POSIXct(date))) 


uscpi_ann <- read_excel(paste0(data_folder, "data/us_cpi_bloomberg.xlsx", sep = ""), sheet = "Annualized") %>%
  mutate(date = as.Date(as.POSIXct(Date))) 


uscpi <- uscpi_yoy %>% left_join(uscpi_ann, by = "date")

# Charts ---------------------------------------------------#

## Headline

my_breaks <- seq(as.Date("2021-01-31"), max(as.Date(uscpi$date)), by = "3 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01",
         paste(format(x, "%b"), "\n", format(x, "%y")), 
         format(x, "%b"))
})

plot_cpi_head <- uscpi %>% filter(date > as.Date("2021-01-31")) %>% select(c(date, headline_yoy, headline_mom_ann)) %>%
  ggplot(aes(x=date)) +
  geom_bar(mapping = aes(y=headline_yoy, fill = "Change YoY"), stat = "identity", position = "dodge", alpha = 0.7, color = "grey30") +
  geom_line(mapping = aes(y=headline_mom_ann* 100, color = "Three month annual rate"), linewidth = 1) +
  scale_color_manual(values = c(PEACH)
  ) +
  scale_fill_manual(values = c(MEDIUMGREEN)
    ) +
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1, nrow=2), fill = guide_legend(order = 3, nrow = 2)) +
  plot_theme +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0.5,  size = 20),
        plot.subtitle = element_text(hjust = 0,   size = 14),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 14),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),

        axis.text.x = element_text(size = 14),
        axis.line.y.left = element_line(color = "grey30"),
        axis.text.y.left =  element_text(size = 14), 
        axis.ticks.length.y.left = unit(-1, "mm"),
        axis.text.y.right = element_text(size = 4, color = "white"), 
        axis.line.y.right = element_line(color = "grey30"),
        axis.ticks.length.y.right = unit(-1, "mm")
        ) +
  labs(title = "Headline\n",
    subtitle = "Percent (%)") +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0.03,0)
  ) +
  scale_y_continuous(
    sec.axis = dup_axis(),
    limits = c(0, 12),
    breaks = seq(0, 12, by =1.5), 
    expand = c(0, 0)
  ) 

grid.draw(plot_cpi_head)

ggsave(paste0(macro_folder, 'inflation/output/underlying_figures/Headline CPI.png', sep = ''), plot_cpi_head, width = 11, height = 8)



#Core 

plot_cpi_core <- uscpi %>% filter(date > as.Date("2021-01-31")) %>% select(c(date, core_yoy, core_mom_ann)) %>%
  ggplot(aes(x=date)) +
  geom_bar(mapping = aes(y=core_yoy, fill = "Change YoY"), stat = "identity", position = "dodge", alpha = 0.7, color = "grey30") +
  geom_line(mapping = aes(y=core_mom_ann* 100, color = "Three month annual rate"), linewidth = 1) +
  scale_color_manual(values = c(PEACH)
  ) +
  scale_fill_manual(values = c(MEDIUMGREEN)
  ) +
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1, nrow=2), fill = guide_legend(order = 3, nrow = 2)) +
  plot_theme +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0.5,  size = 20),
        plot.subtitle = element_text(hjust = 1,   size = 14),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 14),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        
        axis.text.x = element_text(size = 14),
        axis.line.y.left = element_line(color = "grey30"),
        axis.text.y.left = element_text(size= 4, color = "white"), 
        axis.ticks.length.y.left = unit(-1, "mm"),
        axis.text.y.right = element_text(size = 14), 
        axis.line.y.right = element_line(color = "grey30"),
        axis.ticks.length.y.right = unit(-1, "mm")
  ) +
  labs(title = "Core\n",
       subtitle = "Percent (%)") +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0.03,0)
  ) +
  scale_y_continuous(
    sec.axis = dup_axis(),
    limits = c(0, 12),
    breaks = seq(0, 12, by =1.5), 
    expand = c(0, 0)
  ) 

grid.draw(plot_cpi_core)


ggsave(paste0(macro_folder, 'inflation/output/underlying_figures/Core CPI.pdf', sep = ''), plot_cpi_core, width = 11, height = 8, device = cairo_pdf)





g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot_cpi_head)


#plot_inflation <- plot_headline + plot_core

plot_inflation <- grid.arrange(arrangeGrob(plot_cpi_head + theme(legend.position="none"),
                                           plot_cpi_core + theme(legend.position="none"),
                                           nrow=1),
                               mylegend, nrow=2,heights=c(10, 1))


ggsave(paste0(macro_folder, 'inflation/output/CPI Inflation.png', sep = ''), plot_inflation, width = 1, height = 8)

