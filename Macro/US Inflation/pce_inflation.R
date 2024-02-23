#### PCE Inflation Chartpack
### Created: 2/27/2023  

rm(list = ls())

# ----------------------------------------------------------#

current_month <- "May"
current_year <- "2023"

prev_month <- "April"


list <- grep("cpi",names(.GlobalEnv),value=TRUE) 

rm(list = list)

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

# Preliminaries ------------------------------------------------------#


#Import data

us_pce <- read_excel(paste(data_folder, "data/inflation_data.xlsx", sep =""), sheet = 'us_pce') %>%
  mutate(date = as.Date(as.POSIXct(date))) 


#2019 Average 

avg_2019 <- us_pce %>%
  mutate(year = year(date)) %>%
  select(c(year, pcei_change_YoY, pcei_core_change_YoY)) %>%
  filter(year == 2019) %>%
  unique()

#Control time period
min_date <- as.Date("2020-01-01")
max_date <- max(as.Date(us_pce$date))


# Charts -------------------------------------------------------------

# Headline and Core PCE Summary - YoY Change

# set the breaks on the x-axis
my_breaks <- seq(as.Date("2020-01-01"), max(as.Date(us_pce$date)), by = "6 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%Y"), sep = ''), 
         format(x, "%b"))
})


plot_pce_index <- us_pce %>% filter(date >= min_date) %>%  
  ggplot(aes(x=date)) +
  geom_line(mapping = aes(y=pcei_core_change_YoY, color = "Core (YoY)"), linewidth = 0.75) +
  geom_line(mapping = aes(y=pcei_change_YoY, color = "Headline (YoY)"), linewidth = 0.75) +
  scale_color_manual(breaks = c("Headline (YoY)", "Core (YoY)"),
                     values = c("Headline (YoY)" = MEDIUMGREEN, 
                                "Core (YoY)" = GREYBLUE)
  ) +
  # guides(shape = guide_legend(order = 2, nrow = 3),col = guide_legend(order = 2, nrow=3), fill = guide_legend(order = 1, nrow = 3)
  # ) +
  plot_theme + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 14),
        plot.subtitle = element_text(hjust = 0,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_blank(),
        axis.line.y.left = element_line(color = "grey30")
  ) +
  labs(title = 'PCE Price Index Inflation\n',
       subtitle = 'Percent',
       caption = paste('Source: BEA. As of ', format(max(us_pce$date), "%B %Y"), ".", sep = "")
  ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0.03,0)
  ) +
  scale_y_continuous(
    limits = c(0, 8),
    breaks = seq(0, 8, by = 2), 
    expand = c(0, 0)
  )


grid.draw(plot_pce_index)

ggsave(paste0(macro_folder, 'inflation/output/underlying_figures/PCE Headline and Core.pdf', sep = ''), plot_pce_index, width = 11, height = 8, device = cairo_pdf)


# Core PCE - Annualized Rate 3months vs. 6months 

plot_pce_core_annual <- us_pce  %>% filter(date >= min_date) %>%  
  ggplot(aes(x=date)) +
  geom_hline(yintercept = 0, col = "grey30", size = 0.5, linetype = 1) +
  geom_bar(mapping = aes(y = pcei_core_ann_3Mo, fill = "3 months annualized"), position = "dodge", stat = "identity", alpha = 0.7, color = MEDIUMGREEN) +
  geom_line(mapping = aes(y=pcei_core_ann_6Mo, color = "6 months annualized"), linewidth = 0.75) +
  scale_color_manual(values = c(PEACH)
  ) +  
  scale_fill_manual(values = c(MEDIUMGREEN)
  ) + 
  guides(shape = guide_legend(order = 2, nrow = 3),col = guide_legend(order = 2, nrow=3), fill = guide_legend(order = 1, nrow = 3)
  ) +
  plot_theme + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 14),
        plot.subtitle = element_text(hjust = 0,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_blank(),
        axis.line.y.left = element_line(color = "grey30")
  ) +
  labs(title = 'Core PCE Price Index\n',
       subtitle = 'Percent',
       caption = paste('Source: BEA. As of ', format(max(us_pce$date), "%B %Y"), ".", sep = "")
  ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0.03,0)
  ) +
  scale_y_continuous(
    limits = c(-2, 8),
    breaks = seq(-2, 8, by = 2), 
    expand = c(0, 0)
  )



grid.draw(plot_pce_core_annual)

ggsave(paste0(macro_folder, 'inflation/output/underlying_figures/Core PCE Annualized.pdf', sep = ''), plot_pce_core_annual, width = 11, height = 8, device = cairo_pdf)


#PCE and Consumer Spending

plot_pce_spending <- us_pce %>% filter(date >= min_date) %>%   
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0, col = "grey30", size = 0.5, linetype = 1) +
  geom_bar(aes(y = pi_spending_value, fill = "Real spending (MoM)"), color = MEDIUMGREEN, position = "dodge", stat = "identity", alpha = 0.7) +
  geom_line(aes(y = pcei_change_YoY, color = "Headline Index (MoM)"), linewidth = 0.75) +
    geom_line(aes(y = pcei_core_change_YoY, color = "Core Index (MoM)"), linewidth = 0.75) +
  scale_color_manual(
    breaks = c("Headline Index (MoM)", "Core Index (MoM)"),
    values = c("Headline Index (MoM)" = PEACH, "Core Index (MoM)" = GREYBLUE)
  ) +
  scale_fill_manual(values = c(MEDIUMGREEN),
  ) +
  guides(shape = guide_legend(order = 2, nrow = 3),col = guide_legend(order = 2, nrow=3), fill = guide_legend(order = 1, nrow = 3)
  ) +
  plot_theme + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 14),
        plot.subtitle = element_text(hjust = 0,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.text.x = element_text(hjust = 0.5, size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_blank(),
        axis.line.y.left = element_line(color = "grey30")
  ) +
  labs(title = 'PCE Indexes and Inflation-adjusted Spending\n',
       subtitle = 'Percent',
       caption = paste('Source: BEA. As of ', format(max(us_pce$date), "%B %Y"), ".", sep = "")
  ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0.01,0)
  ) +
  scale_y_continuous(
    limits = c(-14, 10),
    breaks = seq(-14,10, by = 2), 
    expand = c(0, 0)
  ) 


grid.draw(plot_pce_spending)

ggsave(paste0(macro_folder, 'inflation/output/underlying_figures/PCE Index and Real Spending.pdf', sep = ''), plot_pce_spending, width = 11, height = 8, device = cairo_pdf)



# Export

title <- ggdraw() + draw_label("Price Developments", fontface='bold')
caption <- ggdraw() + draw_label(paste0("RockCreek - ", format(Sys.Date(), "%B %d, %Y"), sep = ''), size = 8,
                                 fontface = 'plain', x=0.03, y = 0.3, hjust = 0, alpha = 0.5)

mc1 <- list(plot_pce_spending, plot_pce_index, plot_pce_core_annual)

page1 <- plot_grid(mc1[[1]],
                   align = 'hv', nrow = 1, ncol = 1)

pce <- plot_grid(mc1[[2]], mc1[[3]], align = 'hv', nrow =1, ncol = 2)

pdf(paste0(macro_folder,'inflation/output/underlying_figures/page1.pdf', sep = ''), width = 8.27, height = 11.69)
plot_grid(title, page1, pce, caption, align = 'hv', nrow = 4, ncol = 1, rel_heights=c(0.1, 1, 1, 0.05)) 
dev.off()


input_path <- paste0(macro_folder,'inflation/output/underlying_figures', sep = '')

output_path <- paste0(macro_folder,'inflation/output/Inflation.pdf', sep = '')

setwd(input_path)
staple_pdf(input_directory = NULL, input_files = c("page1.pdf"), output = output_path, overwrite = TRUE)



