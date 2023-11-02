# Cordflip and positive/negative examples


#Bond Market Returns

bond_return <- assets %>% filter(group == "bond") 

bond_return$series <-  factor(bond_return$series, levels = rev(c("US 2yr",
                                                                 "US 10yr",
                                                                 "US 30yr",
                                                                 "German 10yr",
                                                                 "German 30yr",
                                                                 "UK 10yr",
                                                                 "UK 30yr",
                                                                 "JGB 10yr",
                                                                 "JGB 30yr"
)))

bond_return$pos_neg <- ifelse(bond_return$value > 0, "positive", "negative")

plot_bond_return <- bond_return %>%
  ggplot(aes(x=series, y= value, fill = pos_neg)) +
  geom_col(color = "black", alpha = 0.5, width = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(values = c("positive" = GREYBLUE, "negative" = "red")) +  
  coord_flip() +
  geom_text(aes(label = value,        
                y=  case_when(value < 0 ~ value-5,
                              value > 0 ~ value+5),
                group = series), position = position_dodge(width = 0.9), size = 3.5, color = "grey30") + # New code line
  plot_theme2 +
  theme(
    plot.margin = unit(c(1, 1, 1, 4), "lines"),
    legend.position = "none",
    aspect.ratio = 1/2,
    axis.text.y = element_text(family = "roboto", size = 12, color = "grey30",margin = margin(l = 0, r = 40, t = 0, b = 0)),
    axis.line = element_blank(), 
    axis.text.x = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.ticks = element_blank()
  ) 


grid.draw(plot_bond_return)


ggsave(paste(output_folder, "/Table1b_Bonds.svg", sep = ''), plot_bond_return, width = 7, height = 5)

