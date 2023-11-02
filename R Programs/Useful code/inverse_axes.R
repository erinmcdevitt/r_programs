#Creating inverse Charts 


#coordinates aka the limits of each dataset
p1 <- c(-120, 3)
p2 <- c(-30, 5)

# Calculate the slope of the line
m <- (p2[2] - p1[2]) / (p2[1] - p1[1])

#find intercept
b <- p1[2] - m * p1[1]

print(c(m, b))


#use intercept to inform plot, e.g. below
ggplot(data = treasury_check, aes(x = date, y = DGS10)) + 
  geom_line(colour = "black") + 
  geom_line(aes(y = (curve_2s10s * m + b)), colour = "magenta3", size=1) +
  scale_y_continuous(limits = c(3, 5),
                     sec.axis = sec_axis(~(.-b)/m, breaks = seq(-120, -30, by = 15)))




# Example

# Treasury Yields

list <- list(
  series_id = c("DGS10","DGS2", "T10Y2Y" ),
  frequency = c("d")
)

treasury <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y, units = "lin")) %>%
  select(c(date, series_id, value)) %>%
  filter(date >= "2022-01-01") %>%
  pivot_wider(id_cols = date, 
              names_from = series_id, 
              values_from = value) %>%
  mutate(curve_2s10s= (T10Y2Y*100))


treas_ts <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y, units = "lin")) %>%
  select(c(date, series_id, value)) %>%
  filter(date >= "2005-01-01") %>%
  pivot_wider(id_cols = date, 
              names_from = series_id, 
              values_from = value) %>%
  mutate(curve_2s10s= (T10Y2Y*100))



my_breaks <- seq(as.Date("2023-01-03"), max(as.Date(treasury$date)), by = "1 month")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01",
         paste(format(x, " %b"), "\n", format(x, "%Y")), 
         format(x, "%b"))
})

p1 <- c(-120, 3.2)
p2 <- c(-30, 4.8)

# Calculate the slope of the line
m <- (p2[2] - p1[2]) / (p2[1] - p1[1])
b <- p1[2] - m * p1[1]

test <- treasury %>% mutate(test = curve_2s10s * m + b)

plot_treas <- treasury %>% filter(!is.na(DGS10)) %>% filter(date >= as.Date("2023-01-01")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = (DGS10), color = "10Yr (LHS)"), linewidth = 1) +
  geom_line(aes(y = (curve_2s10s * m + b), color = "2s10s (RHS)"), linewidth = 1) +
  scale_color_manual(values = c(DARKGREEN, GREYBLUE)) +
  plot_theme +
  theme(  axis.text.y.left = element_text(size = xsmall, color= color, family = family),  
          axis.line.y.left = element_line(color= color),
          axis.ticks.length.y.left = unit(-2, "mm"), 
          
          legend.box = "horizontal",
          legend.position = c(0.8, 0.7),
          legend.key.size = unit(2,"mm")) +
  labs(title = "Yield Curve bear steepens as Treasuries surge to highest level in a decade",
       subtitle = "Percent", 
       caption = "\nSource: RockCreek; Federal Reserve, H.15 Selected Interest Rates.")  +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +
  scale_y_continuous(limits = c(3.2, 4.8),
                     sec.axis = sec_axis(~(.-b)/m, breaks = seq(-110, -20, by = 10)),
                     breaks = seq(3.4, 4.8, by =0.2),
                     expand = c(0, 0)
  ) 


grid.draw(plot_treas)
