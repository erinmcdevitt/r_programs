#### RCG Styles
#### Created By:erin.mcdevitt
#### Created: 2023-07-06



#############################################################################
# Colors -------------------------------------------------------------------#
#############################################################################



SOFTBLUE <- "#C2EDFE"
PALEBLUE <- "#A7CDFF"
GREYBLUE <- "#A1B8CA"   
GREENBLUE <-  "#80BBCB"
 

LIGHTGREEN <- "#D5F7E9"
LIMEGREEN <-  "#AEF0D5"
MEDIUMGREEN <- "#427E83"
DARKGREEN <-  "#006666"
  

PEACH <- "#FF9E80"
ORANGE <- "#FFCC99"


LIGHTGREY <- "#CED8DF"
GREENGREY <- "#D9DCD2"
BLUEGREY <- "#CED9D8"
DARKGREY <- "#3D4E57"
BLACK <- "#292627"



#############################################################################
# Fonts --------------------------------------------------------------------#
#############################################################################


library(extrafont) 
# library(showtext)

#Database of fonts (includes Tobias, Roboto, Georgia, and Arial)
loadfonts(device = "win", quiet = TRUE)




#############################################################################
# Themes -------------------------------------------------------------------#
#############################################################################


family <- "roboto"
color <- "grey30"

plot_theme <- theme(
  #grid size
  plot.margin = margin(0, 10, 10, 10),
  
  #text
  plot.title = element_text(hjust = 0.5, color= color, size = 16, family = family),
  plot.subtitle = element_text(hjust = 1, color= color, size = 16, family = family),
  plot.caption = element_text(hjust = 0, color= color, size = 16, family = family),
    
  #legend
  legend.position = "bottom",
  legend.justification = "center",
  legend.title = element_blank(),
  legend.key.size = unit(0.5, 'cm'),
  legend.text = element_text(size = 16, color= color),
  legend.key = element_blank(),
  
  #plot background
  rect = element_rect(fill = "transparent"),
  panel.background = element_rect(fill = "transparent", color = NA),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA),
  #panel.grid.major.y = element_blank()),
  
  #both axes
  axis.title = element_blank(),
  
  #x axis
  axis.text.x = element_text(hjust = 0.5, size = 14, color = color),
  axis.ticks.length.x = unit(-2, "mm"),
  axis.line.x.bottom = element_line(color= color),

  #y axis - all
  axis.ticks.y = element_line(color = color),
  axis.ticks.x = element_line(color = color),

  #y axis - left
  axis.text.y.left = element_blank(),
  axis.line.y.left = element_line(color= color),
  axis.ticks.length.y.left = unit(-2, "mm"), 
  
  #y axis right
  axis.text.y.right = element_text(size = 14, color= color, family = family),
  axis.line.y.right = element_line(color= color),
  axis.ticks.length.y.right = unit(-2, "mm"),   
)



annotate_theme <- theme(plot.title = element_text(hjust = 0, size = 14, family = family),
                        plot.caption = element_text(hjust =0, size = 12, color = "grey30", family = family))
