my_theme <- function() {
  
  # Colors
  color.background = "#f5f5f2"
  color.text = "grey20"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    
    # Format the grid
    #theme(panel.grid.major.y = element_blank()) +
    #theme(panel.grid.minor.y = element_blank()) +
    #theme(panel.grid.major.x = element_blank()) +
    #theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks         = element_blank()) +
    
    # Format the legend
    theme(legend.position = c(.35, .97), legend.background = element_rect(fill = "#f5f5f2"), legend.title = element_blank(), legend.text = element_text(color = color.text, size = 10, face = "bold")) +
    theme(legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.key.size = unit(.5, "cm")) + 
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=16, face = "bold")) +
    theme(plot.subtitle    = element_text(color=color.text, size=12)) + 
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_text(color=color.text, size = 10, face = "bold")) +
    theme(axis.text.x      = element_text(size = 10, family = "Courier")) +
    theme(axis.text.y      = element_text(size = 10, family = "Courier")) +
    theme(strip.text       = element_text(face = "bold")) +
    theme(plot.caption = element_text(color = "grey70", size = 10)) + 
    
    # Plot margins
    theme(plot.margin = unit(c(.4, 0.5, 0.4, 0.4), "cm"))
}
