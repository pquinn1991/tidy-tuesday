my_theme <- function() {
  
  # Colors
  color.background = "#f5f5f2"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    #theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    #theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks         = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold.italic")) +
    theme(plot.subtitle    = element_text(color=color.text, size=10, face = "italic")) + 
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_text(size = 14, face = "bold")) +
    theme(axis.text.x      = element_text(size = 9, face = "bold")) +
    theme(axis.text.y      = element_text(size = 9)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}