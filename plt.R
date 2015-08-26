#' ggplot2 theme from minimaxir.com
#'
#' @param scale_text use to scale the size of the text
#' @param font Well, the font you want
#' @examples
#'  ggplot(data=data.frame(x=1:100, y=rnorm(100)),aes(x=x,y=y))+
#'  geom_point()+labs(title="rnd", x="x",y="y")+theme_mini(scale_text=1.5)

theme_mini <- function(scale_text=1, font="Impact") {  
  # best font = "Impact", Times New Roman, Helvetica, Garamond, Futura, Lowetica
  # NEW BEST = "Arial Black", "Comfortaa", "Impact"
  # Generate the colors for the chart procedurally with RColorBrewer
  
  # Credit: Max Woolf http://minimaxir.com
  
  library(RColorBrewer)
  library(grid)
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(text=element_text(family=font))+
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    #theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7*scale_text,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10*scale_text, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7*scale_text,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7*scale_text,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8*scale_text,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8*scale_text,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}



# Credit to http://mathematicalcoffee.blogspot.fr/2014/06/ggpie-pie-graphs-in-ggplot2.html
ggpie <- function (dat, by, totals) {
  ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
    geom_bar(stat='identity', color='black') +
    scale_fill_brewer(palette = "Spectral") +
    guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
    coord_polar(theta='y') +
    theme(axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_text(colour='black'),
          axis.title=element_blank(),
          legend.position="none") +
    scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] / 2, labels=dat[[by]]) 
}
