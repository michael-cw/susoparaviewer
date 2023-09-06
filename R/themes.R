##  1. GGPLOT
styleWB_bar<-ggplot2::theme_bw()+
  ggplot2::theme(
    text=ggplot2::element_text(color = "skyblue2", size = 12),
    panel.border = ggplot2::element_rect(color = "skyblue2"),
    plot.title = ggplot2::element_text(face = "bold", size=16, colour = "skyblue2"),
    plot.subtitle = ggplot2::element_text(face = "bold", size=12, colour = "skyblue2"),
    plot.background = ggplot2::element_rect(fill = "white"),
    legend.background= ggplot2::element_rect(fill="skyblue2"),             # element_rect(fill="white", colour = "white", size=4),
    legend.justification = c(1,0),
    legend.position = c(1,0),
    legend.text = ggplot2::element_text(size=12, colour = "white"),
    axis.ticks = ggplot2::element_blank(),
    #axis.title = element_blank(),
    #axis.text.y = element_text(vjust=1, hjust=2),
    panel.grid.major = ggplot2::element_line(colour = "white", size = 0.2),
    panel.grid.minor = ggplot2::element_blank()
  )

styleWB_map<-ggplot2::theme_bw()+
  ggplot2::theme(
    text=ggplot2::element_text(color = "white", size = 12),
    panel.border = ggplot2::element_rect(color = "skyblue2"),
    plot.title = ggplot2::element_text(face = "bold", size=16, colour = "skyblue2"),
    plot.subtitle = ggplot2::element_text(face = "bold", size=12, colour = "skyblue2"),
    plot.background = ggplot2::element_rect(fill = "white"),
    legend.background= ggplot2::element_rect(fill=ggplot2::alpha("skyblue2", 0.5)),             # element_rect(fill="white", colour = "white", size=4),
    legend.text = ggplot2::element_text(face="bold"),
    legend.justification = c(1,0),
    legend.position = c(1,0),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "white", size = 0.2),
    panel.grid.minor = ggplot2::element_blank()
  )

## 2. Lattice
colselect<-myColours <- RColorBrewer::brewer.pal(6,"Blues")

styleWB_lat <- list(
  superpose.polygon=list(col=colselect[2:5], border="transparent"),
  strip.background=list(col=colselect[6]),
  strip.border=list(col="black")
)


styleWB_line<-ggplot2::theme_bw()+
  ggplot2::theme(
    text=ggplot2::element_text(color = "skyblue2", size = 12),
    panel.border = ggplot2::element_rect(color = "skyblue2"),
    plot.title = ggplot2::element_text(face = "bold", size=16, colour = "skyblue2"),
    plot.subtitle = ggplot2::element_text(face = "bold", size=12, colour = "skyblue2"),
    plot.background = ggplot2::element_rect(fill = "white"),
    legend.background= ggplot2::element_rect(fill="skyblue2"),             # element_rect(fill="white", colour = "white", size=4),
    legend.justification = c(1,0),
    legend.position = c(1,0),
    axis.ticks = ggplot2::element_blank(),
    #axis.title = element_blank(),
    #axis.text.y = element_text(vjust=1, hjust=2),
    panel.grid.major = ggplot2::element_line(colour = "white", size = 0.2),
    panel.grid.minor = ggplot2::element_blank()
  )



## GGPLOT style
styleMain<-ggplot2::theme(legend.justification=c(0,0), legend.position=c(0,0.05),
                 legend.background=ggplot2::element_rect(fill=ggplot2::alpha('#33D2FF', 0.3)),
                 legend.title = ggplot2::element_text(colour = '#FFFFFF', face = 'bold', size=11),
                 legend.text=ggplot2::element_text(colour = '#FFFFFF', face = 'bold', size=10),
                 legend.key.size = ggplot2::unit(0.5, "cm"))




