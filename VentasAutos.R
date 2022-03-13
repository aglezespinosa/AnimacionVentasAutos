##############################################################################
# Grafica Animada de las Ventas de autos en Mexico                           #
# (c) Angel Gonzalez Espinosa 2022                                           #
# aglezespinosa@gmail.com                                                    #
##############################################################################
library(tidyverse)
library(janitor)
library(gganimate)

ventaautos <- read_csv("DatosVentaAutos.csv")

anim2 <- ventaautos %>%
  group_by(Anio) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         #Value_lbl = paste0(" ",round(value/1e9))) %>%
         #Value_lbl = paste0(" ",round(value))) %>%
         Value_lbl = paste0(" ",value)) %>%
  group_by(Marca) %>% 
  filter(rank <=10) %>%
  
  # plot
  ggplot(aes(-rank,Value_rel, fill = Marca)) +
  geom_col(width = 0.8, position="identity") +
  coord_flip() + 
  geom_text(aes(-rank,y=0,label = Marca,hjust=0)) +       #country label
  geom_text(aes(-rank,y=Value_rel,label = Value_lbl, hjust=0)) + # value label
  
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  theme_minimal() +
  theme(legend.position = "none",axis.title = element_blank()) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="red", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  
  
  # animate along Year
  #transition_states(year,4,1)
  transition_states(Anio, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Evolución de las Ventas de Autos en Mexico: {closest_state}',  
       subtitle  =  "Principales Marcas de Autos. Fuente de Datos: AMIA",
       caption  = "Desarrollado por: Angel Gonzalez E. email: aglezespinosa@gmail.com") 


#animate(p, 100, fps = 25, duration = 20, width = 800, height = 600)
# animate(plot, nframes, fps, duration, detail, renderer,
#device, ref_frame, start_pause, end_pause, rewind, ...)

animate(anim2, 200, fps = 20,  duration = 50, width = 800, height = 600, 
        renderer = gifski_renderer("ventasautos1.gif"))