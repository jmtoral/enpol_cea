# Mapas



# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, sf, scales, biscale, cowplot, janitor, gridExtra, ggsflabel)
#extrafont::loadfonts(device = "win", quiet =T)

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/bd_enpol_2021_RData/BD_ENPOL_2021.RData")

diccionario <- read_csv("input/diccionario_de_datos_ENPOL2021_2_3.csv",
                        locale = locale(encoding="LATIN1"))
diccionario2 <- read_csv("input/diccionario_de_datos_ENPOL2021_7.csv",
                         locale = locale(encoding="LATIN1"))

mx <- st_read("input/México_Estados/México_Estados.shp") %>% 
  mutate(ESTADO = str_replace(ESTADO, "Coahuila","Coahuila de Zaragoza"),
         ESTADO = str_replace(ESTADO, "Michoacán","Michoacán de Ocampo"),
         ESTADO = str_replace(ESTADO, "Veracruz","Veracruz de Ignacio de la Llave"),
         ESTADO = str_replace(ESTADO, "Distrito Federal","Ciudad de México"),
         ESTADO = str_replace(ESTADO, "^México$","Estado de México"))  %>% 
  rename(NOM_ENT = ESTADO)
## Diseño -----------------------------------------------------------------------------------------------------------------------------------

mapas <- ENPOL2021_SOC  %>% 
  left_join(ENPOL2021_2_3) %>% 
  left_join(ENPOL2021_7) %>% 
  left_join(ENPOL2021_8_9_10_11) %>% 
  # mutate(no_hetero = as.numeric(!P1_23 %in% c(3,8,9))) %>%
  #mutate(trans = as.numeric(P1_22 %in% c(3,4))) %>%
  mutate(FAC_PER = as.numeric(as.character(FAC_PER)),
         EST_DIS = as.numeric(as.character(EST_DIS)),
         FPC = as.numeric(as.character(FPC)), 
         total = 1) %>% 
  as_survey_design(strata = EST_DIS, 
                   weights = FAC_PER, 
                   fpc = FPC)


#Entidades con mayores porcentajes de percepción de discriminación: Mapa estados con población LGBTQ+ (que implica concentrar en una sola variable a todas aquellas que se consideran no heterosexuales y que se identifican como trans) que perciben que se les ha discriminado por su orientación sexual o identida de género.


## Orientación + Idenditad

nohet_disc <- mapas %>% 
  #filter(P1_23 %in% c(1,2)) %>%
  #filter(P1_22 %in% c(3,4)) %>%
  #mutate(nohet = P1_23 %in% c(1,2)) |> 
  #mutate(nohet = as.numeric(nohet)) |> 
  #filter(P7_46_08 %in% c(1)) %>% 
  #filter(nohet == 1) |> 
  filter(P1_23 %in% c(1,2) | P1_22 %in% c(3,4)) |> 
  group_by(NOM_ENT, P7_46_08) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"),
                                        level = 0.95),
            total_personas = survey_total(na.rm=T)) |> 
  filter(P7_46_08 == 1)


mx %>%
  left_join(nohet_disc) %>% 
  mutate(percentlabel =paste0(round(prop_personas*100, 1),"%")) %>% 
  ggplot(aes(fill=prop_personas, label=percentlabel)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  geom_sf_text_repel(aes(label = percentlabel),
                     nudge_x = 0, nudge_y = -0.6, seed = 10) +
  scale_fill_gradient(low='#ffd400',high='#ff5400', 
                      name='% de personas', 
                      breaks = 0.05*0:6, 
                      labels = percent(0.05*0:6),
                      limit = c(0,.3)) +
  labs(title = "Porcentaje de personas LGBTQ+ que reportaron haberse sentido discriminadas \npor su orientación sexual o preferencia de género",
       subtitle = "con respecto al total de población LBGTQ+ privada se su libertad por estado",
       caption = "Fuente: ENPOL 2021 - INEGI",
       x="", y="") +
  hrbrthemes::theme_ipsum()

ggsave("graficas/zmap1_discriminación.png", width = 12, height = 8)
ggsave("graficas_eps/zmap1_discriminación.eps", width = 12, height = 8,  device="eps")

#Victimización por entidad federativa: Mapa estados con población LGBTQ+ con aquellas que reportan haber tenido alguna forma de victimización (lo que implica poner en una sola variable todas las formas de victimización) 

victimas <- mapas %>% 
  filter(P1_23 %in% c(1,2) | P1_22 %in% c(3,4)) |>  
  filter(P7_40_1 == 1 | P7_40_2 == 1 | P7_40_3 == 1 | P7_40_4 == 1 | 
           P7_40_5 == 1 | P7_40_6 == 1) %>%
  group_by(NOM_ENT) %>% 
  summarize(total_personas = survey_total(na.rm=T))

total_ent <- mapas |> 
  filter(P1_23 %in% c(1,2) | P1_22 %in% c(3,4)) |> 
  group_by(NOM_ENT) |> 
  summarise(total_lgbt = survey_total(na.rm=T))

mx |> 
  left_join(victimas) |> 
  left_join(total_ent) |> 
  mutate(prop_personas = total_personas/total_lgbt)%>% 
  mutate(percentlabel =paste0(round(prop_personas*100, 1),"%")) %>% 
  ggplot(aes(fill=prop_personas, label=percentlabel)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  geom_sf_text_repel(aes(label = percentlabel),
                     nudge_x = 0, nudge_y = -0.6, seed = 10) +
  scale_fill_gradient(low='grey99',high='#F00000', 
                      name='% de personas', 
                      breaks = seq(0,1,.2), 
                      labels = percent(seq(0,1,.2)),
                      limit = c(0,1)) +
  labs(title = "Porcentaje de personas LGBTQ+ que sufrieron al menos una situación de victimización",
       subtitle = "con respecto al total de población LBGTQ+ privada se su libertad por estado",
       caption = "Fuente: ENPOL 2021 - INEGI",
       x="", y="") +
  hrbrthemes::theme_ipsum() 


ggsave("graficas/zmap4_victimas.png", width = 12, height = 8)
ggsave("graficas_eps/zmap4_victimas.eps", width = 12, height = 8,  device="eps")


#Acceso a medicamentos. Mapa estados con población LGBTQ+ con aquellas que reportan no haber recibido medicamentos para alguna de sus enfermedades (excepto COVID) 

sinacceso_med <- mapas %>% 
  filter(P1_23 %in% c(1,2) | P1_22 %in% c(3,4)) %>% 
  filter(P1_26_1 %in% c(2) |
         P1_26_2 %in% c(2) |
         P1_26_3 %in% c(2) |
         P1_26_4 %in% c(2) |
         P1_26_5 %in% c(2) |
         P1_26_6 %in% c(2) |
         P1_26_7 %in% c(2) |
         P1_26_8 %in% c(2) |
         P1_26_9 %in% c(2))  %>%
  group_by(NOM_ENT) %>% 
  summarize(total_personas = survey_total(na.rm=T))


sinacceso_n <- mapas %>% 
  filter(P1_23 %in% c(1,2) | P1_22 %in% c(3,4)) %>% 
  filter(P1_28_1 %in% c(1,2,3) |
           P1_28_2 %in% c(1,2,3) |
           P1_28_3 %in% c(1,2,3) |
           P1_28_4 %in% c(1,2,3) |
           P1_28_5 %in% c(1,2,3) |
           P1_28_6 %in% c(1,2,3) |
           P1_28_7 %in% c(1,2,3) |
           P1_28_8 %in% c(1,2,3) |
           P1_28_9 %in% c(1,2,3))  %>%
  group_by(NOM_ENT) %>% 
  summarize(total_personas = survey_total(na.rm=T))
  
total_ent <- mapas |> 
  filter(P1_23 %in% c(1,2) | P1_22 %in% c(3,4)) |> 
  group_by(NOM_ENT) |> 
  summarise(total_lgbt = survey_total(na.rm=T))


mx |> 
  left_join(sinacceso_med) |> 
  left_join(total_ent) |> 
  mutate(prop_personas = total_personas/total_lgbt)%>% 
  mutate(percentlabel =paste0(round(prop_personas*100, 1),"%")) %>% 
  ggplot(aes(fill=prop_personas, label=percentlabel)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  geom_sf_text_repel(aes(label = percentlabel),
                     nudge_x = 0, nudge_y = -0.6, seed = 10) +
  scale_fill_gradient(low='grey99',high='#F00000', 
                      name='% de personas', 
                      breaks = seq(0,.3,.05), 
                      labels = percent(seq(0,.3,.05)),
                      limit = c(0,.3)) +
  labs(title = "Porcentaje de personas LGBTQ+ que no reciben tratamiento para al menos una enfermedad",
       subtitle = "con respecto al total de población LBGTQ+ privada se su libertad por estado",
       caption = "Fuente: ENPOL 2021 - INEGI\n
       Se incluyen personas que dicen no necesitarlo, curarse con remedios caseros o haber terminado el tratamiento.",
       x="", y="") +
  hrbrthemes::theme_ipsum() 


ggsave("graficas/zmap5_medicamentos.png", width = 12, height = 8)
ggsave("graficas_eps/zmap5_medicamento.eps", width = 12, height = 8,  device="eps")


#Visita íntima, Mapa estados con población LGBTQ+ con porcentajes de visita íntima (igual con este tengo duda).

nohet_visitint <- mapas %>% 
  #filter(P1_23 %in% c(1,2)) %>%
  #filter(P1_22 %in% c(3,4)) %>%
  #mutate(nohet = P1_23 %in% c(1,2)) |> 
  #mutate(nohet = as.numeric(nohet)) |> 
  #filter(P7_46_08 %in% c(1)) %>% 
  #filter(nohet == 1) |> 
  filter(P1_23 %in% c(1,2) | P1_22 %in% c(3,4)) |> 
  group_by(NOM_ENT, P7_31 ) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"),
                                        level = 0.95),
            total_personas = survey_total(na.rm=T)) |> 
  filter(P7_31 == 2)


mx %>%
  left_join(nohet_visitint) %>% 
  mutate(percentlabel =paste0(round(prop_personas*100, 1),"%")) %>% 
  ggplot(aes(fill=prop_personas, label=percentlabel)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  geom_sf_text_repel(aes(label = percentlabel),
                     nudge_x = 0, nudge_y = -0.6, seed = 10) +
  scale_fill_gradient(low='#89fda6',high='#027d21', 
                      name='% de personas', 
                      breaks = seq(.5,1,.1), 
                      labels = percent(seq(.5,1,.1)),
                      limit = c(0.5,1)) +
  labs(title = "Porcentaje de personas LGBTQ+ que no recibieron visita íntima",
       subtitle = "con respecto al total de población LBGTQ+ privada se su libertad por estado, sin contar las suspensiones de visita por COVID-19",
       caption = "Fuente: ENPOL 2021 - INEGI",
       x="", y="") +
  hrbrthemes::theme_ipsum()

ggsave("graficas/zmap3_visita.png", width = 12, height = 8)
ggsave("graficas_eps/zmap3_visita.eps", width = 12, height = 8,  device="eps")



#Mapa sobre Mapa estados con población LGBTQ+ con aquellas que se les proporcionaron las herramientas necesarias para la reinserción social.

nohet_rein <- mapas %>% 
  #filter(P1_23 %in% c(1,2)) %>%
  #filter(P1_22 %in% c(3,4)) %>%
  #mutate(nohet = P1_23 %in% c(1,2)) |> 
  #mutate(nohet = as.numeric(nohet)) |> 
  #filter(P7_46_08 %in% c(1)) %>% 
  #filter(nohet == 1) |> 
  filter(P1_23 %in% c(1,2) | P1_22 %in% c(3,4)) |> 
  group_by(NOM_ENT, P10_6) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"),
                                        level = 0.95),
            total_personas = survey_total(na.rm=T)) |> 
  filter(P10_6 == 1)


mx %>%
  left_join(nohet_rein) %>% 
  mutate(percentlabel =paste0(round(prop_personas*100, 1),"%")) %>% 
  ggplot(aes(fill=prop_personas, label=percentlabel)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  geom_sf_text_repel(aes(label = percentlabel),
                     nudge_x = 0, nudge_y = -0.6, seed = 10) +
  scale_fill_gradient(low='#b0bdd5',high='#001dff', 
                      name='% de personas', 
                      breaks = seq(.5,1,.1), 
                      labels = percent(seq(.5,1,.1)),
                      limit = c(0.5,1)) +
  labs(title = "Porcentaje de personas LGBTQ+ sienten que el Centro les dio las herramientas \npara la reinserción social",
       subtitle = "con respecto al total de población LBGTQ+ privada se su libertad por estado",
       caption = "Fuente: ENPOL 2021 - INEGI",
       x="", y="") +
  hrbrthemes::theme_ipsum()

ggsave("graficas/zmap2_reins.png", width = 12, height = 8)
ggsave("graficas_eps/zmap2_reins.eps", width = 12, height = 8,  device="eps")
