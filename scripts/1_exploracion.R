# Análisis de la ENPOL para Proyecto LGBTI
# Fecha de elaboración: 2022/03/09
# Fecha de actualización: 2022/03/10
# Autor: Manuel Toral


###### Preguntas de interés

# P7_46_08 y quizás la comparación con las P7_46_*

# P1_23 1.23 De las siguientes frases, ¿cuál define su orientación sexual?

# P1_22 1.22 ¿Cómo se identifica usted?

# La comparación con las socioeconómicas P1_*

######



# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, sf, scales, biscale, cowplot)
extrafont::loadfonts(device = "win", quiet =T)

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/bd_enpol_2021_RData/BD_ENPOL_2021.RData")

mx <- st_read("input/México_Estados/México_Estados.shp") %>% 
  mutate(ESTADO = str_replace(ESTADO, "Coahuila","Coahuila de Zaragoza"),
         ESTADO = str_replace(ESTADO, "Michoacán","Michoacán de Ocampo"),
         ESTADO = str_replace(ESTADO, "Veracruz","Veracruz de Ignacio de la Llave"),
         ESTADO = str_replace(ESTADO, "Distrito Federal","Ciudad de México"),
         ESTADO = str_replace(ESTADO, "^México$","Estado de México"))  %>% 
  rename(NOM_ENT = ESTADO)

# Limpieza y orden -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


## Lo primero es estar segurxs del uso del factor de expansión -----------------------------------------------------------------------------------------------------------------------------------

soc <- ENPOL2021_SOC  %>% 
  mutate(no_hetero = as.numeric(!P1_23 %in% c(3,8,9))) %>%
  mutate(trans = as.numeric(P1_22 %in% c(3,4))) %>%
  mutate(FAC_PER = as.numeric(as.character(FAC_PER)),
         EST_DIS = as.numeric(as.character(EST_DIS)),
         FPC = as.numeric(as.character(FPC)), 
         total = 1) %>% 
  as_survey_design(strata = EST_DIS, 
                   weights = FAC_PER, 
                   fpc = FPC)


soc %>% 
  group_by(NOM_ENT) %>% 
  summarize(total_personas = survey_total(total, 
                                          na.rm=T, 
                                          vartype = c("ci"), 
                                          level = 0.95))

## La población total corresponde con los tabuladores



## Análisis socioeconómico -----------------------------------------------------------------------------------------------------------------------------------------------------------------------


### Cruces ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

soc %>% 
  group_by(P1_22, P1_23) %>% 
  summarize(total_personas = survey_total(total)) %>% 
  ungroup %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre", "2"="Mujer","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                        "8" = "No entendí")) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
  group_by(P1_22) %>% 
  mutate(suma_p1_22 = sum(total_personas)) %>% 
  ungroup %>% 
  mutate(prop = total_personas/suma_p1_22) %>% 
  mutate(prop = round(prop*100,1)) %>% 
  select(-total_personas_se, -total_personas, -suma_p1_22) %>% 
  pivot_wider(P1_22, names_from = P1_23, values_from = prop)


soc %>% 
  group_by(P1_22, P1_23) %>% 
  summarize(total_personas = survey_total(total)) %>% 
  ungroup %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre", "2"="Mujer","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                        "8" = "No entendí")) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
  select(-total_personas_se) %>% 
  pivot_wider(P1_22, names_from = P1_23, values_from = total_personas)

### Personas trans por entidad -------------------------------------------------------------------------------------------------------------------------------------------------------------------


trans_ent <- soc %>% 
  filter(!P1_22 %in% c(8,9)) %>% 
  group_by(NOM_ENT, trans) %>% 
  summarize(total_personas = survey_total(total)) %>% 
  mutate(total_personas = round(total_personas)) %>% 
  select(-total_personas_se) %>% 
  as_tibble() %>% 
  ungroup %>% 
  pivot_wider(NOM_ENT, names_from = trans, values_from = total_personas) %>% 
  mutate(percent = round(`1`/`0`,4))



mx %>%
  left_join(trans_ent) %>% 
  mutate(percentlabel =paste0(round(percent*100, 1),"%")) %>% 
  ggplot(aes(fill=percent)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  labs(title = "Porcentaje de PPL transexual, transgénero o travesti",
       subtitle = "privadas de la libertad con respecto al total",
       caption = "Fuente: ENPOL 2021 - INEGI",
       x="", y="") +
  scale_fill_gradient(low='#b9fefe',high='#028686', 
                      name='% de personas', 
                      breaks = 0.004*0:5, labels = percent(0.004*0:5)) +
  hrbrthemes::theme_ipsum()+
  annotate(
    geom = "curve", size =0.8,
    x = -108, y = 19, 
    xend = -104, yend = 19, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = -112, y = 20, 
           label = "En Colima\n1.7% de la PPL se\nautodenomina trans", 
           hjust = "left") +
  annotate(
    geom = "curve", size =0.8,
    x = -95, y = 25, 
    xend = -99, yend = 19.5, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = -94.5, y = 26, label = "En CDMX y EdoMex\nhay 289 personas\nautodenominadas\ncomo trans", hjust = "left")


ggsave("graficas/tot_edo_trans.png", width = 10, height = 8)

### Personas no heterosexuales por entidad -------------------------------------------------------------------------------------------------------------------------------------------------------------------

no_hetero_ent <- soc %>% 
  filter(!P1_23 %in% c(8,9)) %>% 
  group_by(NOM_ENT, no_hetero) %>% 
  summarize(total_personas = survey_total(total)) %>% 
  mutate(total_personas = round(total_personas)) %>% 
  select(-total_personas_se) %>% 
  as_tibble() %>% 
  ungroup %>% 
  pivot_wider(NOM_ENT, names_from = no_hetero, values_from = total_personas) %>% 
  mutate(percent = round(`1`/`0`,4))

mx %>%
  left_join(no_hetero_ent) %>% 
  mutate(percentlabel =paste0(round(percent*100, 1),"%")) %>% 
  ggplot(aes(fill=percent)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  scale_fill_gradient(low='#ffccff',high='#910092', 
                      name='% de personas', 
                      breaks = 0.025*0:6, 
                      labels = percent(0.025*0:6),
                      limit = c(0,.13)) +
  labs(title = "Porcentaje de PPL bisexuales, homosexuales o con otra preferencia sexual",
       subtitle = "con respecto al total de población penitenciaria por estado",
       caption = "Fuente: ENPOL 2021 - INEGI",
       x="", y="") +
  hrbrthemes::theme_ipsum()+
  annotate(
    geom = "curve", size =0.8,
    x = -95, y = 25, 
    xend = -99, yend = 19, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = -94.5, y = 26, 
           label = "En Morelos poco más\nde 12% de la PPL\nse considera bisexual,\nhomosexual u otro.", 
           hjust = "left") +
  annotate(
    geom = "curve", size =0.8,
    x = -108, y = 19, 
    xend = -99.5, yend = 19.5, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = -115, y = 20, label = "En CDMX y EdoMex\nhay 2,298 PPL\nautodenominadas\ncomo bisexuales,\nheterosexuales\nu otra preferencia sexual.", hjust = "left")


ggsave("graficas/tot_edo_nhet.png", width = 10, height = 8)



            


 ### Bivariado -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


data <- mx %>% left_join(no_hetero_ent %>% 
  select(NOM_ENT, percent) %>% 
  rename(p_no_het = percent) %>% 
  left_join(trans_ent %>% 
              select(NOM_ENT, percent) %>% 
              rename(p_trans = percent), by="NOM_ENT")) %>% 
  bi_class(x = p_no_het, y = p_trans, style = "quantile", dim = 3)


map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), 
          color = "white", size = 0.1, show.legend = F) +
  geom_sf(color="white", size=0.8)+
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  coord_sf(datum = NA)+
  labs(
    title = "Proporción de población privada de la libertad LGBTQ+",
    subtitle = "Porcentaje de PPL bisexuales, homosexuales o con otra preferencia sexual vs. porcentaje de PPL transexual, transgénero o travesti",
    caption = "Fuente: ENPOL 2021 - INEGI"
  ) +
  hrbrthemes::theme_ipsum()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Mayor % bisexual, homosexual u otra",
                    ylab = "Mayor % transexual",
                    size = 10)

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.1, 0.3, 0.3)

ggsave("graficas/tot_edo_bivariate.png", width = 10, height = 8)