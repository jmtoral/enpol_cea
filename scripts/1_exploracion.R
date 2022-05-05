# Análisis de la ENPOL para Proyecto LGBTI
# Fecha de elaboración: 2022/03/09
# Fecha de actualización: 2022/03/23
# Autor: Manuel Toral


###### Preguntas de interés

# P7_46_08 y quizás la comparación con las P7_46_*

# P1_23 1.23 De las siguientes frases, ¿cuál define su orientación sexual?

# P1_22 1.22 ¿Cómo se identifica usted?

# La comparación con las socioeconómicas P1_*

######



# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, sf, scales, biscale, cowplot, janitor, gridExtra, ggsflabel)
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
  mutate(no_hetero = as.numeric(P1_23 %in% c(1,2,4))) %>%
  mutate(trans = as.numeric(P1_22 %in% c(3,4,5))) %>%
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

## La población total corresponde con los tabuladores Check!



## Diagnóstico general --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## La población por centro penitenciario corresponde al tabulador



## Análisis socioeconómico -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

soc %>% 
    filter(no_hetero == 1 | trans ==1) %>% 
    summarize(total_personas = survey_total(total) %>%  round())

### Cruces entre identidad y orientación ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

soc %>% 
  group_by(P1_22, SEXO) %>% 
  summarize(total_personas = survey_total(total) %>%  round())%>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre", "2"="Mujer","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                        "8" = "No entendí"))%>% 
  mutate(SEXO = recode(SEXO, "1"="Hombre", "2"="Mujer"))%>% 
  select(-total_personas_se) %>% 
  pivot_wider(P1_22, names_from = SEXO, values_from = total_personas) %>% 
  replace(is.na(.), 0)%>%
 # adorn_totals(c("col","row")) %>%  
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()%>% 
  write.csv("tablas/tabla_sexo.csv", row.names = F, na="0")



soc %>% 
  group_by(P1_22, P1_23) %>% 
  summarize(total_personas = survey_total(total) %>%  round()) %>%
  ungroup %>% 
  mutate(total_personas = round(total_personas)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre", "2"="Mujer","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                        "8" = "No entendí")) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
  select(-total_personas_se) %>% 
  pivot_wider(P1_22, names_from = P1_23, values_from = total_personas) %>% 
  replace(is.na(.), 0)%>%
  adorn_totals(c("col","row")) %>%  
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>% 
  write.csv("tablas/tabla1.csv", row.names = F, na="0")


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

trans_ent %>% arrange(-percent)

mx %>%
  left_join(trans_ent) %>% 
  mutate(percentlabel =paste0(round(percent*100, 1),"%")) %>% 
  ggplot(aes(fill=percent, label = percentlabel)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  geom_sf_text_repel(aes(label = percentlabel)) +
  labs(title = "Porcentaje de PPL transexual, transgénero o travesti",
       subtitle = "privadas de la libertad con respecto al total",
       caption = "Fuente: ENPOL 2021 - INEGI",
       x="", y="") +
  scale_fill_gradient(low='#b9fefe',high='#028686', 
                      name='% de personas', 
                      breaks = 0.004*0:5, labels = percent(0.004*0:5)) +
   hrbrthemes::theme_ipsum()#+
  # annotate(
  #   geom = "curve", size =0.8,
  #   x = -108, y = 19, 
  #   xend = -104, yend = 19, 
  #   curvature = .3, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = -112, y = 20, 
  #          label = "En Colima\n1.7% de la PPL se\nautodenomina trans", 
  #          hjust = "left") +
  # annotate(
  #   geom = "curve", size =0.8,
  #   x = -95, y = 25, 
  #   xend = -99, yend = 19.5, 
  #   curvature = .3, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = -94.5, y = 26, label = "En CDMX y EdoMex\nhay 289 personas\nautodenominadas\ncomo trans", hjust = "left")


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

no_hetero_ent %>% arrange(-percent)

mx %>%
  left_join(no_hetero_ent) %>% 
  mutate(percentlabel =paste0(round(percent*100, 1),"%")) %>% 
  ggplot(aes(fill=percent, label=percentlabel)) +
  geom_sf(color="white", size=0.8)+
  coord_sf(datum = NA)+
  geom_sf_text_repel(aes(label = percentlabel)) +
  scale_fill_gradient(low='#ffccff',high='#910092', 
                      name='% de personas', 
                      breaks = 0.025*0:6, 
                      labels = percent(0.025*0:6),
                      limit = c(0,.13)) +
  labs(title = "Porcentaje de PPL bisexual, homosexual o con otra orientación sexual",
       subtitle = "con respecto al total de población penitenciaria por estado",
       caption = "Fuente: ENPOL 2021 - INEGI",
       x="", y="") +
   hrbrthemes::theme_ipsum()#+
  # annotate(
  #   geom = "curve", size =0.8,
  #   x = -95, y = 25, 
  #   xend = -99, yend = 19, 
  #   curvature = .3, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = -94.5, y = 26, 
  #          label = "En Morelos poco más\nde 12% de la PPL\nse considera bisexual,\nhomosexual u otro.", 
  #          hjust = "left") +
  # annotate(
  #   geom = "curve", size =0.8,
  #   x = -108, y = 19, 
  #   xend = -99.5, yend = 19.5, 
  #   curvature = .3, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = -115, y = 20, label = "En CDMX y EdoMex\nhay 2,298 PPL\nautodenominadas\ncomo bisexuales,\nheterosexuales\nu otra preferencia sexual.", hjust = "left")


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
                    ylab = "Mayor % trans",
                    size = 10)

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.1, 0.3, 0.3)

ggsave("graficas/tot_edo_bivariate.png", width = 10, height = 8)



# Origen _ destino --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





# Reclusorio -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

soc %>% 
  filter(!P1_23 %in% c(8,9)) %>% 
  group_by(NOM_ENT,NOM_INT, FUERO,  no_hetero) %>% 
  summarize(total_personas = round(survey_total( 
                                          na.rm=T, 
                                          vartype = c("ci"), 
                                          level = 0.95)),
           prop_personas = survey_mean( 
              na.rm=T, 
              vartype = c("ci"), 
              level = 0.95)) %>% 
  filter(!is.na(NOM_INT))  ->X
  


X %>% 
  filter(no_hetero == 1) %>% 
  mutate(NOM_INT = str_to_title(NOM_INT)) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, "Cereso","CERESO")) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, "Cefereso","CEFERESO")) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, "Cps","CPS")) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, " De "," de ")) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, "Ii","II")) %>% 
 # mutate(NOM_INT = str_c(NOM_INT, "-", NOM_ENT)) %>% 
  ggplot(aes(fill = as.factor(FUERO), label=total_personas,
             x= total_personas, y= reorder(NOM_INT, total_personas)))+
  geom_col() +
  scale_fill_manual(name="Fuero", labels = c("Federal", "Local"),
                    values = c("#F3A200", "#009292"))+
  labs(x="Total de personas", y="",
       title = "Personas bisexuales, homosexuales o con otra preferencia sexual",
       subtitle = "por nombre y tipo de centro penitenciario",
       caption = "Fuente: ENPOL 2021 - INEGI") +
   hrbrthemes::theme_ipsum(grid ="X")+
  geom_text(hjust = -0.2) +
  theme(plot.caption = element_text(hjust = 1, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") #NEW parameter
  # geom_errorbar(aes(xmin=total_personas_low, 
  #                   xmax=total_personas_upp), width=.2,
  #               position=position_dodge(.9))


ggsave("graficas/nohet_porcentro.jpg", width = 13, height = 12)





soc %>% 
  filter(!P1_23 %in% c(8,9)) %>% 
  group_by(NOM_ENT,NOM_INT, FUERO,  trans) %>% 
  summarize(total_personas = round(survey_total( 
    na.rm=T, 
    vartype = c("ci"), 
    level = 0.95)),
    prop_personas = survey_mean( 
      na.rm=T, 
      vartype = c("ci"), 
      level = 0.95)) %>% 
  filter(!is.na(NOM_INT))  ->Y



Y %>% 
  filter(trans == 1) %>% 
  mutate(NOM_INT = str_to_title(NOM_INT)) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, "Cereso","CERESO")) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, "Cefereso","CEFERESO")) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, "Cps","CPS")) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, " De "," de ")) %>% 
  mutate(NOM_INT = str_replace(NOM_INT, "Ii","II")) %>% 
  # mutate(NOM_INT = str_c(NOM_INT, "-", NOM_ENT)) %>% 
  ggplot(aes(fill = as.factor(FUERO), label=total_personas,
             x= total_personas, y= reorder(NOM_INT, total_personas)))+
  geom_col() +
  scale_fill_manual(name="Fuero", labels = c("Federal", "Local"),
                    values = c("#F3A200", "#009292"))+
  labs(x="Total de personas", y="",
       title = "Personas transexuales, transgénero o travestis",
       subtitle = "por nombre y tipo de centro penitenciario",
       caption = "Fuente: ENPOL 2021 - INEGI") +
  hrbrthemes::theme_ipsum(grid ="X")+
  geom_text(hjust = -0.2) +
  theme(plot.caption = element_text(hjust = 1, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") 


ggsave("graficas/trans_porcentro.jpg", width = 13, height = 12)

# Tipo de penales correspondintes a identidad de género ---------------------------------------------------------------------------------------------------------------------------------------------------

soc %>% 
  filter(!P1_22 %in% c(6,8,9)) %>%
  group_by(P1_22, P1_1) %>% 
  summarize(total_personas = survey_mean( 
                                          na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95)) %>%
  ungroup %>% 
 # mutate(total_personas = round(total_personas)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                        "8" = "No entendí"))  %>% 
  mutate(P1_1 = recode(P1_1, "1"="Varonil", "2"="Femenil","3"="Mixto"))  %>% 
  mutate(P1_1 = fct_relevel(P1_1, "Femenil","Varonil","Mixto")) %>% 
  ggplot(aes(P1_1, total_personas, fill=P1_1, 
             label=str_c(round(total_personas*100,1),"%")
             )) +
  geom_col() +
  facet_wrap(~P1_22)+
 # guides(fill="none") +
  hrbrthemes::theme_ipsum(grid="Y") +
  labs(x="Tipo de centro penitenciario",
       y="Porcentaje de personas\n",
       title="Porcentaje de personas por identidad de género",
       subtitle="y tipo de centro penitenciario",
       caption = "Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.") +
  geom_errorbar(aes(ymin=total_personas_low, ymax=total_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=total_personas_upp+0.2), position = position_dodge(0.9), vjust=2,
            size=3)+
  scale_fill_manual(values = c("#11898A", "#8A2017", "#D68A04"),
                    name="Tipo de centro") +
  scale_y_continuous(labels = scales::percent)
  
ggsave("graficas/per_portipo.png", width = 8, height = 6)


soc %>% 
  filter(!P1_22 %in% c(6,8,9)) %>%
  group_by(P1_22, P1_1) %>% 
  summarize(total_personas = survey_mean(
                                          na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95)) %>%
  ungroup %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre", "2"="Mujer","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                        "8" = "No entendí"))  %>% 
  mutate(P1_1 = recode(P1_1, "1"="Varonil", "2"="Femenil","3"="Mixto"))  %>% 
  mutate(P1_1 = fct_relevel(P1_1, "Femenil","Varonil","Mixto")) %>% 
  pivot_wider(c(P1_22), names_from = P1_1, values_from = total_personas)





# Tipo de penales correspondintes a orientación sexual ---------------------------------------------------------------------------------------------------------------------------------------------------

soc %>% 
  filter(!P1_23 %in% c(6,8,9)) %>%
  group_by(P1_23, P1_1) %>% 
  summarize(total_personas = survey_mean( 
                                          na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95)) %>%
  ungroup %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
  mutate(P1_1 = recode(P1_1, "1"="Varonil", "2"="Femenil","3"="Mixto"))  %>% 
  mutate(P1_1 = fct_relevel(P1_1, "Femenil","Varonil","Mixto")) %>% 
  ggplot(aes(P1_1, total_personas, fill=P1_1, 
             label=str_c(round(total_personas*100,1),"%")
  )) +
  geom_col() +
  facet_wrap(~P1_23)+
 # guides(fill="none") +
  hrbrthemes::theme_ipsum(grid="Y") +
  labs(x="Tipo de centro penitenciario",
       y="Porcentaje de personas\n",
       title="Porcentaje de personas por orientación sexual",
       subtitle="y tipo de centro penitenciario",
       caption = "La línea negra representa el intervalo de confianza.") +
  geom_errorbar(aes(ymin=total_personas_low, ymax=total_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=total_personas_upp+0.2), position = position_dodge(0.9), vjust=2,
            size=3)+
  scale_fill_manual(values = c("#11898A", "#8A2017", "#D68A04"),
                    name="Tipo de centro") +
  scale_y_continuous(labels = scales::percent)

ggsave("graficas/orient_portipo.png", width = 8, height = 6)



## Por fueros --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

a <- soc %>% 
  filter(!P1_23 %in% c(3,6,8,9)) %>%
  group_by(P1_23, FUERO) %>% 
  summarize(total_personas = survey_total(total, 
                                          na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95) %>%  round()) %>%
  ungroup %>% 
  mutate(total_personas = round(total_personas)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%
  mutate(FUERO = recode(FUERO, "1"="Federal", "2"="Estatal")) %>% 
  filter(FUERO == "Federal") %>% 
  ggplot(aes(P1_23, total_personas, label=total_personas)) +
  geom_col()+
  geom_errorbar(aes(ymin=total_personas_low, ymax=total_personas_upp), width=.2,
                position=position_dodge(.9))+
  guides(fill="none") +
  hrbrthemes::theme_ipsum(grid="Y") +
  labs(
    #x="Tipo de centro penitenciario",
       y="Número de personas\n",
       title="Número de personas por identidad de género",
       #subtitle="y tipo de centro penitenciario",
       #caption = "La línea negra representa el intervalo de confianza."
       )+
  geom_text(vjust=-4, nudge_y = 0.5) +
  ylim(0,500)


b <- soc %>% 
  filter(!P1_23 %in% c(3,6,8,9)) %>%
  group_by(P1_23, FUERO) %>% 
  summarize(total_personas = survey_total(total, 
                                          na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95) %>%  round()) %>%
  ungroup %>% 
  mutate(total_personas = round(total_personas)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%
  mutate(FUERO = recode(FUERO, "1"="Federal", "2"="Estatal")) %>% 
  filter(FUERO == "Estatal") %>% 
  ggplot(aes(P1_23, total_personas, label=total_personas)) +
  geom_col()+
  geom_errorbar(aes(ymin=total_personas_low, ymax=total_personas_upp), width=.2,
                position=position_dodge(.9))+
  guides(fill="none") +
  hrbrthemes::theme_ipsum(grid="Y") +
  labs(x="Tipo de centro penitenciario",
       y="Número de personas\n",
       title="        ",
       subtitle="     ",
       caption = "La línea negra representa el intervalo de confianza.")+
  geom_text(vjust=-4) +
  ylim(0,8000)


grid.arrange(a, b, nrow = 1)



#Pruebas ----

soc %>% 
  group_by(P1_22, P1_1, NOM_INT) %>% 
  summarize(total_personas = survey_total(total) %>%  round()) %>%
  ungroup %>% 
  mutate(total_personas = round(total_personas)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre", "2"="Mujer","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                        "8" = "No entendí"))  %>% 
  mutate(P1_1 = recode(P1_1, "1"="Varonil", "2"="Femenil","3"="Mixto")) %>% 
  filter(P1_22 == "Hombre" & P1_1 == "Femenil")


soc %>% 
  group_by(SEXO, P1_1) %>% 
  summarize(total_personas = survey_total(total) %>%  round()) %>%
  ungroup %>% 
  mutate(total_personas = round(total_personas)) %>% 
  mutate(P1_1 = recode(P1_1, "1"="Varonil", "2"="Femenil","3"="Mixto")) %>% 
  mutate(SEXO = recode(SEXO, "1"="Hombre", "2"="Mujer"))->y





# Origen - Destino --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(networkD3)
library(viridis)

noment <- soc %>% 
  group_by(NOM_ENT, CVE_ENT) %>% 
  summarise(total_personas = survey_total(na.rm=T) %>% 
              round()) %>% select(destino=NOM_ENT, P1_5= CVE_ENT) %>% as_tibble()


sankey_comp <- soc %>%
 # mutate(no_hetero == 1) %>% 
  #mutate(trans == 1) %>% 
  rename(origen = NOM_ENT) %>% 
  group_by(origen, P1_5, trans) %>% 
  summarise(total_personas = survey_total(na.rm=T) %>% 
              round()) %>% 
  as_tibble() %>% 
  filter(trans == 1) %>%
  filter(!is.na(P1_5)) %>% 
  left_join(noment) %>% 
  select(origen, destino, total_personas) %>% 
  filter(!is.na(destino)) %>% 
  mutate(destino = str_c(destino, " "))


sankey <- sankey_comp %>% 
  filter(total_personas > 1)
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(sankey$origen), 
         as.character(sankey$destino)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
sankey$IDsource <- match(sankey$origen, nodes$name)-1 
sankey$IDtarget <- match(sankey$destino, nodes$name)-1


p <- sankeyNetwork(Links = sankey, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "total_personas", NodeID = "name", 
                   sinksRight=F)
p
