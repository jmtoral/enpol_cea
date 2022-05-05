# Análisis de la ENPOL para Proyecto LGBTI: Reincersión
# Fecha de elaboración: 2022/04/21
# Fecha de actualización: 2022/04/21
# Autor: Manuel Toral


###### Preguntas de interés




# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, sf, scales, biscale, cowplot, janitor)
extrafont::loadfonts(device = "win", quiet =T)

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/bd_enpol_2021_RData/BD_ENPOL_2021.RData")
diccionario <- read_csv("input/diccionario_de_datos_ENPOL2021_2_3.csv",
                        locale = locale(encoding="LATIN1"))

## Diseño -----------------------------------------------------------------------------------------------------------------------------------

reint <- ENPOL2021_SOC  %>% 
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


# Herramientas necesarias para reincorporarse a la vida social --------------------------------------------------------------------------------------------------------------------------------

reint %>% 
  filter(!P1_22 %in% c(6,8,9)) %>%
  filter(!P10_6 %in% c(6,8,9)) %>%
  group_by(P1_22,P10_6) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra")) %>% 
  mutate(P10_6 = recode(P10_6, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_22, prop_personas,
             fill=P10_6,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#4D6075","#DB6723")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  labs(title="Porcentaje de personas que contestaron que considera que el Centro penitenciario le ha dado las herramientas necesarias para reincorporarse a la vida social",
       subtitle="por tipo de identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/5_opinionherramientas_identidad.png", width = 10, height = 8)




reint %>% 
  filter(!P1_23 %in% c(6,8,9)) %>%
  filter(!P10_6 %in% c(6,8,9)) %>%
  group_by(P1_23,P10_6) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
  mutate(P10_6 = recode(P10_6, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_23, prop_personas,
             fill=P10_6,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#4D6075","#DB6723")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  labs(title="Porcentaje de personas que contestaron que considera que el Centro penitenciario le ha dado las herramientas necesarias para reincorporarse a la vida social",
       subtitle="por tipo de orientación",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/5_opinionherramientas_orientacion.png", width = 10, height = 8)



# Lugar temporal ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


reint %>% 
  filter(!P1_22 %in% c(6,8,9)) %>%
  filter(!P10_2 %in% c(6,8,9)) %>%
  group_by(P1_22,P10_2) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra")) %>% 
  mutate(P10_2 = recode(P10_2, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_22, prop_personas,
             fill=P10_2,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#DB6723","#A52FF5")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  labs(title="Porcentaje de personas que contestaron que considera que el tendría hogar temporal a su salida",
       subtitle="por tipo de identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/5_hogartemporal_identidad.png", width = 10, height = 8)




reint %>% 
  filter(!P1_23 %in% c(6,8,9)) %>%
  filter(!P10_2 %in% c(6,8,9)) %>%
  group_by(P1_23,P10_2) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
  mutate(P10_2 = recode(P10_2, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_23, prop_personas,
             fill=P10_2,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#DB6723", "#A52FF5")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  labs(title="Porcentaje de personas que contestaron q considera que el Centro penitenciario le ha dado las herramientas necesarias para reincorporarse a la vida social",
       subtitle="por tipo de orientación",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/5_hogartemporal_orientacion.png", width = 10, height = 8)




