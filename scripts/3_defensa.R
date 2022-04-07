# Análisis de la ENPOL para Proyecto LGBTI: Defensa
# Fecha de elaboración: 2022/04/05
# Fecha de actualización: 2022/04/05
# Autor: Manuel Toral


###### Preguntas de interés

# P7_46_08 y quizás la comparación con las P7_46_*

# P1_23 1.23 De las siguientes frases, ¿cuál define su orientación sexual?

# P1_22 1.22 ¿Cómo se identifica usted?


######



# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, sf, scales, biscale, cowplot, janitor, gridExtra)
extrafont::loadfonts(device = "win", quiet =T)

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/bd_enpol_2021_RData/BD_ENPOL_2021.RData")
diccionario <- read_csv("input/diccionario_de_datos_ENPOL2021_2_3.csv",
                        locale = locale(encoding="LATIN1"))

## Diseño -----------------------------------------------------------------------------------------------------------------------------------

judicial <- ENPOL2021_SOC  %>% 
  left_join(ENPOL2021_5) %>% 
  # mutate(no_hetero = as.numeric(!P1_23 %in% c(3,8,9))) %>%
  #mutate(trans = as.numeric(P1_22 %in% c(3,4))) %>%
  mutate(FAC_PER = as.numeric(as.character(FAC_PER)),
         EST_DIS = as.numeric(as.character(EST_DIS)),
         FPC = as.numeric(as.character(FPC)), 
         total = 1) %>% 
  as_survey_design(strata = EST_DIS, 
                   weights = FAC_PER, 
                   fpc = FPC)



# Sentencia? --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Trans
judicial %>% 
  filter(!P1_22 %in% c(6,8,9)) %>% 
  filter(!P5_3 %in% c(8,9)) %>%
  group_by(P1_22, P5_3) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci"), 
                                          level = 0.95)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra"))%>% 
  mutate(P5_3 = recode(P5_3, "1"="Sin sentencia", "2"="Sentencia por algunos delitos",
                       "3"="Sentencia para todos los delitos",
                        "4"="Hombre trans", "5"="Otra")) %>% 
  ggplot(aes(x=P5_3, y=prop_personas,
             fill=P1_22)) +
  geom_col(position = "dodge")
