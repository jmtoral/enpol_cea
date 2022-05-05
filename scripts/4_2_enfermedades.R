# Análisis de la ENPOL para Proyecto LGBTI: Salud Enfermedades
# Fecha de elaboración: 2022/04/25
# Fecha de actualización: 2022/04/25
# Autor: Manuel Toral


###### Preguntas de interés




# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, scales, janitor)
extrafont::loadfonts(device = "win", quiet =T)

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/bd_enpol_2021_RData/BD_ENPOL_2021.RData")
diccionario <- read_csv("input/diccionario_de_datos_ENPOL2021_2_3.csv",
                        locale = locale(encoding="LATIN1"))

## Diseño -----------------------------------------------------------------------------------------------------------------------------------

salud <- ENPOL2021_SOC  %>% 
  # mutate(no_hetero = as.numeric(!P1_23 %in% c(3,8,9))) %>%
  #mutate(trans = as.numeric(P1_22 %in% c(3,4))) %>%
  mutate(FAC_PER = as.numeric(as.character(FAC_PER)),
         EST_DIS = as.numeric(as.character(EST_DIS)),
         FPC = as.numeric(as.character(FPC)), 
         total = 1) %>% 
  as_survey_design(strata = EST_DIS, 
                   weights = FAC_PER, 
                   fpc = FPC)



# Enfermedades ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

enfermedades <- names(ENPOL2021_SOC %>%select(contains("P1_24_")))




enfermedades_identidad <- lapply(enfermedades, function(i){
  salud %>% 
    filter(!P1_22 %in% c(6,8,9)) %>% 
    filter(!!sym(i) %in% c(1,2)) %>% 
    group_by(P1_22, !!sym(i)) %>% 
    summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95))%>% 
    ungroup %>% 
    as_tibble() %>% 
    filter(!!sym(i) == 1) %>% 
    mutate(variable = i) %>% 
    select(-!!sym(i)) %>% 
    mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                          "4"="Hombre trans"))%>% 
    mutate(variable = recode(variable, "P1_24_1"= "Diabetes" ,"P1_24_2"= "Hipertensión",
                             "P1_24_3"="Cáncer", "P1_24_4"="Bronquitis o neumonía","P1_24_5"="Tuberculosis",
                             "P1_24_6"="Hepatitis (cualquiera)", "P1_24_7"="VIH (SIDA)", "P1_24_8"="COVID 19",
                             "P1_24_9"="Otra"))
}) %>% 
  bind_rows() 


enfermedades_identidad %>% 
 # filter(!variable == "Otros") %>% 
  ggplot(aes(x= P1_22, 
             y=prop_personas, 
             fill=P1_22, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~variable, ncol=3, scales = "free_x")+
  geom_text(aes(y=prop_personas_upp+0.05), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  #guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que tenían alguna enfermedad",
       subtitle="por enfermedad e identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")



# Medicamentos ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

medicamentos<- names(ENPOL2021_SOC %>%select(contains("P1_26_")))

`%!in%` = Negate(`%in%`)


salud %>% 
  group_by(P1_22, P1_26_1, P1_28_1) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci","se"), 
                                        level = 0.95))->x


medicamentos_identidad <- lapply(as.character(1:9), function(i){
  salud %>% 
    filter(!!sym(str_c("P1_26_", i)) %in% c(1,2)) %>% 
    filter(!!sym(str_c("P1_28_", i)) %in% c(1,2,3, NA_character_)) %>%
    filter(!P1_22 %in% c(6,8,9)) %>% 
    group_by(P1_22, !!sym(str_c("P1_26_", i))) %>% 
    summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95))%>% 
    ungroup %>% 
    as_tibble() %>% 
    filter(!!sym(str_c("P1_26_", i)) == 2) %>% 
    mutate(variable = str_c("P1_26_", i)) %>% 
    mutate(respuesta = !!sym(str_c("P1_26_", i))) %>% 
    select(-!!sym(str_c("P1_26_", i))) %>% 
    mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                          "4"="Hombre trans"))%>% 
    mutate(variable = recode(variable, "P1_26_1"= "Diabetes" ,"P1_26_2"= "Hipertensión",
                             "P1_26_3"="Cáncer", "P1_26_4"="Bronquitis o neumonía","P1_26_5"="Tuberculosis",
                             "P1_26_6"="Hepatitis (cualquiera)", "P1_26_7"="VIH (SIDA)", "P1_26_8"="COVID 19",
                             "P1_26_9"="Otra"))
}) %>% 
  bind_rows() 


medicamentos_identidad %>% 
  # filter(!variable == "Otros") %>% 
  ggplot(aes(x= P1_22, 
             y=prop_personas, 
             fill=P1_22, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~variable, ncol=3, scales = "free_x")+
  geom_text(aes(y=prop_personas_upp+0.2), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  #guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que no reciben medicamento para la enfermedad que padece",
       subtitle="por enfermedad e identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")
