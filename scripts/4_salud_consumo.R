# Análisis de la ENPOL para Proyecto LGBTI: Salud
# Fecha de elaboración: 2022/04/20
# Fecha de actualización: 2022/04/20
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


# Suicidio ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Trans

salud %>% 
  filter(!P1_29 %in% c(6,8,9)) %>%
  filter(!P1_22 %in% c(6,8,9)) %>%
  group_by(P1_22, P1_29) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra")) %>% 
  mutate(P1_29 = recode(P1_29, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_29, prop_personas,
             fill=P1_29,
             label = str_c(round(prop_personas*100,1),"%")),
             ) +
  geom_col() +
  facet_wrap(~P1_22)+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#4D6075","#DB6723")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que contestaron que pensaron en quitarse o no la vida",
       subtitle="por tipo de identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/4_salud_pensamsuic.png", width = 10, height = 8)




salud %>% 
  filter(!P1_30 %in% c(6,8,9)) %>%
  filter(!P1_22 %in% c(6,8,9)) %>%
  filter(!is.na(P1_30)) %>% 
  group_by(P1_22, P1_30) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra")) %>% 
  mutate(P1_30 = recode(P1_30, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_30, prop_personas,
             fill=P1_30,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col() +
  facet_wrap(~P1_22)+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#4D6075","#DB6723")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que contestaron que intentaron o no quitarse la vida",
       subtitle="por tipo de identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/4_salud_intentosuic.png", width = 10, height = 8)




## LBI
salud %>% 
  filter(!P1_29 %in% c(6,8,9)) %>%
  filter(!P1_23 %in% c(6,8,9)) %>%
  group_by(P1_23, P1_29) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%  
  mutate(P1_29 = recode(P1_29, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_29, prop_personas,
             fill=P1_29,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col() +
  facet_wrap(~P1_23)+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#4D6075","#DB6723")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que contestaron que pensaron o no en quitarse la vida",
       subtitle="por tipo de orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/4_salud_pensamsuic_orientacion.png", width = 10, height = 8)




salud %>% 
  filter(!P1_30 %in% c(6,8,9)) %>%
  filter(!P1_23 %in% c(6,8,9)) %>%
  filter(!is.na(P1_30)) %>% 
  group_by(P1_23, P1_30) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%
  mutate(P1_30 = recode(P1_30, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_30, prop_personas,
             fill=P1_30,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col() +
  facet_wrap(~P1_23)+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#4D6075","#DB6723")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que contestaron que intentaron o no quitarse la vida",
       subtitle="por tipo de orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/4_salud_intentosuic_orientacion.png", width = 10, height = 8)



## Examen médico al llegar---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


salud %>% 
  filter(!P1_32 %in% c(6,8,9)) %>%
  filter(!P1_22 %in% c(6,8,9)) %>%
  group_by(P1_22, P1_32) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra")) %>% 
  mutate(P1_32 = recode(P1_32, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_32, prop_personas,
             fill=P1_32,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col() +
  facet_wrap(~P1_22)+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#009292","#DB6723")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que respondieron si se les practicó un examen médico o no",
       subtitle="por tipo de identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/4_salud_examenmedico.png", width = 10, height = 8)


salud %>% 
  filter(!P1_32 %in% c(6,8,9)) %>%
  filter(!P1_23 %in% c(6,8,9)) %>%
  group_by(P1_23, P1_32) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%
  mutate(P1_32 = recode(P1_32, "1"="Sí", "2"="No")) %>%
  ungroup %>% 
  ggplot(aes(P1_32, prop_personas,
             fill=P1_32,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col() +
  facet_wrap(~P1_23)+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#009292","#DB6723")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que respondieron si se les practicó un examen médico o no",
       subtitle="por tipo de identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/4_salud_examenmedico_orientacion.png", width = 10, height = 8)


## Examen médico constante---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


salud %>% 
  filter(!P1_33_1 %in% c(6,8,9)) %>%
  filter(!P1_22 %in% c(6,8,9)) %>%
  group_by(P1_22, P1_33_1) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra")) %>% 
  mutate(P1_33_1 = recode(P1_33_1, "1"="Sí", "2"="No")) %>% 
  ungroup %>% 
  ggplot(aes(P1_33_1, prop_personas,
             fill=P1_33_1,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col() +
  facet_wrap(~P1_22)+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#DB6723","#31A8A8")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que respondieron si se les practicó un examen médio o no peridódicamente",
       subtitle="por tipo de identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/4_salud_examenmedicoper.png", width = 10, height = 8)


salud %>% 
  filter(!P1_33_1 %in% c(6,8,9)) %>%
  filter(!P1_23 %in% c(6,8,9)) %>%
  group_by(P1_23, P1_33_1) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%
  mutate(P1_33_1 = recode(P1_33_1, "1"="Sí", "2"="No")) %>%
  ungroup %>% 
  ggplot(aes(P1_33_1, prop_personas,
             fill=P1_33_1,
             label = str_c(round(prop_personas*100,1),"%")),
  ) +
  geom_col() +
  facet_wrap(~P1_23)+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="Y") +
  scale_fill_manual(values = c("#DB6723","#31A8A8")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que respondieron si se les practicó un examen médio o no periódicamente",
       subtitle="por tipo de identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")

ggsave("graficas/4_salud_examenmedicoper_orientacion.png", width = 10, height = 8)




# Drogas ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

primconsumo <- names(ENPOL2021_SOC %>%select(contains("P1_41_")))



drogas_identidad <- lapply(primconsumo, function(i){
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
    select(-!!sym(i))
})  %>% 
  bind_rows() 

drogas_identidad <-  drogas_identidad %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans")) %>% 
  mutate(variable = recode(variable, "P1_41_1"= "Tabaco" ,"P1_41_2"= "Alcohol",
                           "P1_41_3"="Marihuana", "P1_41_4"="Inhalables","P1_41_5"="LSD",
                           "P1_41_6"="Hongos", "P1_41_7"="Cocaína", "P1_41_8"="Pasta base",
                           "P1_41_9"="Crack", "P1_41_10"="Heroína", "P1_41_11"="Opioides",
                           "P1_41_12"="Antidepresivos","P1_41_13"="Anfetaminas y metanfetamienas",
                           "P1_41_14"="Otros"))


drogas_identidad %>% 
  filter(!variable == "Otros") %>% 
  ggplot(aes(x= P1_22, 
             y=prop_personas, 
             fill=P1_22, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~variable, ncol=3, scales = "free_x")+
  geom_text(aes(y=prop_personas_upp+0.3), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  #guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que contestaron que habían consumido alguna droga alguna vez en su vida",
       subtitle="por tipo de droga e identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")


ggsave("graficas/4_salud_drogas_identidad.png", width = 12, height = 12)






drogas_orientacion <- lapply(primconsumo, function(i){
  salud %>% 
    filter(!P1_23 %in% c(6,8,9)) %>% 
    filter(!!sym(i) %in% c(1,2)) %>% 
    group_by(P1_23, !!sym(i)) %>% 
    summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95))%>% 
    ungroup %>% 
    as_tibble() %>% 
    filter(!!sym(i) == 1) %>% 
    mutate(variable = i) %>% 
    select(-!!sym(i))
})  %>% 
  bind_rows() 

drogas_orientacion <-  drogas %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra")) %>% 
  mutate(variable = recode(variable, "P1_41_1"= "Tabaco" ,"P1_41_2"= "Alcohol",
                           "P1_41_3"="Marihuana", "P1_41_4"="Inhalables","P1_41_5"="LSD",
                           "P1_41_6"="Hongos", "P1_41_7"="Cocaína", "P1_41_8"="Pasta base",
                           "P1_41_9"="Crack", "P1_41_10"="Heroína", "P1_41_11"="Opioides",
                           "P1_41_12"="Antidepresivos","P1_41_13"="Anfetaminas y metanfetamienas",
                           "P1_41_14"="Otros"))




drogas_orientacion %>% 
  filter(!variable == "Otros") %>% 
  ggplot(aes(x= P1_23, 
             y=prop_personas, 
             fill=P1_23, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~variable, ncol=3, scales = "free_x")+
  geom_text(aes(y=prop_personas_upp+0.3), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  #guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que contestaron que habían consumido alguna droga alguna vez en su vida",
       subtitle="por tipo de droga y orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")


ggsave("graficas/4_salud_drogas_orientacion.png", width = 12, height = 12)




# Drogas últimos 30 días------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

mesconsumo <- names(ENPOL2021_SOC %>%select(contains("P1_44_")))




drogas_identidad <- lapply(mesconsumo, function(i){
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
    #elect(-!!sym(i)) %>% 
    mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                          "4"="Hombre trans"))%>% 
    mutate(variable = recode(variable, "P1_44_1"= "Tabaco" ,"P1_44_2"= "Alcohol",
                             "P1_44_3"="Marihuana", "P1_44_4"="Inhalables","P1_44_5"="LSD",
                             "P1_44_6"="Hongos", "P1_44_7"="Cocaína", "P1_44_8"="Pasta base",
                             "P1_44_9"="Crack", "P1_44_10"="Heroína", "P1_44_11"="Opioides",
                             "P1_44_12"="Antidepresivos","P1_44_13"="Anfetaminas y metanfetamienas",
                             "P1_44_14"="Otros"))
}) %>% 
   bind_rows() 

drogas_identidad <-  drogas_identidad %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans")) %>% 
  mutate(variable = recode(variable, "P1_41_1"= "Tabaco" ,"P1_41_2"= "Alcohol",
                           "P1_41_3"="Marihuana", "P1_41_4"="Inhalables","P1_41_5"="LSD",
                           "P1_41_6"="Hongos", "P1_41_7"="Cocaína", "P1_41_8"="Pasta base",
                           "P1_41_9"="Crack", "P1_41_10"="Heroína", "P1_41_11"="Opioides",
                           "P1_41_12"="Antidepresivos","P1_41_13"="Anfetaminas y metanfetamienas",
                           "P1_41_14"="Otros"))


drogas_identidad %>% 
  filter(!variable == "Otros") %>% 
  ggplot(aes(x= P1_22, 
             y=prop_personas, 
             fill=P1_22, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~variable, ncol=3, scales = "free_x")+
  geom_text(aes(y=prop_personas_upp+0.3), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  #guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que contestaron que habían consumido alguna droga alguna vez en su vida",
       subtitle="por tipo de droga e identidad sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")


ggsave("graficas/4_salud_drogas_identidad.png", width = 12, height = 12)






drogas_orientacion <- lapply(mesconsumo, function(i){
  salud %>% 
    filter(!P1_23 %in% c(6,8,9)) %>% 
    filter(!!sym(i) %in% c(1,2)) %>% 
    group_by(P1_23, !!sym(i)) %>% 
    summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95))%>% 
    ungroup %>% 
    as_tibble() %>% 
    filter(!!sym(i) == 1) %>% 
    mutate(variable = i) %>% 
    select(-!!sym(i))
})  %>% 
  bind_rows() 

drogas_orientacion <-  drogas %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra")) %>% 
  mutate(variable = recode(variable, "P1_44_1"= "Tabaco" ,"P1_44_2"= "Alcohol",
                           "P1_44_3"="Marihuana", "P1_44_4"="Inhalables","P1_44_5"="LSD",
                           "P1_44_6"="Hongos", "P1_44_7"="Cocaína", "P1_44_8"="Pasta base",
                           "P1_44_9"="Crack", "P1_44_10"="Heroína", "P1_44_11"="Opioides",
                           "P1_44_12"="Antidepresivos","P1_44_13"="Anfetaminas y metanfetamienas",
                           "P1_44_14"="Otros"))




drogas_orientacion %>% 
  filter(!variable == "Otros") %>% 
  ggplot(aes(x= P1_23, 
             y=prop_personas, 
             fill=P1_23, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~variable, ncol=3, scales = "free_x")+
  geom_text(aes(y=prop_personas_upp+0.3), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  #guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que contestaron que habían consumido alguna droga alguna vez en su vida",
       subtitle="por tipo de droga y orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")


ggsave("graficas/4_salud_drogas_orientacion.png", width = 12, height = 12)


