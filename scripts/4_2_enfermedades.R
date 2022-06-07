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

# Nohet-------

## Enfermedades ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

enfermedades <- names(ENPOL2021_SOC %>%select(contains("P1_24_")))




enfermedades_identidad <- lapply(enfermedades, function(i){
  salud %>% 
    filter(!P1_23 %in% c(4,5,6,8,9)) %>% 
    filter(!!sym(i) %in% c(1,2)) %>% 
    group_by(P1_23, !!sym(i)) %>% 
    summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95))%>% 
    ungroup %>% 
    as_tibble() %>% 
    filter(!!sym(i) == 1) %>% 
    mutate(variable = i) %>% 
    select(-!!sym(i)) %>% 
    mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                          "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
    mutate(variable = recode(variable, "P1_24_1"= "Diabetes" ,"P1_24_2"= "Hipertensión",
                             "P1_24_3"="Cáncer", "P1_24_4"="Bronquitis o neumonía","P1_24_5"="Tuberculosis",
                             "P1_24_6"="Hepatitis (cualquiera)", "P1_24_7"="VIH (SIDA)", "P1_24_8"="COVID 19",
                             "P1_24_9"="Otra"))
}) %>% 
  bind_rows() 


enfermedades_identidad %>% 
 # filter(!variable == "Otros") %>% 
  ggplot(aes(x= P1_23, 
             y=prop_personas, 
             fill=P1_23, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
 #geom_errorbar(aes(ymin=prop_personas_low, 
  #                  ymax=prop_personas_upp), width=.2,
   #             position=position_dodge(.9)) +
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
       subtitle="por enfermedad e orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\n")


ggsave("graficas/4_salud_enfermedades_nohet.png", width = 12, height = 8)

## Medicamentos ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

medicamentos<- names(ENPOL2021_SOC %>%select(contains("P1_26_")))

`%!in%` = Negate(`%in%`)


salud %>% 
  group_by(P1_23, P1_26_1, P1_28_1) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci","se"), 
                                        level = 0.95))->x


medicamentos_orientacion <- lapply(as.character(1:9), function(i){
  salud %>% 
    filter(!!sym(str_c("P1_26_", i)) %in% c(1,2)) %>% 
    filter(!!sym(str_c("P1_28_", i)) %in% c(1,2,3, NA_character_)) %>%
    filter(!P1_23 %in% c(4,6,8,9)) %>% 
    group_by(P1_23, !!sym(str_c("P1_26_", i))) %>% 
    summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95))%>% 
    ungroup %>% 
    as_tibble() %>% 
    filter(!!sym(str_c("P1_26_", i)) == 2) %>% 
    mutate(variable = str_c("P1_26_", i)) %>% 
    mutate(respuesta = !!sym(str_c("P1_26_", i))) %>% 
    select(-!!sym(str_c("P1_26_", i))) %>% 
    mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                          "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
    mutate(variable = recode(variable, "P1_26_1"= "Diabetes" ,"P1_26_2"= "Hipertensión",
                             "P1_26_3"="Cáncer", "P1_26_4"="Bronquitis o neumonía","P1_26_5"="Tuberculosis",
                             "P1_26_6"="Hepatitis (cualquiera)", "P1_26_7"="VIH (SIDA)", "P1_26_8"="COVID 19",
                             "P1_26_9"="Otra"))
}) %>% 
  bind_rows() 


medicamentos_orientacion %>% 
  # filter(!variable == "Otros") %>% 
  ggplot(aes(x= P1_23, 
             y=prop_personas, 
             fill=P1_23, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  #geom_errorbar(aes(ymin=prop_personas_low, 
   #                 ymax=prop_personas_upp), width=.2,
    #            position=position_dodge(.9)) +
  facet_wrap(~variable, ncol=3, scales = "free_x")+
  geom_text(aes(y=prop_personas_upp+0.2), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  #guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que no reciben medicamento",
       subtitle="por falta de recursos u obstáculos de las autoridades, por enfermedad e orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\n")


ggsave("graficas/4_salud_medicamentos_nohet.png", width = 12, height = 8)




# Trans -------

## Enfermedades ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

enfermedades <- names(ENPOL2021_SOC %>%select(contains("P1_24_")))




enfermedades_identidad <- lapply(enfermedades, function(i){
  salud %>% 
    filter(!P1_22 %in% c(4,5,6,8,9)) %>% 
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
  #geom_errorbar(aes(ymin=prop_personas_low, 
  #                  ymax=prop_personas_upp), width=.2,
  #             position=position_dodge(.9)) +
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
       subtitle="por enfermedad e identidad de género",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\n")


ggsave("graficas/4_salud_enfermedades_trans.png", width = 12, height = 8)

## Medicamentos ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
    filter(!P1_22 %in% c(4,6,8,9)) %>% 
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
  #geom_errorbar(aes(ymin=prop_personas_low, 
  #                 ymax=prop_personas_upp), width=.2,
  #            position=position_dodge(.9)) +
  facet_wrap(~variable, ncol=3, scales = "free_x")+
  geom_text(aes(y=prop_personas_upp+0.2), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,.2)) +
  #guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
  guides(fill="none")+
  labs(title="Porcentaje de personas que no reciben medicamento",
       subtitle="por falta de recursos u obstáculos de las autoridades, por enfermedad e identidad de género",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\n")


ggsave("graficas/4_salud_medicamentos_trans.png", width = 12, height = 8)



# Servicios ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


serv <- ENPOL2021_SOC  %>% 
  left_join(ENPOL2021_6) %>%
  # mutate(no_hetero = as.numeric(!P1_23 %in% c(3,8,9))) %>%
  #mutate(trans = as.numeric(P1_22 %in% c(3,4))) %>%
  mutate(FAC_PER = as.numeric(as.character(FAC_PER)),
         EST_DIS = as.numeric(as.character(EST_DIS)),
         FPC = as.numeric(as.character(FPC)), 
         total = 1) %>% 
  as_survey_design(strata = EST_DIS, 
                   weights = FAC_PER, 
                   fpc = FPC)

diccionario6 <- read_csv("input/diccionario_de_datos_ENPOL2021_6.csv",
                          locale = locale(encoding="LATIN1"))



servsal <- names(ENPOL2021_6 %>%select(contains("P6_10_0")))[c(1:4,8)]


## Identidad -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



serv_identidad <- lapply(servsal, function(i){
  serv %>% 
    filter(!P1_22 %in% c(5, 6,8,9)) %>% 
    filter(!!sym(i) %in% c(1,2)) %>% 
    group_by(P1_22, !!sym(i)) %>% 
    summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95))%>% 
    ungroup %>% 
    as_tibble() %>% 
    #filter(!!sym(i) == 1) %>% 
    mutate(variable = i) %>% 
    mutate(respuesta = !!sym(i)) %>% 
    select(-!!sym(i)) %>% 
    mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                          "4"="Hombre trans"))
}) %>% 
  bind_rows() 


serv_identidad <- serv_identidad |> 
  left_join(diccionario6 %>% 
              distinct(NEMONICO, NOMBRE_CAMPO) %>% 
              select(variable = NEMONICO, NOMBRE_CAMPO)) |> 
  mutate(NOMBRE_CAMPO = str_remove_all(NOMBRE_CAMPO, 
                                       "Bienes y servicios proporcionados por el Centro: ")) |> 
  filter(respuesta==2)



serv_identidad |> 
  ggplot(aes(P1_22, prop_personas, fill= P1_22,
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  facet_wrap(~NOMBRE_CAMPO, scales = "free_y", ncol=2)+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")+
  #scale_fill_manual(values = c("#009292","#922500","#926e00","#490092")) +
  labs(title="Porcentaje de personas que NO recibieron y servicios proporcionados por el Centro",
       subtitle="con respecto a tipo servicio e identidad de género",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLos porcentajes no suman 100% porque se comparan con el total por identidad de género.")

ggsave("graficas/4serv_orientacion.png", width = 12, height = 10)



## Orientación -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



serv_orientacion <- lapply(servsal, function(i){
  serv %>% 
    filter(!P1_23 %in% c(4,5, 6,8,9)) %>% 
    filter(!!sym(i) %in% c(1,2)) %>% 
    group_by(P1_23, !!sym(i)) %>% 
    summarise(prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95))%>% 
    ungroup %>% 
    as_tibble() %>% 
    #filter(!!sym(i) == 1) %>% 
    mutate(variable = i) %>% 
    mutate(respuesta = !!sym(i)) %>% 
    select(-!!sym(i)) %>% 
    mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                          "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) 
}) %>% 
  bind_rows() 


serv_orientacion <- serv_orientacion |> 
  left_join(diccionario6 %>% 
              distinct(NEMONICO, NOMBRE_CAMPO) %>% 
              select(variable = NEMONICO, NOMBRE_CAMPO)) |> 
  mutate(NOMBRE_CAMPO = str_remove_all(NOMBRE_CAMPO, 
                                       "Bienes y servicios proporcionados por el Centro: ")) |> 
  filter(respuesta==2)



serv_orientacion |> 
  ggplot(aes(P1_23, prop_personas, fill= P1_23,
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  facet_wrap(~NOMBRE_CAMPO, scales = "free_y", ncol=2)+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent)+
  guides(fill="none")+
 # scale_fill_manual(values = c("#009292","#922500","#926e00","#490092")) +
  labs(title="Porcentaje de personas que NO recibieron y servicios proporcionados por el Centro",
       subtitle="con respecto a tipo servicio e identidad de género",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLos porcentajes no suman 100% porque se comparan con el total por identidad de género.")

ggsave("graficas/4serv_identidad.png", width = 12, height = 10)
