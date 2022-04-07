# Análisis de la ENPOL para Proyecto LGBTI: Tortura
# Fecha de elaboración: 2022/03/23
# Fecha de actualización: 2022/04/05
# Autor: Manuel Toral


###### Preguntas de interés

# P7_46_08 y quizás la comparación con las P7_46_*

# P1_23 1.23 De las siguientes frases, ¿cuál define su orientación sexual?

# P1_22 1.22 ¿Cómo se identifica usted?

# La comparación con las socioeconómicas P1_*

######



# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, sf, scales, biscale, cowplot, janitor, gridExtra)
extrafont::loadfonts(device = "win", quiet =T)

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/bd_enpol_2021_RData/BD_ENPOL_2021.RData")
diccionario <- read_csv("input/diccionario_de_datos_ENPOL2021_2_3.csv",
                        locale = locale(encoding="LATIN1"))

## Diseño -----------------------------------------------------------------------------------------------------------------------------------

tortura <- ENPOL2021_SOC  %>% 
  left_join(ENPOL2021_2_3 %>% 
              select(ID_PER, contains("P3_17"),contains("P3_18"))) %>% 
 # mutate(no_hetero = as.numeric(!P1_23 %in% c(3,8,9))) %>%
  #mutate(trans = as.numeric(P1_22 %in% c(3,4))) %>%
  mutate(FAC_PER = as.numeric(as.character(FAC_PER)),
         EST_DIS = as.numeric(as.character(EST_DIS)),
         FPC = as.numeric(as.character(FPC)), 
         total = 1) %>% 
  as_survey_design(strata = EST_DIS, 
                   weights = FAC_PER, 
                   fpc = FPC)




# Antes del MP: Trans -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


antes_mp_identidad <- lapply(names(ENPOL2021_2_3 %>%select(contains("P3_17"),contains("P3_18"))),
                             function(i){
  tortura %>% 
    filter(!P1_22 %in% c(6,8,9)) %>% 
    filter(!!sym(i) %in% c(1,2)) %>% 
    group_by(P1_22, !!sym(i)) %>% 
    summarize(total_personas = survey_total(na.rm=T, 
                                            vartype = c("ci","se"), 
                                            level = 0.95) %>% 
                round(),
              prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95)) %>% 
    ungroup %>% 
    as_tibble()%>% 
    #select(-contains("se"))%>% 
    group_by(P1_22) %>% 
    mutate(prop = total_personas/sum(total_personas)) %>% 
    ungroup %>% 
    filter(!!sym(i) == 1) %>% 
    mutate(variable = i) %>% 
    select(-!!sym(i))
}) %>% 
  bind_rows() #%>% 
#mutate(no_hetero = recode(no_hetero, "1"="Bisexual, Hmosexual u Otro", 
# "0"="Heterosexual")) %>% 

antes_mp_identidad <- antes_mp_identidad %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                        "8" = "No entendí")) %>% 
  left_join(diccionario %>% 
              distinct(NEMONICO, NOMBRE_CAMPO) %>% 
              select(variable = NEMONICO, NOMBRE_CAMPO)) %>% 
  mutate(clase = case_when(
    str_detect(NOMBRE_CAMPO, "físicas") ~ "Física",
    str_detect(NOMBRE_CAMPO, "psicológica") ~ "Psicológica"
  )) %>% 
  mutate(tipo = str_remove_all(
    NOMBRE_CAMPO, "Agresiones físicas antes de presentarlo con el MP o Juez de lo penal: |Violencia psicológica antes de presentarlo con el MP o Juez de lo penal: "
  )) %>% 
  select(-NOMBRE_CAMPO) %>% 
  mutate(tipo = gsub("(^[[:alpha:]])", "\\U\\1", tipo, perl=TRUE))

antes_mp_identidad %>% 
  filter(clase == "Física") %>% 
  group_by(tipo) %>% 
  mutate(max = max(prop_personas)) %>% 
  ggplot(aes(x= P1_22, 
             y=prop_personas, 
             fill=P1_22, 
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_errorbar(aes(ymin=prop_personas_low, 
                    ymax=prop_personas_upp), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~tipo, scales = "free_y", ncol=3)+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=3)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title="Identidad"))+
  scale_fill_manual(values = c("#009292","#922500","#926e00","#490092")) +
  labs(title="Porcentaje de personas que contestaron que sufrieron alguna forma de maltrato físico",
       subtitle="por tipo de maltrato e identidad de género, antes de presentarlo con el MP o Juez de lo penal",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")



 ggsave("graficas/tortura_trans_fisica.png", width = 14, height = 14)


 
 
 antes_mp_identidad %>% 
   filter(clase == "Psicológica") %>% 
   group_by(tipo) %>% 
   mutate(max = max(prop_personas)) %>% 
   ggplot(aes(x= P1_22, 
              y=prop_personas, 
              fill=P1_22, 
              label = str_c(round(prop_personas*100,1),"%"))) +
   geom_col()+
   geom_errorbar(aes(ymin=prop_personas_low, 
                     ymax=prop_personas_upp), width=.2,
                 position=position_dodge(.9)) +
   facet_wrap(~tipo, scales = "free_y", ncol=3)+
   geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
             size=3)+
   hrbrthemes::theme_ipsum(grid="") +
   scale_y_continuous(labels = scales::percent) +
   guides(fill=guide_legend(title="Identidad"))+
   scale_fill_manual(values = c("#009292","#922500","#926e00","#490092")) +
   labs(title="Porcentaje de personas que contestaron que sufrieron alguna forma de maltrato psicológico",
        subtitle="por tipo de maltrato e identidad sexual, antes de presentarlo con el MP o Juez de lo penal",
        y="", x="",
        caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")
 
 
 
 ggsave("graficas/tortura_trans_psicol.png", width = 14, height = 12)






 
 
 # Antes del MP: No Hetero -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 
 antes_mp_orientacion <- lapply(names(ENPOL2021_2_3 %>%select(contains("P3_17"),contains("P3_18"))),
                              function(i){
                                tortura %>% 
                                  filter(!P1_23 %in% c(6,8,9)) %>% 
                                  filter(!!sym(i) %in% c(1,2)) %>% 
                                  group_by(P1_23, !!sym(i)) %>% 
                                  summarize(total_personas = survey_total(na.rm=T, 
                                                                          vartype = c("ci","se"), 
                                                                          level = 0.95) %>% 
                                              round(),
                                            prop_personas = survey_mean(na.rm=T, 
                                                                        vartype = c("ci","se"), 
                                                                        level = 0.95)) %>% 
                                  ungroup %>% 
                                  as_tibble()%>% 
                                  #select(-contains("se"))%>% 
                                  group_by(P1_23) %>% 
                                  mutate(prop = total_personas/sum(total_personas)) %>% 
                                  ungroup %>% 
                                  filter(!!sym(i) == 1) %>% 
                                  mutate(variable = i) %>% 
                                  select(-!!sym(i))
                              }) %>% 
   bind_rows() #%>% 
 #mutate(no_hetero = recode(no_hetero, "1"="Bisexual, Hmosexual u Otro", 
 # "0"="Heterosexual")) %>% 
 
 antes_mp_orientacion <- antes_mp_orientacion %>% 
   mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                         "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
   left_join(diccionario %>% 
               distinct(NEMONICO, NOMBRE_CAMPO) %>% 
               select(variable = NEMONICO, NOMBRE_CAMPO)) %>% 
   mutate(clase = case_when(
     str_detect(NOMBRE_CAMPO, "físicas") ~ "Física",
     str_detect(NOMBRE_CAMPO, "psicológica") ~ "Psicológica"
   )) %>% 
   mutate(tipo = str_remove_all(
     NOMBRE_CAMPO, "Agresiones físicas antes de presentarlo con el MP o Juez de lo penal: |Violencia psicológica antes de presentarlo con el MP o Juez de lo penal: "
   )) %>% 
   select(-NOMBRE_CAMPO) %>% 
   mutate(tipo = gsub("(^[[:alpha:]])", "\\U\\1", tipo, perl=TRUE))
 
 antes_mp_orientacion %>% 
   filter(clase == "Física") %>% 
   group_by(tipo) %>% 
   mutate(max = max(prop_personas)) %>% 
   ggplot(aes(x= P1_23, 
              y=prop_personas, 
              fill=P1_23, 
              label = str_c(round(prop_personas*100,1),"%"))) +
   geom_col()+
   geom_errorbar(aes(ymin=prop_personas_low, 
                     ymax=prop_personas_upp), width=.2,
                 position=position_dodge(.9)) +
   facet_wrap(~tipo, scales = "free_y", ncol=3)+
   geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
             size=3)+
   hrbrthemes::theme_ipsum(grid="") +
   scale_y_continuous(labels = scales::percent) +
   guides(fill=guide_legend(title="Orientación"))+
   scale_fill_manual(values = c("#009292","#113DA3","#F3A200","#F36E00")) +
   labs(title="Porcentaje de personas que contestaron que sufrieron alguna forma de maltrato físico",
        subtitle="por tipo de maltrato y orientación sexual, antes de presentarlo con el MP o Juez de lo penal",
        y="", x="",
        caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")
 
 
 
 ggsave("graficas/tortura_nohet_fisica.png", width = 14, height = 14)
 
 
 
 
 antes_mp_orientacion %>% 
   filter(clase == "Psicológica") %>% 
   group_by(tipo) %>% 
   mutate(max = max(prop_personas)) %>% 
   ggplot(aes(x= P1_23, 
              y=prop_personas, 
              fill=P1_23, 
              label = str_c(round(prop_personas*100,1),"%"))) +
   geom_col()+
   geom_errorbar(aes(ymin=prop_personas_low, 
                     ymax=prop_personas_upp), width=.2,
                 position=position_dodge(.9)) +
   facet_wrap(~tipo,  ncol=3)+
   geom_text(aes(y=prop_personas_upp+0.3), position = position_dodge(0.9), vjust=2,
             size=3)+
   hrbrthemes::theme_ipsum(grid="") +
   scale_y_continuous(labels = scales::percent) +
   guides(fill=guide_legend(title="Orientación"))+
   scale_fill_manual(values = c("#009292","#113DA3","#F3A200","#F36E00")) +
   labs(title="Porcentaje de personas que contestaron que sufrieron alguna forma de maltrato psicológico",
        subtitle="por tipo de maltrato y orientación sexual, antes de presentarlo con el MP o Juez de lo penal",
        y="", x="",
        caption="Fuente: ENPOL 2021 - INEGI\nLa línea negra representa el intervalo de confianza a 95%.")
 
 
 
 ggsave("graficas/tortura_nohet_psicol.png", width = 14, height = 12)
 
 

























# patio de juegos ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tortura %>%
  group_by(P1_22) %>%
  summarize(proportion = survey_mean(),
            total = survey_total())
