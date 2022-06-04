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
diccionario2 <- read_csv("input/diccionario_de_datos_ENPOL2021_7.csv",
                        locale = locale(encoding="LATIN1"))
## Diseño -----------------------------------------------------------------------------------------------------------------------------------

tortura <- ENPOL2021_SOC  %>% 
  left_join(ENPOL2021_2_3 %>% 
              select(ID_PER, contains("P3_17"),contains("P3_18"))) %>% 
  left_join(ENPOL2021_7 %>% 
              select(ID_PER, contains("P7_40"),contains("P7_49"))) %>% 
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
 # geom_errorbar(aes(ymin=prop_personas_low, 
  #                  ymax=prop_personas_upp), width=.2,
   #             position=position_dodge(.9)) +
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
  # geom_errorbar(aes(ymin=prop_personas_low, 
   #                  ymax=prop_personas_upp), width=.2,
    #             position=position_dodge(.9)) +
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
  # geom_errorbar(aes(ymin=prop_personas_low, 
   #                  ymax=prop_personas_upp), width=.2,
    #             position=position_dodge(.9)) +
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
   #geom_errorbar(aes(ymin=prop_personas_low, 
    #                 ymax=prop_personas_upp), width=.2,
     #            position=position_dodge(.9)) +
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
        caption="Fuente: ENPOL 2021 - INEGI")
 
 
 
 ggsave("graficas/tortura_nohet_psicol.png", width = 14, height = 12)
 
 













 
 
 
 # En la cárcel: Trans -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 
reclusion_identidad <- lapply(names(ENPOL2021_7 %>%select(contains("P7_40"))),
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
 
 reclusion_identidad <- reclusion_identidad %>% 
   mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                         "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                         "8" = "No entendí")) %>% 
   left_join(diccionario2 %>% 
               distinct(NEMONICO, NOMBRE_CAMPO) %>% 
               select(variable = NEMONICO, NOMBRE_CAMPO)) %>% 
   mutate(tipo = str_remove_all(
     NOMBRE_CAMPO, "Victimización en el centro penitenciario: "
   )) %>% 
   select(-NOMBRE_CAMPO) %>% 
   mutate(tipo = gsub("(^[[:alpha:]])", "\\U\\1", tipo, perl=TRUE))
 
 reclusion_identidad %>% 
   group_by(tipo) %>% 
   mutate(max = max(prop_personas)) %>% 
   ggplot(aes(x= P1_22, 
              y=prop_personas, 
              fill=P1_22, 
              label = str_c(round(prop_personas*100,1),"%"))) +
   geom_col()+
   # geom_errorbar(aes(ymin=prop_personas_low, 
   #                  ymax=prop_personas_upp), width=.2,
   #             position=position_dodge(.9)) +
   facet_wrap(~tipo,  ncol=3)+
   geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
             size=5)+
   hrbrthemes::theme_ipsum(grid="") +
   scale_y_continuous(labels = scales::percent) +
   guides(fill="none")+
   scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
   labs(title="Porcentaje de personas que sufrieron alguna victimización en el centro penitenciario",
        subtitle="por tipo de victimización e identidad de género, los últimos 12 meses dentro del Centro Penitenciario",
        y="", x="",
        caption="Fuente: ENPOL 2021 - INEGI")
 
 
 
 ggsave("graficas/2_tortura_trans_fisica_centro.png", width = 14, height = 10)
 
 
 
 
 
 
 # [Responsables] en la cárcel: Trans -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 x <- paste0("P7_42_", 1:6, "_")

 responsables <- as.vector(outer(x, 1:3, paste, sep=""))

resp_design <- ENPOL2021_SOC  %>% 
  left_join(ENPOL2021_7 %>% 
              select(ID_PER, responsables, contains("P7_40"))) %>%
  mutate(FAC_PER = as.numeric(as.character(FAC_PER)),
         EST_DIS = as.numeric(as.character(EST_DIS)),
         FPC = as.numeric(as.character(FPC)), 
         total = 1)%>% 
  as_survey_design(strata = EST_DIS, 
                   weights = FAC_PER, 
                   fpc = FPC)

resp_design %>% 
  filter(P7_40_2 == 1) %>% 
  filter(P1_22 %in% c(3,4)) %>% 
  select(P1_22, 
         P7_42_2_1, 
         P7_42_2_2, 
         P7_42_2_3)%>% 
  group_by(P1_22, 
           P7_42_2_1,
           P7_42_2_2,
           P7_42_2_3) %>% 
  summarize(total_personas = survey_total(na.rm=T),
            prob_personas = survey_mean(na.rm=T)) %>% 
  mutate(total_personas = round(total_personas))->x




internos <- lapply(1:6, function(i){
  resp_design %>% 
    filter(!!sym(paste0("P7_40_",i))==1) %>% 
    filter(P1_22 %in% c(1,2,3,4)) %>% 
    select(P1_22, 
           !!sym(paste0("P7_40_",i)), 
           !!sym(paste0("P7_42_",i, "_1"))) %>% 
    group_by(P1_22, !!sym(paste0("P7_42_",i, "_1"))) %>% 
    summarize(total_personas = survey_total(na.rm=T),
              prop_personas = survey_mean(na.rm=T, 
                                            vartype = c("ci","se"), 
                                            level = 0.95)) %>% 
    filter(!!sym(paste0("P7_42_",i, "_1")) == 1) %>% 
    select(-!!sym(paste0("P7_42_",i, "_1"))) %>% 
    ungroup %>% 
    as_tibble() %>% 
    mutate(origen = "Internos(as)") %>% 
    mutate(variable = paste0("P7_40_",i))
}) %>% 
  bind_rows()




custodios <- lapply(1:6, function(i){
  resp_design %>% 
    filter(!!sym(paste0("P7_40_",i))==1) %>% 
    filter(P1_22 %in% c(1,2,3,4)) %>% 
    select(P1_22, 
           !!sym(paste0("P7_40_",i)), 
           !!sym(paste0("P7_42_",i, "_2"))) %>% 
    group_by(P1_22, !!sym(paste0("P7_42_",i, "_2"))) %>% 
    summarize(total_personas = survey_total(na.rm=T),
              prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95)) %>% 
    filter(!!sym(paste0("P7_42_",i, "_2")) == 1) %>% 
    select(-!!sym(paste0("P7_42_",i, "_2"))) %>% 
    ungroup %>% 
    as_tibble() %>% 
    mutate(origen = "Custodios(as)") %>% 
    mutate(variable = paste0("P7_40_",i))
}) %>% 
  bind_rows()

  

tecnico <- lapply(1:6, function(i){
  resp_design %>% 
    filter(!!sym(paste0("P7_40_",i))==1) %>% 
    filter(P1_22 %in% c(1,2,3,4)) %>% 
    select(P1_22, 
           !!sym(paste0("P7_40_",i)), 
           !!sym(paste0("P7_42_",i, "_3"))) %>% 
    group_by(P1_22, !!sym(paste0("P7_42_",i, "_3"))) %>% 
    summarize(total_personas = survey_total(na.rm=T),
              prop_personas = survey_mean(na.rm=T, 
                                          vartype = c("ci","se"), 
                                          level = 0.95)) %>% 
    filter(!!sym(paste0("P7_42_",i, "_3")) == 1) %>% 
    select(-!!sym(paste0("P7_42_",i, "_3"))) %>% 
    ungroup %>% 
    as_tibble() %>% 
    mutate(origen = "Personal técnico") %>% 
    mutate(variable = paste0("P7_40_",i))
}) %>% 
  bind_rows()

 origen <- bind_rows(internos, custodios, tecnico)  %>% 
   mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                         "4"="Hombre trans", "5"="Otra", "6" = "Prefiero no responder", 
                         "8" = "No entendí")) %>% 
            left_join(diccionario2 %>% 
                        distinct(NEMONICO, NOMBRE_CAMPO) %>% 
                        select(variable = NEMONICO, NOMBRE_CAMPO)) %>% 
   mutate(NOMBRE_CAMPO = str_remove_all(NOMBRE_CAMPO, "Victimización en el centro penitenciario: "))
 
 origen %>% filter(P1_22 %in% c("Hombre trans", "Mujer trans")) %>% 
   ggplot(aes(origen, prop_personas, fill=P1_22,
          label = str_c(round(prop_personas*100,1),"%")))+
   geom_col(position = "dodge")+
   facet_wrap(~NOMBRE_CAMPO) +
   geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
             size=5)+
   hrbrthemes::theme_ipsum(grid="") +
   scale_y_continuous(
     breaks = seq(0,1,.2),
     labels = scales::percent) +
   guides(fill=guide_legend(title="Identidad"))+
   scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A"))+
   labs(title="Porcentaje de personas trans que sufrieron alguna victimización en el centro penitenciario",
        subtitle="por tipo de victimización, identidad de género y origen en los últimos 12 meses dentro del Centro Penitenciario",
        y="", x="",
        caption="Fuente: ENPOL 2021 - INEGI\nLos porcentajes suman más de 100% debido a que el hay múltiples origenes de la victimización.")
 
 
 ggsave("graficas/2_tortura_trans_origen.png", width = 14, height = 10)
 
 # EN la cárcelA: No Hetero -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 
 reclusion_orientacion <- lapply(names(ENPOL2021_7 %>%select(contains("P7_40"))),
                                function(i){
                                  tortura %>% 
                                    filter(!P1_23 %in% c(4,6,8,9)) %>% 
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
 
 reclusion_orientacion <- reclusion_orientacion  %>% 
   mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                         "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>% 
   left_join(diccionario2 %>% 
               distinct(NEMONICO, NOMBRE_CAMPO) %>% 
               select(variable = NEMONICO, NOMBRE_CAMPO)) %>% 
   mutate(tipo = str_remove_all(
     NOMBRE_CAMPO, "Victimización en el centro penitenciario: "
   )) %>% 
   select(-NOMBRE_CAMPO) %>% 
   mutate(tipo = gsub("(^[[:alpha:]])", "\\U\\1", tipo, perl=TRUE))
 
 reclusion_orientacion %>% 
   group_by(tipo) %>% 
   mutate(max = max(prop_personas)) %>% 
   ggplot(aes(x= P1_23, 
              y=prop_personas, 
              fill=P1_23, 
              label = str_c(round(prop_personas*100,1),"%"))) +
   geom_col()+
   # geom_errorbar(aes(ymin=prop_personas_low, 
   #                  ymax=prop_personas_upp), width=.2,
   #             position=position_dodge(.9)) +
   facet_wrap(~tipo,ncol=3)+
   geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
             size=5)+
   hrbrthemes::theme_ipsum(grid="") +
   scale_y_continuous(labels = scales::percent) +
   guides(fill="none")+
   scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A")) +
   labs(title="Porcentaje de personas que sufrieron alguna victimización en el centro penitenciario",
        subtitle="por tipo de victimización y orientación sexual, los últimos 12 meses dentro del Centro Penitenciario",
        y="", x="",
        caption="Fuente: ENPOL 2021 - INEGI")
 
 
 
 ggsave("graficas/2_tortura_nohet_fisica_reclusion.png", width = 14, height = 10)
 
 
 
 
 
 # [Responsables] en la cárcel: Nohet -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 x <- paste0("P7_42_", 1:6, "_")
 
 responsables <- as.vector(outer(x, 1:3, paste, sep=""))
 

 internos <- lapply(1:6, function(i){
   resp_design %>% 
     filter(!!sym(paste0("P7_40_",i))==1) %>% 
     filter(P1_23 %in% c(1,2,3)) %>% 
     select(P1_23, 
            !!sym(paste0("P7_40_",i)), 
            !!sym(paste0("P7_42_",i, "_1"))) %>% 
     group_by(P1_23, !!sym(paste0("P7_42_",i, "_1"))) %>% 
     summarize(total_personas = survey_total(na.rm=T),
               prop_personas = survey_mean(na.rm=T, 
                                           vartype = c("ci","se"), 
                                           level = 0.95)) %>% 
     filter(!!sym(paste0("P7_42_",i, "_1")) == 1) %>% 
     select(-!!sym(paste0("P7_42_",i, "_1"))) %>% 
     ungroup %>% 
     as_tibble() %>% 
     mutate(origen = "Internos(as)") %>% 
     mutate(variable = paste0("P7_40_",i))
 }) %>% 
   bind_rows()
 
 
 
 
 custodios <- lapply(1:6, function(i){
   resp_design %>% 
     filter(!!sym(paste0("P7_40_",i))==1) %>% 
     filter(P1_23 %in% c(1,2,3)) %>% 
     select(P1_23, 
            !!sym(paste0("P7_40_",i)), 
            !!sym(paste0("P7_42_",i, "_2"))) %>% 
     group_by(P1_23, !!sym(paste0("P7_42_",i, "_2"))) %>% 
     summarize(total_personas = survey_total(na.rm=T),
               prop_personas = survey_mean(na.rm=T, 
                                           vartype = c("ci","se"), 
                                           level = 0.95)) %>% 
     filter(!!sym(paste0("P7_42_",i, "_2")) == 1) %>% 
     select(-!!sym(paste0("P7_42_",i, "_2"))) %>% 
     ungroup %>% 
     as_tibble() %>% 
     mutate(origen = "Custodios(as)") %>% 
     mutate(variable = paste0("P7_40_",i))
 }) %>% 
   bind_rows()
 
 
 
 tecnico <- lapply(1:6, function(i){
   resp_design %>% 
     filter(!!sym(paste0("P7_40_",i))==1) %>% 
     filter(P1_23 %in% c(1,2,3)) %>% 
     select(P1_23, 
            !!sym(paste0("P7_40_",i)), 
            !!sym(paste0("P7_42_",i, "_3"))) %>% 
     group_by(P1_23, !!sym(paste0("P7_42_",i, "_3"))) %>% 
     summarize(total_personas = survey_total(na.rm=T),
               prop_personas = survey_mean(na.rm=T, 
                                           vartype = c("ci","se"), 
                                           level = 0.95)) %>% 
     filter(!!sym(paste0("P7_42_",i, "_3")) == 1) %>% 
     select(-!!sym(paste0("P7_42_",i, "_3"))) %>% 
     ungroup %>% 
     as_tibble() %>% 
     mutate(origen = "Personal técnico") %>% 
     mutate(variable = paste0("P7_40_",i))
 }) %>% 
   bind_rows()
 
 origen <- bind_rows(internos, custodios, tecnico)  %>% 
   mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                         "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%
   left_join(diccionario2 %>% 
               distinct(NEMONICO, NOMBRE_CAMPO) %>% 
               select(variable = NEMONICO, NOMBRE_CAMPO)) %>% 
   mutate(NOMBRE_CAMPO = str_remove_all(NOMBRE_CAMPO, "Victimización en el centro penitenciario: "))
 
 origen %>% filter(P1_23 %in% c("Bisexual", "Homosexual")) %>% 
   ggplot(aes(origen, prop_personas, fill=P1_23,
              label = str_c(round(prop_personas*100,1),"%")))+
   geom_col(position = "dodge")+
   facet_wrap(~NOMBRE_CAMPO) +
   geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
             size=5)+
   hrbrthemes::theme_ipsum(grid="") +
   scale_y_continuous(
     breaks = seq(0,1,.2),
     labels = scales::percent) +
   guides(fill=guide_legend(title="Orientación"))+
   scale_fill_manual(values = c("#009292","#A52FF5","#F5782F","#D4F50A"))+
   labs(title="Porcentaje de personas homosexuales o bisexuales que sufrieron alguna victimización en el centro penitenciario",
        subtitle="por tipo de victimización, identidad de género y origen en los últimos 12 meses dentro del Centro Penitenciario",
        y="", x="",
        caption="Fuente: ENPOL 2021 - INEGI\nLos porcentajes suman más de 100% debido a que el hay múltiples origenes de la victimización.")
 
 
 ggsave("graficas/2_tortura_nohet_origen.png", width = 14, height = 10)
 
 
 
 
 
 











# patio de juegos ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tortura %>%
  group_by(P1_22) %>%
  summarize(proportion = survey_mean(),
            total = survey_total())
