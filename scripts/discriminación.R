# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, sf, scales, biscale, cowplot, janitor, gridExtra, ggsflabel)
extrafont::loadfonts(device = "win", quiet =T)

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/bd_enpol_2021_RData/BD_ENPOL_2021.RData")


# Discriminación ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


disc <- ENPOL2021_SOC  %>% 
  left_join(ENPOL2021_7) %>%
  # mutate(no_hetero = as.numeric(!P1_23 %in% c(3,8,9))) %>%
  #mutate(trans = as.numeric(P1_22 %in% c(3,4))) %>%
  mutate(FAC_PER = as.numeric(as.character(FAC_PER)),
         EST_DIS = as.numeric(as.character(EST_DIS)),
         FPC = as.numeric(as.character(FPC)), 
         total = 1) %>% 
  as_survey_design(strata = EST_DIS, 
                   weights = FAC_PER, 
                   fpc = FPC)

## Orientación

disc %>% 
  group_by(P7_46_08) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95))
disc %>% 
  filter(P1_23 %in% c(1,2)) %>%
  filter(P7_46_08 %in% c(1,2)) %>% 
  #filter(P7_46_08 == 1) %>% 
  group_by(P7_46_08) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"),
                                        level = 0.95)) %>% 
  mutate(P7_46_08 = recode(P7_46_08, "1"="Sí", "2"="No")) %>% 
  ggplot(aes(P7_46_08, prop_personas, fill=P7_46_08,
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill="none")+
  scale_fill_manual(values = c("#009292","#F3A200","#F36E00")) +
  labs(title="Porcentaje de personas homosexuales y bisexuales",
       subtitle="por situación de tratos diferentes o rechazo por su orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI")

ggsave("graficas/discriminacion_nohet_fisica_reclusion.png", width = 10, height = 8)




## Identidad

disc %>% 
  group_by(P7_46_09) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95))
disc %>% 
  filter(P1_22 %in% c(3,4)) %>%
  filter(P7_46_09 %in% c(1,2)) %>% 
  #filter(P7_46_08 == 1) %>% 
  group_by(P7_46_09) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"),
                                        level = 0.95)) %>% 
  mutate(P7_46_09 = recode(P7_46_09, "1"="Sí", "2"="No")) %>% 
  ggplot(aes(P7_46_09, prop_personas, fill=P7_46_09,
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill="none")+
  scale_fill_manual(values = c("#009292","#F3A200","#F36E00")) +
  labs(title="Porcentaje de personas transgénero, transexuales o trasvestis",
       subtitle="por situación de tratos diferentes o rechazo por su orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI")

ggsave("graficas/discriminacion_trans_fisica_reclusion.png", width = 10, height = 8)




# Visitas íntimas ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

disc %>% 
  filter(!P1_22 %in% c(6,8,9)) %>%
  filter(P7_31 %in% c(1,2,3)) %>% 
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra")) %>%
  mutate(P7_31 = recode(P7_31, "1"="Sí", "2"="No","3"="No por COVID")) %>%
  #filter(P7_46_08 == 1) %>% 
  group_by(P1_22, P7_31) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95))  %>% 
  ggplot(aes(P7_31, prop_personas, fill=P1_22,
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_text(aes(y=prop_personas_upp+0.3), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill="none")+
  facet_wrap(~P1_22)+
  scale_fill_manual(values = c("#009292","#F3A200","#F36E00", "darkred")) +
  labs(title="Porcentaje de personas con respecto a si ha recibido visita íntima",
       subtitle="por identidad de género, en los últimos 12 meses",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI")

ggsave("graficas/visita_trans_fisica_reclusion.png", width = 10, height = 8)





disc %>% 
  filter(P1_23 %in% c(1,2,3)) %>%
  filter(P7_31 %in% c(1,2,3)) %>% 
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%
  mutate(P7_31 = recode(P7_31, "1"="Sí", "2"="No","3"="No por COVID")) %>%
  #filter(P7_46_08 == 1) %>% 
  group_by(P1_23, P7_31) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"), 
                                        level = 0.95))  %>% 
  ggplot(aes(P7_31, prop_personas, fill=P1_23,
             label = str_c(round(prop_personas*100,1),"%"))) +
  geom_col()+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill="none")+
  facet_wrap(~P1_23)+
  scale_fill_manual(values = c("#009292","#F3A200","#F36E00", "darkred")) +
  labs(title="Porcentaje de personas con respecto a si ha recibido visita íntima",
       subtitle="por orientación sexual, en los últimos 12 meses",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI")

ggsave("graficas/visita_nohet_fisica_reclusion.png", width = 10, height = 6)

