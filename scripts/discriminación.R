# Bibliotecas --------------------------------------------------------------

pacman::p_load(tidyverse, srvyr, sf, scales, biscale, cowplot, janitor, gridExtra, ggsflabel)
extrafont::loadfonts(device = "win", quiet =T)

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("data/bd_enpol_2021_RData/BD_ENPOL_2021.RData")

diccionario7 <- read_csv("input/diccionario_de_datos_ENPOL2021_7.csv",
                          locale = locale(encoding="LATIN1"))

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
  labs(title="Porcentaje de personas homosexuales y bisexuales privadas de su libertad",
       subtitle="por situación de tratos diferentes o rechazo por su orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI")

ggsave("graficas/discriminacion_nohet_fisica_reclusion.png", width = 10.5, height = 8)

ggsave("graficas_eps/discriminacion_nohet_fisica_reclusion.eps", width = 10.5, height = 8,
       device="eps")


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
  labs(title="Porcentaje de personas trans privadas de su libertad",
       subtitle="por situación de tratos diferentes o rechazo por su orientación sexual",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI")

ggsave("graficas/discriminacion_trans_fisica_reclusion.png", width = 10, height = 8)

ggsave("graficas_eps/discriminacion_trans_fisica_reclusion.eps", width = 10.5, height = 8,
       device="eps")



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

ggsave("graficas_eps/visita_trans_fisica_reclusion.eps", width = 10.5, height = 8,
       device="eps")



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
ggsave("graficas_eps/visita_nohet_fisica_reclusion.eps", width = 10.5, height = 8,
       device="eps")



# Visitas normales --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


disc %>% 
  filter(P1_23 %in% c(1,2,3)) %>%
  filter(P7_25 %in% c(1,2)) %>% 
  #filter(P7_46_08 == 1) %>% 
  group_by(P1_23, P7_25) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"),
                                        level = 0.95)) %>%
  mutate(P1_23 = recode(P1_23, "1"="Bisexual", "2"="Homosexual","3"="Heterosexual",
                        "4"="Otra",  "8" = "No sabe", "9" = "No contesta")) %>%
  mutate(P7_25 = recode(P7_25, "1"="Sí", "2"="No")) %>% 
  ggplot(aes(
    P1_23, 
    prop_personas, fill=P7_25,
    label = str_c(round(prop_personas*100,1),"%"))
         ) +
  geom_col(position ="dodge")+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  #guides(fill="none")+
  scale_fill_manual(values = c("#009292","#F3A200","#F36E00", "darkred"),
                    name= "¿Recibe visita familiar?") +
  labs(title="Porcentaje de personas con respecto a si ha recibido visita familiar",
       subtitle="por orientación sexual, en los últimos 12 meses",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI")

ggsave("graficas_eps/visita_nohet_familiar.eps", width = 10.5, height = 8,
       device="eps")


disc %>% 
  filter(P1_22 %in% c(1,2,3, 4)) %>%
  filter(P7_25 %in% c(1,2)) %>% 
  #filter(P7_46_08 == 1) %>% 
  group_by(P1_22, P7_25) %>% 
  summarise(prop_personas = survey_mean(na.rm=T, 
                                        vartype = c("ci"),
                                        level = 0.95)) %>%
  mutate(P1_22 = recode(P1_22, "1"="Hombre cis", "2"="Mujer cis","3"="Mujer trans",
                        "4"="Hombre trans", "5"="Otra")) %>%
  mutate(P7_25 = recode(P7_25, "1"="Sí", "2"="No")) %>% 
  ggplot(aes(
    P1_22, 
    prop_personas, fill=P7_25,
    label = str_c(round(prop_personas*100,1),"%"))
  ) +
  geom_col(position ="dodge")+
  geom_text(aes(y=prop_personas_upp+0.1), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  #guides(fill="none")+
  scale_fill_manual(values = c("#009292","#F3A200","#F36E00", "darkred"),
                    name= "¿Recibe visita familiar?") +
  labs(title="Porcentaje de personas con respecto a si ha recibido visita familiar",
       subtitle="por identidad de género, en los últimos 12 meses",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI")

ggsave("graficas_eps/visita_trans_familiar.eps", width = 10.5, height = 8,
       device="eps")


# Razones discriminación orientación --------------------------------------------------------------------------------------------------------------------------------------------------------------

# P7_46_08 Discriminación por ser gay, lesbiana o bisexual (orientación/preferencia sexual)?
# P7_46_09 Discriminación por ser ser hombre, mujer o persona trans (identidad de género)?
# Internos(as) P7_47_1 
# Custodios(as) P7_47_2 
# Personal técnico penitenciario (psicólogos, trabajadores sociales, criminólogos, administrativos, etc.) P7_47_3

fuent_disc <- lapply(names(ENPOL2021_7 |>  select(starts_with("P7_47_")))[1:3], function(i){
  disc |> 
    filter(P7_46_08 == 1) |> 
   # filter(!!sym(i) == 1) |> 
    group_by(!!sym(i)) |> 
    summarise(total_personas = survey_total(na.rm=T)) |> 
    mutate(variable = i) |> 
    rename(respuesta = !!sym(i))
}) |> 
  bind_rows() |> 
  mutate(variable = case_when(
    variable == "P7_47_1" ~ "Intern(oa)s",
    variable == "P7_47_2" ~ "Custodios(as)",
    variable == "P7_47_3" ~ "Personal técnico"
  )) |> 
  filter(respuesta == 1) |> 
  mutate(total_dis = disc |> 
           filter(P7_46_08 == 1) |> 
           summarise(total_personas = survey_total(na.rm=T)) |>
           pull(total_personas)
  ) |> 
  mutate(prop_personas = total_personas/total_dis)

fuent_disc |> 
  ggplot(aes(
    variable, 
    prop_personas, fill=variable,
    label = str_c(round(prop_personas*100,1),"%"))
  ) +
  geom_col(position ="dodge")+
  geom_text(aes(y=prop_personas+0.2), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  #guides(fill="none")+
  scale_fill_manual(values = c("#009292","#F3A200","#F36E00", "darkred"),
                    name= "Responsable") +
  guides(fill="none") +
  labs(title="Porcentaje de personas que fueron discriminadas\npor su orientación sexual",
       subtitle="por reponsable",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa suma de porcentajes es más de 100% porque una persona pudo haber sido discriminada por más de un responsable.")

ggsave("graficas/discrim_nohet_responsable.png", width = 10, height = 6)
ggsave("graficas_eps/discrim_nohet_responsable.eps", width = 10.5, height = 8,
       device="eps")

# Razones discriminación identidad ----------------------------------------------------------------------------------------------------------------------------------------------------------------

fuent_disc <- lapply(names(ENPOL2021_7 |>  select(starts_with("P7_47_")))[1:3], function(i){
  disc |> 
    filter(P7_46_09 == 1) |> 
    # filter(!!sym(i) == 1) |> 
    group_by(!!sym(i)) |> 
    summarise(total_personas = survey_total(na.rm=T)) |> 
    mutate(variable = i) |> 
    rename(respuesta = !!sym(i))
}) |> 
  bind_rows() |> 
  mutate(variable = case_when(
    variable == "P7_47_1" ~ "Intern(oa)s",
    variable == "P7_47_2" ~ "Custodios(as)",
    variable == "P7_47_3" ~ "Personal técnico"
  )) |> 
  filter(respuesta == 1) |> 
  mutate(total_dis = disc |> 
           filter(P7_46_09 == 1) |> 
           summarise(total_personas = survey_total(na.rm=T)) |>
           pull(total_personas)
  ) |> 
  mutate(prop_personas = total_personas/total_dis)

fuent_disc |> 
  ggplot(aes(
    variable, 
    prop_personas, fill=variable,
    label = str_c(round(prop_personas*100,1),"%"))
  ) +
  geom_col(position ="dodge")+
  geom_text(aes(y=prop_personas+0.2), position = position_dodge(0.9), vjust=2,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(labels = scales::percent) +
  #guides(fill="none")+
  scale_fill_manual(values = c("#009292","#F3A200","#F36E00", "darkred"),
                    name= "Responsable") +
  guides(fill="none") +
  labs(title="Porcentaje de personas que fueron discriminadas\npor su identidad de género",
       subtitle="por reponsable",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa suma de porcentajes es más de 100% porque una persona pudo haber sido discriminada por más de un responsable.")

ggsave("graficas/discrim_trans_responsable.png", width = 10, height = 6)

ggsave("graficas_eps/discrim_trans_responsable.eps", width = 10.5, height = 8,
       device="eps")

















# Formas de discriminación orientación --------------------------------------------------------------------------------------------------------------------------------------------------------------

# P7_46_08 Discriminación por ser gay, lesbiana o bisexual (orientación/preferencia sexual)?
# P7_46_09 Discriminación por ser ser hombre, mujer o persona trans (identidad de género)?
# P7_48_01:10 

forma_disc <- lapply(names(ENPOL2021_7 |>  select(starts_with("P7_48_")))[1:10], function(i){
  disc |> 
    filter(P7_46_08 == 1) |> 
    # filter(!!sym(i) == 1) |> 
    group_by(!!sym(i)) |> 
    summarise(total_personas = survey_total(na.rm=T)) |> 
    mutate(variable = i) |> 
    rename(respuesta = !!sym(i))
}) |> 
  bind_rows() |> 
  filter(respuesta == "1") |> 
  left_join(diccionario7 |> 
              filter(RANGO_CLAVES == "1") |> 
              select(NOMBRE_CAMPO, variable = NEMONICO)) |> 
  mutate(total_dis = disc |> 
           filter(P7_46_08 == 1) |> 
           summarise(total_personas = survey_total(na.rm=T)) |>
           pull(total_personas)
  ) |> 
  mutate(prop_personas = total_personas/total_dis) |> 
  mutate(NOMBRE_CAMPO = str_remove_all(NOMBRE_CAMPO,
                                       "Acción de discriminación experimentada: "))
  

forma_disc |> 
  ggplot(aes(
    reorder(NOMBRE_CAMPO, prop_personas), 
    prop_personas, fill=variable,
    label = str_c(round(prop_personas*100,1),"%"))
  ) +
  geom_col(position ="dodge")+
  geom_text(aes(y=prop_personas+0.1), position = position_dodge(0.9), #hjust=0.5,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(breaks = seq(0,1,.2), 
                     labels = percent(seq(0,1,.2))) +
  coord_flip()+
  guides(fill="none")+
  theme(plot.title.position = 'plot') +
  # scale_fill_manual(values = c("#009292","#F3A200","#F36E00", "darkred"),
  #                   name= "Responsable") +
  labs(title="Porcentaje de personas que fueron discriminadas por su orientación sexual",
       subtitle="por acción de discriminación experimentada",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa suma de porcentajes es más de 100% porque una persona pudo haber sufrido más de una acción.")

ggsave("graficas/discrim_nohet_accion.png", width = 12, height = 8)


ggsave("graficas_eps/discrim_nohet_accion.eps", width = 10.5, height = 8,
       device="eps")

# Formas de discriminación identidad ----------------------------------------------------------------------------------------------------------------------------------------------------------------

forma_disc <- lapply(names(ENPOL2021_7 |>  select(starts_with("P7_48_")))[1:10], function(i){
  disc |> 
    filter(P7_46_09 == 1) |> 
    # filter(!!sym(i) == 1) |> 
    group_by(!!sym(i)) |> 
    summarise(total_personas = survey_total(na.rm=T)) |> 
    mutate(variable = i) |> 
    rename(respuesta = !!sym(i))
}) |> 
  bind_rows() |> 
  filter(respuesta == "1") |> 
  left_join(diccionario7 |> 
              filter(RANGO_CLAVES == "1") |> 
              select(NOMBRE_CAMPO, variable = NEMONICO)) |> 
  mutate(total_dis = disc |> 
           filter(P7_46_09 == 1) |> 
           summarise(total_personas = survey_total(na.rm=T)) |>
           pull(total_personas)
  ) |> 
  mutate(prop_personas = total_personas/total_dis) |> 
  mutate(NOMBRE_CAMPO = str_remove_all(NOMBRE_CAMPO,
                                       "Acción de discriminación experimentada: "))


forma_disc |> 
  ggplot(aes(
    reorder(NOMBRE_CAMPO, prop_personas), 
    prop_personas, fill=variable,
    label = str_c(round(prop_personas*100,1),"%"))
  ) +
  geom_col(position ="dodge")+
  geom_text(aes(y=prop_personas+0.1), position = position_dodge(0.9), #hjust=0.5,
            size=5)+
  hrbrthemes::theme_ipsum(grid="") +
  scale_y_continuous(breaks = seq(0,1,.2), 
                     labels = percent(seq(0,1,.2))) +
  coord_flip()+
  guides(fill="none")+
  theme(plot.title.position = 'plot') +
  # scale_fill_manual(values = c("#009292","#F3A200","#F36E00", "darkred"),
  #                   name= "Responsable") +
  labs(title="Porcentaje de personas que fueron discriminadas por su identidad de género",
       subtitle="por acción de discriminación experimentada",
       y="", x="",
       caption="Fuente: ENPOL 2021 - INEGI\nLa suma de porcentajes es más de 100% porque una persona pudo haber sufrido más de una acción.")

ggsave("graficas/discrim_trans_accion.png", width = 12, height = 8)
ggsave("graficas_eps/discrim_trans_accion.eps", width = 10.5, height = 8,
       device="eps")
