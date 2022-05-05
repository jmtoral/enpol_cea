# Análisis de la ENPOL para Proyecto LGBTI: CNDH
# Fecha de elaboración: 2022/05/05
# Fecha de actualización: 2022/05/05
# Autor: Manuel Toral

pacman::p_load(tidyverse, readxl)

indicador_gen <- read_excel("input/2000353.xlsx", sheet="E_NacIndicador")


indicador_gen %>% 
  filter(str_detect(Tema, "LGBTT|Homosex")) %>% 
  mutate(Indicador = case_when(
    str_detect(Indicador,"38.a") ~ "A: Registro de personas LGBTQ+",
    str_detect(Indicador,"38.b") ~ "B: Ubicación por seguridad de personas LGBTQ+ que lo soliciten",
    str_detect(Indicador,"38.c") ~ "C: Acceso en igualdad de condiciones a las instalaciones",
    str_detect(Indicador,"38.d") ~ "D: Acceso a servicios del centro en igualdad de condiciones"
  )) %>% 
  mutate(Año = as.integer(Año)) %>% 
  ggplot(aes(Año, Calificación,
             color = Indicador)) +
    geom_line(size=1) +
    geom_point() +
    facet_wrap(~`Tipo de centro`, ncol = 1)+
  hrbrthemes::theme_ipsum(grid="XY")+
  labs(title= "Puntajes promedio de centro penitenciario del rubro V* tema 38**",
       subtitle="por tipo de centro penitenciario e indicador",
       caption="Fuente: DNSP de la CNDH.\n*Grupos de personas privadas de la libertad con requerimientos específicos.\n**Personas LGBTQ+ u Homosexuales")


ggsave("graficas/cndh_rubroVt38.png", width = 10, height = 8)
