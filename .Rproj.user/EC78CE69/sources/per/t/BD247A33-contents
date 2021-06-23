library(dplyr)
load("datos_depurados (1).Rda")
datos_2 = datos
save(datos_2, file = "datos_depurados (1).Rda")
rm(datos)
datos_2$cant_menores_14 = as.numeric(datos_2$cant_menores_14)
datos_2$ingresos_del_hogar = as.factor(datos_2$ingresos_del_hogar)
datos_2$ingresos_del_hogar = factor(datos_2$ingresos_del_hogar, levels = c("Menos de $25.000",
                                          "Entre $25000 y 55.000","Entre $55.000 y $99.000",
                                          "Entre $100.000 y $200.000","M?s de $200.000"))

### Tabla ingresos del hogar y cantidad promedio de personas seg?n rango etario
tabla1 = datos_2 %>%
  filter(is.na(ingresos_del_hogar)==F) %>% 
  group_by(ingresos_del_hogar) %>%
  summarise("menos de 14 a?os" = mean(cant_menores_14, na.rm = T),
            "de 15 a 60 a?os" = mean(cant_14_60, na.rm = T),
            "m?s de 60 a?os" = mean(cant_mas_60, na.rm = T))
tabla1

### Tabla ingresos del hogar y cantidad promedio de perceptores de ingresos, por g?nero
tabla2 = datos_2 %>%
  filter(is.na(ingresos_del_hogar)==F) %>% 
  group_by(ingresos_del_hogar) %>%
  summarise("cantidad total de perceptores" = mean(cant_perceptores, na.rm = T),
            "cantidad de perceptores mujeres" = mean(cant_perceptores_mujer, na.rm = T),
            "cantidad de perceptores varones" = mean(cant_perceptores_varon, na.rm = T))
tabla2


### Tabla ingresos del hogar y cantidad promedio de miembros de la "vivienda" (no hogar)
tabla3 = datos_2 %>%
  filter(is.na(ingresos_del_hogar)==F) %>% 
  group_by(ingresos_del_hogar) %>%
  summarise("miembros del hogar" = mean(vivienda_miembros, na.rm = T),
            "cantidad de perceptores de ingresos" = mean(cant_perceptores, na.rm = T),
            "ratio miembros por perceptorx" =  mean(vivienda_miembros, na.rm = T)/mean(cant_perceptores, na.rm = T))
tabla3
