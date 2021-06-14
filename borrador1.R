class(datos$`6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?`)
datos$`6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?`<-
  as.factor(datos$`6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?`)
table(datos$`6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?`)
levels(datos$`6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?`)

datos$ingreso<-
  datos$`6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?`
  
         

datos$ingreso<-
  factor(datos$`6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?`,
         levels=c("Menos de $25.000", "Entre $25000 y 55.000", "Entre $55.000 y $99.000",
                  "Entre $100.000 y $200.000", "Más de $200.000"))


table(datos$`6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?`, 
      datos$ingreso)

datos$nse<-as.factor(datos$`Nivel Socio Económico`)

datos$nse<-factor(datos$nse, levels=c("01. Alto", "02. Medio alto", "03.Medio",
                                    "0.4 Medio Bajo", "0.5  Bajo"))
levels(datos$nse)<-c("Alto", "Medio alto", "Medio",
                    "Medio Bajo", "Bajo")
100*round(addmargins(prop.table(addmargins(table(datos$ingreso, datos$nse),2),2),1),3)
addmargins(table(datos$ingreso, datos$nse))

addmargins(prop.table(addmargins(table(datos$ingreso, datos$nse),2),2),1)

table(datos$`6.4 Ahora le voy a preguntar ¿qué pasó con los ingresos familiares durante la pandemia? Le voy a dar algunas opciones y debe indicarme una…`)
datos$cambio_ingresos<-as.factor(
  datos$`6.4 Ahora le voy a preguntar ¿qué pasó con los ingresos familiares durante la pandemia? Le voy a dar algunas opciones y debe indicarme una…`)

levels(datos$cambio_ingresos)<-c("aumento", "disminución", "variables",
                     "sin cambio, igual trabajo", "sin cambio, más trabajo")
table(datos$`6.4 Ahora le voy a preguntar ¿qué pasó con los ingresos familiares durante la pandemia? Le voy a dar algunas opciones y debe indicarme una…`, datos$cambio_ingresos)


100*round(addmargins(prop.table(addmargins(table(datos$cambio_ingresos, datos$nse),2),2),1),3)
addmargins(table(datos$cambio_ingresos, datos$nse))


# poner las variable sne una lista y
tablas_exploratorias[i] <- for (i in 5:8) {
    100*round(
    addmargins(
      prop.table(
        addmargins(
          table(
            datos[i], datos$nse),2),2),1),3)
  
}

u <- vector(mode = "list", length = 8)

rm(u)
for (i in 1:4) u<-list(table(datos[i]))


u
     

table(datos$"7.3 ¿El trabajo de la persona que aporta el ingreso principal en el hogar, es de…? (leer opciones)")
datos$tipo_trabajo<-
datos$tipo_trabajo<-as.factor(datos$`7.3 ¿El trabajo de la persona que aporta el ingreso principal en el hogar, es de…? (leer opciones)`)
table(datos$tipo_trabajo)

levels(datos$conformidad_trabajo_ahora)<-c("igual", "mejor", "peor")
table(datos$conformidad_trabajo_antes)
levels(datos$conformidad_trabajo_antes)<-c("igual", "mejor", "peor")
table(datos$`8.4 Actualmente, ¿cómo se siente con la situación laboral de su familia?`)


table(datos$`8.5 ¿Tuvieron durante la pandemia algunas de las siguientes dificultades para llevar adelante su trabajo?`)


datos<-separate(
  datos,
  `8.5 ¿Tuvieron durante la pandemia algunas de las siguientes dificultades para llevar adelante su trabajo?`,
  c("dificultad_1","dificultad_2",
    "dificultad_3","dificultad_4",
    "dificultad_5","dificultad_6","dificultad_7"),
  sep = ",",
  remove = FALSE,
  convert = FALSE,
  extra = "warn",
  fill = "warn")

datos$dificultad_1<-as.factor(datos$dificultad_1)
datos$dificultad_2<-as.factor(datos$dificultad_2)
datos$dificultad_3<-as.factor(datos$dificultad_3)
datos$dificultad_4<-as.factor(datos$dificultad_4)
datos$dificultad_5<-as.factor(datos$dificultad_5)
datos$dificultad_6<-as.factor(datos$dificultad_6)
datos$dificultad_7<-as.factor(datos$dificultad_7)


table(datos$`10.1 Cuando se presenta un problema de salud a Ud. o a un miembro de su familia, ¿acuden a?:`)


table(datos$`16.1 Durante la vigencia del aislamiento, ¿considera que la información obtenida sobre las medidas educativas en relación a los miembros de su hogar fue...?`)
table(datos$`La vivienda de su casa cuenta con [Cloacas]`, datos$nse)

sizes <- factor(sizes, levels = c("small", "medium", "large"))
table(datos$conformidad_trabajo_antes)
datos$conformidad_trabajo_antes<-
  factor(datos$conformidad_trabajo_antes, levels=c("Muy conforme", "Conforme", "Disconforme"))

###############

x <- list()rm(list=ls())
library(reshape2)
library(kableExtra)
library(tidyverse)
library(tidyselect)
datos_jp = read.table("datos_tablas.txt", header = T, sep = "\t")
datos_jp$X = NULL

i = 10
for(i in 2:ncol(datos_jp)){
  datos_ = datos_jp[,c(1,i)]
  datos_[[2]]= trimws(datos_[[2]], which = "l")
  datos_[[2]] = gsub(", ", ",", datos_[[2]])
  valores <- na.omit(unique(unlist(strsplit(as.character(datos_[[2]]), ","))))
  ver = separate(datos_, vars_pull(names(datos_), 2), into = as.character(c(1:length(valores))), sep =",")
  tabla = melt(ver, id.vars = c("nse"), na.rm = TRUE)
  
  
  tabla = kable_styling(column_spec(kable(100*round(
    addmargins(
      prop.table(
        addmargins(
          table(tabla$value, tabla$nse),
          2),2),1),3), 
    caption = paste("Variable - ",names(datos_)[[2]],"(porcentajes por NSE)")),  1, width="5cm"),
    latex_options = "HOLD_position")
  assign(paste0("tabla",i), tabla)
  x[[paste("Tabla de frecuencias relativas de la variable",names(datos_)[[2]])]] <- get(paste0("tabla",i))
  tabla
}
#################
?trimws
x <- list()
i = 10
for(i in 2:ncol(datos_jp)){
  datos_ = datos_jp[,c(1,i)]
  datos_[[2]]= trimws(datos_[[2]], which = "l")
  datos_[[2]] = gsub(", ", ",", datos_[[2]])
  valores <- na.omit(unique(unlist(strsplit(as.character(datos_[[2]]), ","))))
  ver = separate(datos_, vars_pull(names(datos_), 2), into = as.character(c(1:length(valores))), sep =",")
  tabla = melt(ver, id.vars = c("nse"), na.rm = TRUE)
  
  
  tabla = kable_styling(column_spec(kable(100*round(
    addmargins(
      prop.table(
        addmargins(
          table(tabla$value, tabla$nse),
          2),2),1),3), 
    caption = paste(names(datos_)[[2]],"(porcentajes por NSE)")),  1, width="5cm"),
    latex_options = "HOLD_position")
  assign(paste0("tabla",i), tabla)
  x[[paste("Tabla de frecuencias relativas de la variable",names(datos_)[[2]])]] <- get(paste0("tabla",i))
  tabla
}

##############
datos_ = datos_jp[,1]
datos_[[2]]= trimws(datos_[[2]], which = "l")
datos_[[2]] = gsub(", ", ",", datos_[[2]])
valores <- na.omit(unique(unlist(strsplit(as.character(datos_[[2]]), ","))))
ver = separate(as.data.frame(datos_), vars_pull(names(datos_), 2),
               into = as.character(c(1:length(valores))), sep =",")
tabla = melt(ver, id.vars = c("nse"), na.rm = TRUE)

tabla2


##################################################
load("G:/Mi unidad/proyecto_FCS_diciembre_2020/resultados/datos.Rda")
datos$nse<-as.factor(datos$`Nivel Socio Económico`)

datos$nse<-factor(datos$nse, levels=c("01. Alto", "02. Medio alto", "03.Medio",
                                      "0.4 Medio Bajo", "0.5  Bajo"))
levels(datos$nse)<-c("Alto", "Medio alto", "Medio",
                     "Medio Bajo", "Bajo")

table(datos$`3.2 Alguno de los integrantes del hogar tiene... (marcar todas las que señalen)`)


datos<-separate(datos, `3.2 Alguno de los integrantes del hogar tiene... (marcar todas las que señalen)`,
                into=c("cs1", "cs2", "cs3", "cs4"), sep = ",")

datos$cs1<- trimws(datos$cs1, which = "l")
datos$cs2<- trimws(datos$cs2, which = "l")
datos$cs3<- trimws(datos$cs3, which = "l")
datos$cs4<- trimws(datos$cs4, which = "l")


o1<-addmargins(table(datos$cs1, datos$nse),1)
o2<-addmargins(table(datos$cs2, datos$nse),1)
o3<-addmargins(table(datos$cs3, datos$nse),1)
o4<-addmargins(table(datos$cs4, datos$nse),1)

o1_sin_cobertura<-o1[1,]
o1_nsc<-o1[2,]
o1_obra_social<-o1[3,]
o1_otra<-o1[4,]
o1_pami<-o1[5,]
o1_prepaga_os<-o1[6,]
o1_prepaga_pago_voluntario<-o1[7,]
o1_PROFE<-o1[8,]
o1_total<-o1[9,]

o2_sin_cobertura<-o2[1,]
o2_obra_social<-o2[2,]
o2_otra<-o2[3,]
o2_prepaga_os<-o2[4,]
o2_prepaga_pago_voluntario<-o2[5,]
o2_PROFE<-o2[6,]
o2_total<-o2[7,]

o3
o3_sin_cobertura<-o3[1,]
o3_prepaga_os<-o3[2,]
o3_prepaga_pago_voluntario<-o3[3,]
o3_PROFE<-o3[4,]
o3_total<-o3[5,]

o4
o4_sin_cobertura<-o4[1,]
o4_total<-o4[2,]


sin_cobertura<-o1_sin_cobertura+o2_sin_cobertura+o3_sin_cobertura+
  o4_sin_cobertura
ns_nc<-o1_nsc
obra_social<-o1_obra_social+o2_obra_social
otra<-o1_otra+o2_otra
pami<-o1_pami
prepaga_os<-o1_prepaga_os+o2_prepaga_os+o3_prepaga_os
prepaga_voluntario<-o1_prepaga_pago_voluntario+o2_prepaga_pago_voluntario+
  o3_prepaga_pago_voluntario
profe<-o1_PROFE+o2_PROFE+o3_PROFE
total<-o1_total+o2_total+o3_total+o4_total

cobertura_salud<-rbind(obra_social, pami, prepaga_os, prepaga_voluntario, profe, otra, ns_nc,
      sin_cobertura, total)
cobertura_salud
cobertura_salud<-as.data.frame(cobertura_salud)

obra_social_porc<-100*round(cobertura_salud[1,]/cobertura_salud[9,],3)
pami_porc<-100*round(cobertura_salud[2,]/cobertura_salud[9,],3)
prepaga_os_porc<-100*round(cobertura_salud[3,]/cobertura_salud[9,],3)
prepaga_voluntario_porc<-100*round(cobertura_salud[4,]/cobertura_salud[9,],3)
profe_porc<-100*round(cobertura_salud[5,]/cobertura_salud[9,],3)
otra_porc<-100*round(cobertura_salud[6,]/cobertura_salud[9,],3)
ns_nc_porc<-100*round(cobertura_salud[7,]/cobertura_salud[9,],3)
sin_cobertura_porc<-100*round(cobertura_salud[8,]/cobertura_salud[9,],3)
total_porc<-100*round(cobertura_salud[9,]/cobertura_salud[9,],3)

cobertura_salud_porc<-rbind(obra_social_porc, pami_porc, prepaga_os_porc,
                            prepaga_voluntario_porc, profe_porc, otra_porc,
                            ns_nc_porc, sin_cobertura_porc, total_porc)
cobertura_salud_porc

# ayudas
table(datos[63])
datos$ayuda<-datos$`9.1 ¿Durante la pandemia, han solicitado algún tipo de ayuda en…?`
datos<-separate(datos$`9.1 ¿Durante la pandemia, han solicitado algún tipo de ayuda en…?`,
                into=c("ay1", "ay2", "ay3", "ay4", "ay5","ay6", "ay7", "ay8", "ay9", "ay10", "ay11"),
                sep = ",")

datos$ay1<- trimws(datos$ay1, which = "l")
datos$ay2<- trimws(datos$ay2, which = "l")
datos$ay3<- trimws(datos$ay3, which = "l")
datos$ay4<- trimws(datos$ay4, which = "l")
datos$ay5<- trimws(datos$ay5, which = "l")
datos$ay6<- trimws(datos$ay6, which = "l")
datos$ay7<- trimws(datos$ay7, which = "l")
datos$ay8<- trimws(datos$ay8, which = "l")
datos$ay9<- trimws(datos$ay9, which = "l")
datos$ay10<- trimws(datos$ay10, which = "l")
datos$ay11<- trimws(datos$ay11, which = "l")


o1<-addmargins(table(datos$cs1, datos$nse),1)
o2<-addmargins(table(datos$cs2, datos$nse),1)
o3<-addmargins(table(datos$cs3, datos$nse),1)
o4<-addmargins(table(datos$cs4, datos$nse),1)

o1_sin_cobertura<-o1[1,]
o1_nsc<-o1[2,]
o1_obra_social<-o1[3,]
o1_otra<-o1[4,]
o1_pami<-o1[5,]
o1_prepaga_os<-o1[6,]
o1_prepaga_pago_voluntario<-o1[7,]
o1_PROFE<-o1[8,]
o1_total<-o1[9,]

o2_sin_cobertura<-o2[1,]
o2_obra_social<-o2[2,]
o2_otra<-o2[3,]
o2_prepaga_os<-o2[4,]
o2_prepaga_pago_voluntario<-o2[5,]
o2_PROFE<-o2[6,]
o2_total<-o2[7,]

o3
o3_sin_cobertura<-o3[1,]
o3_prepaga_os<-o3[2,]
o3_prepaga_pago_voluntario<-o3[3,]
o3_PROFE<-o3[4,]
o3_total<-o3[5,]

o4
o4_sin_cobertura<-o4[1,]
o4_total<-o4[2,]


sin_cobertura<-o1_sin_cobertura+o2_sin_cobertura+o3_sin_cobertura+
  o4_sin_cobertura
ns_nc<-o1_nsc
obra_social<-o1_obra_social+o2_obra_social
otra<-o1_otra+o2_otra
pami<-o1_pami
prepaga_os<-o1_prepaga_os+o2_prepaga_os+o3_prepaga_os
prepaga_voluntario<-o1_prepaga_pago_voluntario+o2_prepaga_pago_voluntario+
  o3_prepaga_pago_voluntario
profe<-o1_PROFE+o2_PROFE+o3_PROFE
total<-o1_total+o2_total+o3_total+o4_total

cobertura_salud<-rbind(obra_social, pami, prepaga_os, prepaga_voluntario, profe, otra, ns_nc,
                       sin_cobertura, total)
cobertura_salud
cobertura_salud<-as.data.frame(cobertura_salud)

obra_social_porc<-100*round(cobertura_salud[1,]/cobertura_salud[9,],3)
pami_porc<-100*round(cobertura_salud[2,]/cobertura_salud[9,],3)
prepaga_os_porc<-100*round(cobertura_salud[3,]/cobertura_salud[9,],3)
prepaga_voluntario_porc<-100*round(cobertura_salud[4,]/cobertura_salud[9,],3)
profe_porc<-100*round(cobertura_salud[5,]/cobertura_salud[9,],3)
otra_porc<-100*round(cobertura_salud[6,]/cobertura_salud[9,],3)
ns_nc_porc<-100*round(cobertura_salud[7,]/cobertura_salud[9,],3)
sin_cobertura_porc<-100*round(cobertura_salud[8,]/cobertura_salud[9,],3)
total_porc<-100*round(cobertura_salud[9,]/cobertura_salud[9,],3)

cobertura_salud_porc<-rbind(obra_social_porc, pami_porc, prepaga_os_porc,
                            prepaga_voluntario_porc, profe_porc, otra_porc,
                            ns_nc_porc, sin_cobertura_porc, total_porc)
cobertura_salud_porc

table(datos$`14.6. Durante la pandemia¿ qué tipo de  continuidad educativa tuvieron? [Pre escolar]`)
table(datos$`4.2 En su casa cuentan con conectividad a internet a través de`)
datos$tipo_conexion<-
  as.factor(datos$`4.2 En su casa cuentan con conectividad a internet a través de`)
levels(datos$tipo_conexion)<-
  c("cableada", "cableada", "cableada", "cableada", "ninguna", "solo datos", 
    "cableada", "cableada", "cableada", "cableada", "cableada", "solo datos", 
    "datos y wifi compartido", "solo wifi compartido")


100*round(addmargins(prop.table(
  addmargins(table(
    datos$tipo_conexion, datos$nse),2),2),1),3)

table(datos$`4.3 En el último año, adquirieron nuevos paquetes o mejoraron el acceso a la conectividad?`,
      datos$nse)

datos$mejora_acceso_internet<-as.factor(datos$`4.3 En el último año, adquirieron nuevos paquetes o mejoraron el acceso a la conectividad?`)

datos$mejora_acceso_internet<-
  factor(datos$mejora_acceso_internet, levels=c("Sí", "No", "Ns/Nc"))

datos$mejora_acceso_internet <-droplevels(datos$mejora_acceso_internet)

100*round(addmargins(prop.table(
  addmargins(table(
    datos$mejora_acceso_internet, datos$nse),2),2),1),3)


# tamaño: grande (6 integrantes o más), no grande
# principal aportante: mujer varon No está la variable

table(is.na(datos$`Cuántas son menores de 14 años?`))
datos$`Cuántas son menores de 14 años?`[
  is.na(datos$`Cuántas son menores de 14 años?`)==TRUE]<-0
addmargins(table(datos$`Cuántas son menores de 14 años?`))


table(is.na(datos$`Cuántas tienen más de 60 años?`))
datos$`Cuántas tienen más de 60 años?`[
  is.na(datos$`Cuántas tienen más de 60 años?`)==TRUE]<-0
addmargins(table(datos$`Cuántas tienen más de 60 años?`))


datos$hogar_mas5<-
  ifelse(
    datos$`Cuántas personas viven en esa casa?`>5, 1, 0)
table(datos$`Cuántas personas viven en esa casa?`, datos$hogar_mas5)

datos$hogar_con_menores<-
  ifelse(datos$`Cuántas son menores de 14 años?`==0, 0, 1
  )
table(datos$`Cuántas son menores de 14 años?`, datos$hogar_con_menores)

datos$hogar_con_mayores<-
  ifelse(datos$`Cuántas tienen más de 60 años?`==0, 0, 1
  )
table(datos$`Cuántas tienen más de 60 años?`, datos$hogar_con_mayores)


datos$composicion<-
  datos$hogar_con_mayores+10*datos$hogar_con_menores+100*datos$hogar_mas5
table(datos$composicion)

datos$composicion<-
  as.factor(datos$composicion)



levels(datos$composicion)<-
  c(
    "no grande, sin menores, sin mayores", "no grande, sin menores, con mayores",
    "no grande, con menores, sin mayores", "no grande, con menores, con mayores",
    "grande, sin menores, sin mayores", "grande, sin menores, con mayores", 
    "grande, con menores, sin mayores", "grande, con menores, con mayores")





names(datos_jp)
ncol(datos_jp)
tabla2
names(datos)
table(datos$`6.2 En el último año, los ingresos de las personas provinieron de...?`)
datos$ayuda<-gsub(
  "`6.2 En el último año, los ingresos de las personas provinieron de...?`","( IFE, Progresar, PPP)",
  as.character(df$ayuda))

# ver si sirve
datos$f1= trimws(datos$f1, which = "l")
datos$f2= trimws(datos$f2, which = "l")
datos$f3= trimws(datos$f3, which = "l")
datos$f4= trimws(datos$f4, which = "l")


###############
# se genera otra columna
datos$fuente_ingresos<-
  datos$`6.2 En el último año, los ingresos de las personas provinieron de...?`

#  Se cambian las comas por guiones
datos$fuente_ingresos <- lapply(datos$fuente_ingresos,
                                gsub, pattern="( IFE, Progresar, PPP)",
                  replacement='IFE-Progresar-PPP')

# lo escribo para verle la forma


# se genera una culumna por opción
datos<-separate(datos, fuente_ingresos,
                c("f1", "f2", "f3", "f4"), sep=",")

# se Verifican con el csv los totales
table(datos$f1)# OK
table(datos$f2)#ok
table(datos$f3)# OK
table(datos$f4)# ok

# se quitan espacios antes
datos$f1= trimws(datos$f1, which = "l")
datos$f2= trimws(datos$f2, which = "l")
datos$f3= trimws(datos$f3, which = "l")
datos$f4= trimws(datos$f4, which = "l")

# cada fuente es 1 si está en algna columna
datos$fuente_trabajo<-ifelse((datos$f1=="Trabajo" |
                        datos$f2=="Trabajo" |
                        datos$f3=="Trabajo" |
                        datos$f4=="Trabajo"), "trabajo", NA)
table(datos$fuente_trabajo, datos$nse)


datos$fuente_programa_estatal<-ifelse((datos$f1=="Programa estatal (IFE-Progresar-PPP)" |
                                datos$f2=="Programa estatal (IFE-Progresar-PPP)" |
                                datos$f3=="Programa estatal (IFE-Progresar-PPP)" |
                                datos$f4=="Programa estatal (IFE-Progresar-PPP)"), "programa estatal", NA)
table(datos$fuente_programa_estatal, datos$nse)


datos$fuente_ayuda<-ifelse(datos$f1=="Ayuda social o familiar" |
                                datos$f2=="Ayuda social o familiar" |
                                datos$f3=="Ayuda social o familiar" |
                                datos$f4=="Ayuda social o familiar",
                           "ayuda social o familiar", NA)
table(datos$fuente_ayuda, datos$nse)

datos$fuente_jubilacion<-ifelse((datos$f1=="Jubilación / Pensión" |
                              datos$f2=="Jubilación / Pensión" |
                              datos$f3=="Jubilación / Pensión" |
                              datos$f4=="Jubilación / Pensión"),
                              "jubilación/pensión", NA)
table(datos$fuente_jubilacion, datos$nse)

datos$fuente_otra<-ifelse((datos$f1=="Otra fuente" |
                                   datos$f2=="Otra fuente" |
                                   datos$f3=="Otra fuente" |
                                   datos$f4=="Otra fuente"), "otra fuente de ingresos", NA)
datos$fuente_otra[datos$fuente_otra==0]<-NA
table(datos$fuente_otra, datos$nse)



trabajo<-addmargins(table(datos$fuente_trabajo, datos$nse),2)
programa_estatal<-addmargins(table(datos$fuente_programa_estatal, datos$nse),2)
ayuda<-addmargins(table(datos$fuente_ayuda, datos$nse), 2)
jubilacion<-addmargins(table(datos$fuente_jubilacion, datos$nse), 2)
otra<-addmargins(table(datos$fuente_otra, datos$nse), 2)
table(datos$nse)
totales<-addmargins(table(datos$nse))
totales
totales<-as.vector(totales[1:6])
fuente_de_ingresos_abs<-rbind(trabajo, jubilacion, programa_estatal,
                              ayuda, otra, totales)
fuente_de_ingresos_abs
# El 615 es total de filas no de columnas

fuente_de_ingresos_rel<-100*round(rbind(trabajo/totales,
                              ayuda/totales, jubilacion/totales,
                              otra/totales, totales/totales),3)

fuente_de_ingresos_rel
# el 100% es de fila no de columna
####################
datos$entre14y60<-datos$`Cuántas tienen entre 14 y 60 años?`
datos$mas60<-datos$`Cuántas tienen más de 60 años?`
datos$mas13<-datos$entre14y60+datos$mas60
class(datos$`Cuántas tienen entre 14 y 60 años?`)
datos$celulares_cada_mayor_13<-
  datos$`5.2 Cuántos teléfonos celulares hay en su hogar?`/
  (datos$`Cuántas tienen entre 14 y 60 años?`+ datos$`Cuántas tienen más de 60 años?`)
#############

table(datos$`7.2 ¿La persona que aporta el principal ingreso en el hogar, lo hace por?`,
      datos$nse)
# se genera otra columna
datos$fuente_ingresos_principal<-
  datos$`7.2 ¿La persona que aporta el principal ingreso en el hogar, lo hace por?`

# lo escribo para verle la forma
write.csv(table(datos$fuente_ingresos_principal), "fuente_ingresos_ppal.csv")

# se genera una culumna por opción
datos<-separate(datos, fuente_ingresos_principal,
                c("p1", "p2", "p3"), sep=",")

# se Verifican con el csv los totales
table(datos$p1)# OK
table(datos$p2)#ok
table(datos$p3)# OK


# se quitan espacios antes
datos$p1= trimws(datos$p1, which = "l")
datos$p2= trimws(datos$p2, which = "l")
datos$p3= trimws(datos$p3, which = "l")


datos$fuente_ppal_alquileres<-ifelse((datos$p1=="Cobro de alquileres o renta" |
                                datos$p2=="Cobro de alquileres o renta" |
                                datos$p3=="Cobro de alquileres o renta"),
                                "Cobro de alquileres o renta", NA)
table(datos$fuente_ppal_alquileres, datos$nse)


datos$fuente_ppal_negocio_propio<-ifelse((datos$p1=="Empresa o negocio propio" |
                                         datos$p2=="Empresa o negocio propio" |
                                         datos$p3=="Empresa o negocio propio"), "Empresa o negocio propio", NA)
table(datos$fuente_ppal_negocio_propio, datos$nse)


datos$fuente_ppal_eventual<-ifelse(datos$p1=="Eventual (por changas)" |
                             datos$p2=="Eventual (por changas)" |
                             datos$p3=="Eventual (por changas)",
                           "Eventual (por changas)", NA)
table(datos$fuente_ppal_eventual, datos$nse)

datos$fuente_ppal_jubilacion<-ifelse((datos$p1=="Jubilación / pensión" |
                                   datos$p2=="Jubilación / pensión" |
                                   datos$p3=="Jubilación / pensión"),
                                "Jubilación / pensión", NA)
table(datos$fuente_ppal_jubilacion, datos$nse)

datos$fuente_ppal_plan<-ifelse((datos$p1=="Plan o programa estatal" |
                             datos$p2=="Plan o programa estatal" |
                             datos$p3=="Plan o programa estatal"),
                             "Plan o programa estatal", NA)

table(datos$fuente_ppal_plan, datos$nse)

datos$fuente_ppal_trabajo_formal<-ifelse((datos$p1=="Trabajo formal" |
                                  datos$p2=="Trabajo formal" |
                                  datos$p3=="Trabajo formal"),
                               "Trabajo formal", NA)

table(datos$fuente_ppal_trabajo_formal, datos$nse)

datos$fuente_ppal_trabajo_informal<-ifelse((datos$p1=="Trabajo informal (en negro)" |
                                  datos$p2=="Trabajo informal (en negro)" |
                                  datos$p3=="Trabajo informal (en negro)"),
                               "Trabajo informal (en negro)", NA)

table(datos$fuente_ppal_trabajo_informal, datos$nse)


alquileres<-addmargins(table(datos$fuente_ppal_alquileres, datos$nse),2)
negocio_propio<-addmargins(table(datos$fuente_ppal_negocio_propio, datos$nse),2)
eventual<-addmargins(table(datos$fuente_ppal_eventual, datos$nse), 2)
jubilacion<-addmargins(table(datos$fuente_ppal_jubilacion, datos$nse), 2)
plan<-addmargins(table(datos$fuente_ppal_plan, datos$nse), 2)
trabajo_formal<-addmargins(table(datos$fuente_ppal_trabajo_formal, datos$nse), 2)
trabajo_informal<-addmargins(table(datos$fuente_ppal_trabajo_informal, datos$nse), 2)



fuente_de_ingresos_ppal_abs<-rbind(trabajo_formal,
                                   trabajo_informal,  negocio_propio,
                                   jubilacion, alquileres,
                                  eventual, plan,  totales)

fuente_de_ingresos_ppal_rel<-100*round(rbind(trabajo_formal/totales,
                                        trabajo_informal/totales, 
                                        negocio_propio/totales,
                                        jubilacion/totales, alquileres/totales,
                                        eventual/totales, plan/totales,  totales/totales),3)

write.csv(fuente_de_ingresos_ppal_abs, "fuente_ppal_abs.csv")
# el 100% es de fila no de columna

fuente_de_ingresos_ppal_abs
# El 615 es total de filas no de columnas


notebook<-addmargins(
  table(
    datos$`5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Notebook]`,
    datos$nse)
)[2,1:6]
pc<-addmargins(
  table(
    datos$`5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [PC de escritorio]` ,
    datos$nse)
)[2,1:6]
camara_fotos<-addmargins(
  table(
    datos$`5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Cámara de fotos]` ,
    datos$nse)
)[2,1:6]

impresora<-addmargins(
  table(
    datos$`5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Impresora]` ,
    datos$nse)
)[2,1:6]

notebook_CI<-addmargins(
  table(
    datos$`5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Notebook de Conectar Igualdad]` ,
    datos$nse)
)[2,1:6]

smart_TV<-addmargins(
  table(
    datos$`5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Smart TV]` ,
    datos$nse)
)[2,1:6]

tablet_ipad<-addmargins(
  table(
    datos$`5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Tablet / Ipad]` ,
    datos$nse)
)[2,1:6]


v<-addmargins(table(datos$nse))
electronicos<-100*round(rbind(pc/v[1:6], notebook/v[1:6], notebook_CI/v[1:6],
                    tablet_ipad/v[1:6], smart_TV/v[1:6], impresora/v[1:6],
                    camara_fotos/v[1:6]),3)

row.names(electronicos)<-c("PC de escritorio", "Notebook", "Notebook conectar igualdad",
                           "Tablet / Ipad", "Smart TV", "Impresora", "Cámara de fotos")

electronicos


## prestaciones estatales

addmargins(table(
  datos$`7.4 ¿Algún miembro del hogar percibió alguna de las prestaciones del Estado durante la Pandemia?`, datos$nse))

100*round(
  addmargins(
    prop.table(
      addmargins(
        table(datos$`7.4 ¿Algún miembro del hogar percibió alguna de las prestaciones del Estado durante la Pandemia?`,
              datos$nse),2),2),1),3)

## prestaciones ultimo mes
table(datos$`6.5 ¿Cuáles de las siguientes prestaciones estatales se percibieron en el último mes en el hogar? Puede marcar todas las que correspondan`)

 
datos$AUH<-str_extract(datos$`6.5 ¿Cuáles de las siguientes prestaciones estatales se percibieron en el último mes en el hogar? Puede marcar todas las que correspondan`,
                               "Asignación Universal por Hijo")
table(datos$AUH)

datos$otras<-str_extract(datos$`6.5 ¿Cuáles de las siguientes prestaciones estatales se percibieron en el último mes en el hogar? Puede marcar todas las que correspondan`,
                       "Otras")
table(datos$otras)

datos$SSC<-str_extract(datos$`6.5 ¿Cuáles de las siguientes prestaciones estatales se percibieron en el último mes en el hogar? Puede marcar todas las que correspondan`,
                       "Salario Social Complementario")
table(datos$SSC)

datos$SdD<-str_extract(datos$`6.5 ¿Cuáles de las siguientes prestaciones estatales se percibieron en el último mes en el hogar? Puede marcar todas las que correspondan`,
                       "Seguro de desempleo")
table(datos$SdD)

datos$ninguna<-str_extract(datos$`6.5 ¿Cuáles de las siguientes prestaciones estatales se percibieron en el último mes en el hogar? Puede marcar todas las que correspondan`,
                       "Ninguna de las anteriores")
table(datos$ninguna)

#######
v<-addmargins(table(datos$nse))

prestaciones1<-100*round(addmargins(table(datos$AUH, datos$nse), 2)[1:6]/v[1:6], 3)
prestaciones2<-100*round(addmargins(table(datos$otras, datos$nse), 2)[1:6]/v[1:6],3)
prestaciones3<-100*round(addmargins(table(datos$SSC, datos$nse), 2)[1:6]/v[1:6],3)
prestaciones4<-100*round(addmargins(table(datos$SdD, datos$nse), 2)[1:6]/v[1:6],3)
prestaciones5<-100*round(addmargins(table(datos$ninguna, datos$nse), 2)[1:6]/v[1:6],3)

prestaciones<-as.data.frame(rbind(prestaciones1, prestaciones3,
                                  prestaciones4, prestaciones2, prestaciones5))

rownames(prestaciones)<-c("Asignación Universal por Hijo",
                              "Salario Social Complementario", 
                              "Seguro de desempleo", "Otra",
                              "Ninguna de las anteriores")
kable(prestaciones,
      caption = "Prestaciones estatales percibdas por integrantes del hogar durante el último mes (porcentajes de respuestas por NSE")

########## opciones creditos
9.3.1 Créditos bancarios
9.3.2 Financiación por tarjeta de crédito
9.3.3 Préstamos familiares
9.3.4 Préstamos de amigos 
9.3.5 Otros préstamos entidades no reguladas
9.3.6 No, ninguna

 
table(datos$`9.3Durante la pandemia, ¿debió recurrir a algunas de las siguientes opciones de créditos y financiación?`
      )

datos$creditos<-datos$`9.3Durante la pandemia, ¿debió recurrir a algunas de las siguientes opciones de créditos y financiación?`

datos$creditos<-gsub("[()]", "", datos$creditos)


datos$bancario<-str_extract(datos$creditos,
                           "9.3.1 Créditos bancarios")
datos$tarjeta<-str_extract(datos$creditos,
                            "9.3.2 Financiación por tarjeta de crédito")
datos$familiares<-str_extract(datos$creditos,
                            "9.3.3 Préstamos familiares")
datos$amigos<-str_extract(datos$creditos,
                            "9.3.4 Préstamos de amigos")
datos$otros<-str_extract(datos$creditos,
                            "9.3.5 Otros préstamos entidades no reguladas")
datos$ninguno<-str_extract(datos$`9.3Durante la pandemia, ¿debió recurrir a algunas de las siguientes opciones de créditos y financiación?`,
                            "9.3.6 No, ninguna")

v<-addmargins(table(datos$nse))

credito1<-100*round(addmargins(table(datos$bancario, datos$nse), 2)[1:6]/v[1:6], 3)
credito2<-100*round(addmargins(table(datos$tarjeta, datos$nse), 2)[1:6]/v[1:6],3)
credito3<-100*round(addmargins(table(datos$familiares, datos$nse), 2)[1:6]/v[1:6],3)
credito4<-100*round(addmargins(table(datos$amigos, datos$nse), 2)[1:6]/v[1:6],3)
credito5<-100*round(addmargins(table(datos$otros, datos$nse), 2)[1:6]/v[1:6],3)
credito6<-100*round(addmargins(table(datos$ninguno, datos$nse), 2)[1:6]/v[1:6],3)

credito<-as.data.frame(rbind(credito1, credito2,
                                  credito3, credito4, credito5, credito6))

rownames(credito)<-c("Créditos bancarios", "Financiación por tarjeta de crédito",
                          "Préstamos familiares", "Préstamos de amigos",
                          "Otros préstamos (entidades no reguladas)",
                          "No, ninguna")
kable(credito,
      caption = "Opciones de crédito y financiación usadas durante la pandemia (porcentajes de respuestas por NSE")
############ hacinameinto

table(datos$`Cuántas personas viven en esa casa?`)
table(datos$`Cantidad de ambientes`)
datos$`Cantidad de ambientes`[datos$`Cantidad de ambientes`==0]<-NA
datos$personas_por_cuarto<-datos$`Cuántas personas viven en esa casa?`/
  datos$`Cantidad de ambientes`
summary(datos$personas_por_cuarto)
datos$hacinamiento_critico<-ifelse(datos$personas_por_cuarto>3, "más de tres personas por cuarto", 
                                   "tres personas o menos por cuarto")
table(datos$hacinamiento_critico, datos$nse)
addmargins(table(datos$hacinamiento_critico, datos$nse))

100*round(
  addmargins(
    prop.table(
      addmargins(
        table(
          datos$hacinamiento_critico, datos$nse),2),2),1),3)

########
table(datos$`Mujeres que aportan ingresos`)
table(datos$`Varones que aportan ingresos`)

table(datos$`Cuántos son mujeres?`)
table(datos$`Cuántos son varones?`)

datos$mayoria_mujeres<-
  ifelse(datos$`Cuántos son mujeres?`>datos$`Cuántos son varones?`,
         "Mayoría de mujeres", "Igual o mayor cantidad de varones")

100*round(
  addmargins(
    prop.table(
      addmargins(
        table(
          datos$mayoria_mujeres, datos$composicion_edad), 2),2),1),3)


addmargins(table(
  datos$mayoria_mujeres, datos$composicion_edad))

names(datos)
table(datos$`14. En su hogar hay integrantes cursando [No hay personas cursando estudios formales]`)
datos$no_hay_estudiantes<-datos$`14. En su hogar hay integrantes cursando [No hay personas cursando estudios formales]`
table(datos$hogar_con_menores, datos$no_hay_estudiantes)
table(datos$no_hay_estudiantes)
datos$con_menores_no_escolarizados<-ifelse(
  datos$hogar_con_menores==1 & datos$no_hay_estudiantes=="Sí",1,0
)

table(datos$con_menores_no_escolarizados)

table(datos$`3.4 Cuenta con un servicio médico de emergencia a domicilio?`, datos$nse)
datos$emergencia<-datos$`3.4 Cuenta con un servicio médico de emergencia a domicilio?`
datos$emergencia<-as.factor(datos$emergencia)
levels(datos$emergencia)
datos$emergencia<-factor(datos$emergencia,
                         levels=c("Sí", "No", "Ns/ Nc"))
table(datos$`3.4 Cuenta con un servicio médico de emergencia a domicilio?`, 
      datos$emergencia)
table(datos$emergencia, datos$nse)

table(datos$`3.5 En caso de urgencias, puede acudir a un servicio de ambulancia?`)
datos$ambulancia<-datos$`3.5 En caso de urgencias, puede acudir a un servicio de ambulancia?`
datos$ambulancia<-as.factor(datos$ambulancia)
levels(datos$ambulancia)
datos$ambulancia<-factor(datos$ambulancia,
                         levels=c("Sí", "No", "Ns/ Nc"))
table(datos$`3.5 En caso de urgencias, puede acudir a un servicio de ambulancia?`, 
      datos$ambulancia)
###########


table(datos$`12.2.1.B)¿Quién le parece que es el responsable de esta situación?`)


datos$responsable_problemas_salud<-datos$`12.2.1.B)¿Quién le parece que es el responsable de esta situación?`

datos$responsable_problemas_salud<-
  gsub("[)]", "", datos$responsable_problemas_salud)

datos$provincia<-str_extract(datos$responsable_problemas_salud,
                            "12.2.1.B.1 El gobierno provincial y sus hospitales")
datos$municipalidad<-str_extract(datos$responsable_problemas_salud,
                             "12.2.1.B.2el gobierno municipal y los dispensarios")
datos$nacion<-str_extract(datos$responsable_problemas_salud,
                             "12.2.1.B.4 el gobierno nacional y sus políticas")
datos$obras_sociales<-str_extract(datos$responsable_problemas_salud,
                             "12.2.1.B.3 las obras sociales y clínicas privadas")
datos$un_po_ca_un<-str_extract(datos$responsable_problemas_salud,
                             "12.2.1.B.5 un poco cada uno")
datos$no_sabe<-str_extract(datos$responsable_problemas_salud,
                             "12.2.1.B.6 NS/NC")
responsable1_abs<-addmargins(table(datos$provincia, datos$nse),2)
responsable2_abs<-addmargins(table(datos$municipalidad, datos$nse),2)
responsable3_abs<-addmargins(table(datos$nacion, datos$nse),2)
responsable4_abs<-addmargins(table(datos$obras_sociales, datos$nse),2)
responsable5_abs<-addmargins(table(datos$un_po_ca_un, datos$nse),2)
responsable6_abs<-addmargins(table(datos$no_sabe, datos$nse),2)

responsable_abs<-as.data.frame(rbind(responsable1_abs, responsable2_abs,
                                 responsable3_abs, responsable4_abs,
                                 responsable5_abs, responsable6_abs))

rownames(responsable_abs)<-c("El gobierno provincial y sus hospitales",
                         "El gobierno municipal y los dispensarios",
                         "El gobierno nacional y sus políticas",
"Las obras sociales y clínicas privadas", "Un poco cada uno",  "NS/NC")

responsable_abs
kable(credito,
      caption = "Opciones de crédito y financiación usadas durante la pandemia (porcentajes de respuestas por NSE")

table(datos$`10.1 Cuando se presenta un problema de salud a Ud. o a un miembro de su familia, ¿acuden a?:`)
datos$lugar_atencion_salud<-as.factor(datos$`10.1 Cuando se presenta un problema de salud a Ud. o a un miembro de su familia, ¿acuden a?:`)


datos$hospital_publico<-str_extract(datos$lugar_atencion_salud,
                             "10.1.1 Hospital Público")
datos$hospital_privado<-str_extract(datos$lugar_atencion_salud,
                                 "10.1.2 Hospital privado de mi Obra Social o Prepaga")
datos$dispensario<-str_extract(datos$lugar_atencion_salud,
                          "10.1.3 Dispensario barrial")
datos$medico_particular<-str_extract(datos$lugar_atencion_salud,
                                  "10.1.4 Médico particular")
datos$otras<-str_extract(datos$lugar_atencion_salud,
                               "10.1.6 Otras")
datos$no_sabe<-str_extract(datos$lugar_atencion_salud,
                           "10.1.5 Ns / NC")

table(datos
      $no_sabe)
atencion1_abs<-addmargins(table(datos$hospital_publico, datos$nse),2)
atencion2_abs<-addmargins(table(datos$hospital_privado, datos$nse),2)
atencion3_abs<-addmargins(table(datos$dispensario, datos$nse),2)
atencion4_abs<-addmargins(table(datos$medico_particular, datos$nse),2)
atencion5_abs<-addmargins(table(datos$otras, datos$nse),2)
atencion6_abs<-addmargins(table(datos$no_sabe, datos$nse),2)

atencion_abs<-as.data.frame(rbind(atencion1_abs, atencion2_abs,
                                  atencion3_abs, atencion4_abs,
                                  atencion5_abs, atencion6_abs))

rownames(atencion_abs)<-c("Hospital Público", "Hospital privado de mi Obra Social o Prepaga", 
"Dispensario barrial", "Médico particular", "Otras", "Ns / NC")
atencion_abs

# correlaciones
# con hacinamiento
modelo1<-aov(datos$personas_por_cuarto ~ datos$nse)
summary(modelo1)
TukeyHSD(modelo1)

modelo2<-lm(datos$personas_por_cuarto ~ datos$nse)
summary(modelo2)
# con ingresos
chisq.test(datos$ingreso, datos$nse)


table(datos$hogar_con_covid, datos$hacinamiento_critico)
t.test(datos$personas_por_cuarto~ datos$hogar_con_covid)
table(datos$hogar_con_covid, datos$ingreso)
table(datos$hogar_con_covid, datos$hogar_con_mayores)

table(datos$`4.1 En su hogar cuentan con [Telefonía celular]`)
table(datos$`4.1 En su hogar cuentan con [Telefonía fija]`)
levels(datos$tel_fijo)<-c("no", "si")
table(datos$tel_celular)
write.csv(table(datos$tel_celular, datos$nse), "celular.csv")
write.csv(table(datos$tel_fijo, datos$nse), "fijo.csv")
names(datos)
summary(datos$`1.1 Edad`)
table(datos$`Quién aporta el principal ingreso?`)
write.csv(fuente_de_ingresos_ppal_abs, "fuente_ingresos.csv")
table(datos$`7.3 ¿El trabajo de la persona que aporta el ingreso principal en el hogar, es de…? (leer opciones)`)
length(datos$`1.1 Edad`)


install.packages("sf")

datos$`Cantidad de grupos familiares que viven en esa casa`[
  datos$`Cantidad de grupos familiares que viven en esa casa`==13]<-1
addmargins(table(datos$`Cantidad de grupos familiares que viven en esa casa`))



write.csv(
  table(datos$`12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:`),
  "dificultades.csv")


12.3.1 Dificultades para acceder a turnos médicos por escasez de atención
12.3.2 Dificultades para solicitar turnos de manera virtual por falta de conectividad
12.3.3 Dificultades para solicitar turnos de manera virtual por problemas en los sistemas
12.3.4 Dificultades económicas para pagar consultas, medicamentos, análisis médicos, estudios y especialidades médicas
12.3.5 Posponer intervenciones quirúrgicas
12.3.6 Dificultades para llegar a algún servicio de salud por las restricciones a la movilidad
12.3.7 Otras
Dificultades para que los atiendan 
Llamaron al servicio de emergencia y no llegó 
Termino pagando a parte una operación por problemas de la obra social 
library(stringr)
datos$dificultades_salud_2020_falta_atencion<-
  str_extract(datos$`12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:`,
              "12.3.1 Dificultades para acceder a turnos médicos por escasez de atención")

datos$dificultades_salud_2020_falta_conectividad<-
  str_extract(datos$`12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:`,
              "12.3.2 Dificultades para solicitar turnos de manera virtual por falta de conectividad")

datos$dificultades_salud_2020_falta_sistemas<-
  str_extract(datos$`12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:`,
              "12.3.3 Dificultades para solicitar turnos de manera virtual por problemas en los sistemas")

datos$dificultades_salud_2020_pago_consultas<-
  str_extract(datos$`12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:`,
              "12.3.4 Dificultades económicas para pagar consultas, medicamentos, análisis médicos, estudios y especialidades médicas")

datos$dificultades_salud_2020_posponer_intervenciones<-
  str_extract(datos$`12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:`,
              "12.3.5 Posponer intervenciones quirúrgicas")

datos$dificultades_salud_2020_movilidad<-
  str_extract(datos$`12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:`,
              "12.3.6 Dificultades para llegar a algún servicio de salud por las restricciones a la movilidad")

datos$dificultades_salud_2020_otras<-
  str_extract(datos$`12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:`,
              "12.3.7 Otras")


#######
v<-addmargins(table(datos$nse))

dificultad1<-100*round(addmargins(table(datos$dificultades_salud_2020_falta_atencion, datos$nse), 2)[1:6]/v[1:6], 3)
dificultad2<-100*round(addmargins(table(datos$dificultades_salud_2020_falta_conectividad, datos$nse), 2)[1:6]/v[1:6],3)
dificultad3<-100*round(addmargins(table(datos$dificultades_salud_2020_falta_sistemas, datos$nse), 2)[1:6]/v[1:6],3)
dificultad4<-100*round(addmargins(table(datos$dificultades_salud_2020_pago_consultas, datos$nse), 2)[1:6]/v[1:6],3)
dificultad5<-100*round(addmargins(table(datos$dificultades_salud_2020_posponer_intervenciones, datos$nse), 2)[1:6]/v[1:6],3)
dificultad6<-100*round(addmargins(table(datos$dificultades_salud_2020_movilidad, datos$nse), 2)[1:6]/v[1:6],3)
dificultad7<-100*round(addmargins(table(datos$dificultades_salud_2020_otras, datos$nse), 2)[1:6]/v[1:6],3)
dificultades<-as.data.frame(rbind(dificultad1, dificultad2, dificultad3,
                                  dificultad4,dificultad5,dificultad6,dificultad7))

rownames(dificultades)<-c("Acceder a turnos médicos por escasez de atención",
"Solicitar turnos por falta de conectividad",
"Solicitar turnos por problemas en los sistemas",
"Pagar prestaciones", "Posponer intervenciones quirúrgicas",
"Llegar al servicio de salud por restricciones a la movilidad", "Otras")


kable(prestaciones,
      caption = "Prestaciones estatales percibdas por integrantes del hogar durante el último mes (porcentajes de respuestas por NSE")

table(datos$`5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Cámara de fotos]`)
names(datos)
