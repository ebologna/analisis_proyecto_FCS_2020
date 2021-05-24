rm(list=ls())
# setwd("C:/Users/juan/Downloads/")
datos = read.table("Desigualdades y acceso a derechos (Respuestas) - Respuestas de formulario 1.tsv",
                   header = T, sep = "\t", fileEncoding = "UTF-8", check.names=F)

aux <- data.frame(lapply(datos, function(x) {
  sub(", ", " / ", x)}))
colnames(datos) = gsub("\t", " ", colnames(datos))
colnames(aux) <- colnames(datos)
names(datos)
datos = aux ; rm(aux)

#### Tabla dificultades en la continuidad educativa por tipo de establecimiento

which( colnames(datos)=="15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.1 Falta de conectividad / falta de datos para sostener las actividades virtuales - Si - No]" )
datos$falta_conectividad = datos[,140]
which( colnames(datos)=="15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.2 No tener dispositivos digitales para sostener las actividades - Si - No]")
datos$falta_dispositivos = datos[,141]
which( colnames(datos)=="15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.3 Dispositivos digitales insuficientes para sostener las actividades de los distintos miembros del hogar - Si - No]" )
datos$dispositivos_insuficientes = datos[,142]
#which( colnames(datos)=="15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.2 No tener dispositivos digitales para sostener las actividades - Si - No]" )
datos$falta_espacios = datos[,143]
#which( colnames(datos)=="15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.1 Falta de conectividad / falta de datos para sostener las actividades virtuales - Si - No]" )
datos$conocimiento_dispositivos_acompañantes = datos[,144]
#which( colnames(datos)=="15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.1 Falta de conectividad / falta de datos para sostener las actividades virtuales - Si - No]" )
datos$dificultades_economicas = datos[,145]
#which( colnames(datos)=="15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.1 Falta de conectividad / falta de datos para sostener las actividades virtuales - Si - No]" )
datos$dificultades_tiempo = datos[,146]
#which( colnames(datos)=="15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.1 Falta de conectividad / falta de datos para sostener las actividades virtuales - Si - No]" )
datos$otras_dificultades = datos[,140]


# 
# datos$falta_conectividad = datos$`15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.1 Falta de conectividad / falta de datos para sostener las actividades virtuales - Si - No]`
# datos$falta_dispositivos = datos$`15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.2 No tener dispositivos digitales para sostener las actividades - Si - No]`
# datos$dispositivos_insuficientes = datos$`15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.3 Dispositivos digitales insuficientes para sostener las actividades de los distintos miembros del hogar - Si - No]`
# datos$falta_espacios = datos$`15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.4 Dificultades vinculadas a la falta de espacios adecuados para que los distintos miembros del hogar sostengan la continuidad - Si - No]`
# datos$conocimiento_dispositivos_acompa?antes = datos$`15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.5 Dificultades en el conocimiento sobre dispositivos digitales de los padres y personas mayores para acompa?ar a quienes se est?n educando - Si - No]`
# datos$dificultades_economicas = datos$`15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.6 Dificultades econ?micas para sostener cuotas escolares - Si - No]`
# datos$dificultades_tiempo = datos$`15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.7 Dificultades de tiempo de los adultos para acompa?ar a los estudiantes del hogar - Si - No]`
# datos$otras_dificultades = datos$`15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ?Tuvieron alguna de estas dificultades? [15.1.8 Otras]`


which( colnames(datos)=="14.2 A qu? tipo de establecimiento, p?blico o privado, asisten? [Preescolar]")
datos$preescolar = datos[,79]
which( colnames(datos)=="14.2 A qu? tipo de establecimiento, p?blico o privado, asisten? [Primario]")
datos$primario = datos[,80]
datos$secundario = datos[,81]
datos$universitario = datos[,82]

# datos$preescolar = datos$`14.2 A qu? tipo de establecimiento, p?blico o privado, asisten? [Preescolar]`
# datos$primario = datos$`14.2 A qu? tipo de establecimiento, p?blico o privado, asisten? [Primario]`
# datos$secundario = datos$`14.2 A qu? tipo de establecimiento, p?blico o privado, asisten? [Secundario ]`
# datos$universitario = datos$`14.2 A qu? tipo de establecimiento, p?blico o privado, asisten? [Universitario o terciario]`

aux = datos[,c("falta_conectividad","falta_dispositivos","dispositivos_insuficientes",
                 "falta_espacios","conocimiento_dispositivos_acompa?antes",
                 "dificultades_economicas","dificultades_tiempo","otras_dificultades",
                 "preescolar","primario","secundario","universitario")]
aux$id_hogar = 1:nrow(aux)
library(reshape2)
aux = melt(aux, id.vars = c("id_hogar","preescolar","primario","secundario","universitario"))
aux = dplyr::rename(aux, dificultad = variable, valor = value)
aux = subset(aux, valor=="S?")
aux = melt(aux, id.vars = c("id_hogar","dificultad","valor"))
aux = subset(aux, value=="P?blico" | value=="Privado")
names(aux)
aux = dplyr::rename(aux, nivel = variable, condicion = value)
aux = aux[,c("id_hogar","dificultad","nivel","condicion")]


kable_styling(kable(100*round(addmargins(prop.table(addmargins(table(aux$dificultad, aux$condicion),2),2),1),3), caption = "Dificultades en la continuidad educativa (porcentajes por tipo de establecimiento)"), latex_options = "hold_position")
kable_styling(kable(addmargins(table(aux$dificultad, aux$condicion)), caption = "Dificultades en la continuidad educativa por tipo de establecimiento (absolutos)"), latex_options = "hold_position")


#### Tabla g?nero del principal aportante
# table(datos$`1.2 G?nero`)
# table(datos$`Qui?n aporta el principal ingreso?`)
which( colnames(datos)=="Qui?n aporta el principal ingreso?")
which( colnames(datos)=="1.2 G?nero")


datos$sexo_aportante = ifelse(datos[,110]=="Padre de famiilia" |
                                (datos[,110]=="Uno mismo" &
                                   datos[,5]=="Var?n"), "Var?n",
                                 ifelse(datos[,110]=="Madre de familia" |
                                          (datos[,110]=="Uno mismo" &
                                             datos[,5]=="Mujer"), "Mujer","No especificado"))

### 


datos$nse<-as.factor(datos$"Nivel Socio Econ?mico")

datos$nse<-factor(datos$nse, levels=c("01. Alto", "02. Medio alto", "03.Medio",
                                      "0.4 Medio Bajo", "0.5  Bajo"))
levels(datos$nse)<-c("Alto", "Medio alto", "Medio",
                     "Medio Bajo", "Bajo")

###
kable_styling(kable(100*round(addmargins(prop.table(addmargins(table(datos$sexo_aportante, datos$nse),2),2),1),3), caption = "Sexo del principal aportante de ingresos del hogar (porcentajes por NSE)"), latex_options = "hold_position")
kable_styling(kable(addmargins(table(datos$sexo_aportante, datos$nse)), caption = "Sexo del principal aportante de ingresos del hogar (absolutos)"), latex_options = "hold_position")

