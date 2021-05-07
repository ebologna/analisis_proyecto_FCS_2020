rm(list=ls())
library(readxl)
library(dplyr)
datos = readxl::read_xlsx("Desigualdades y acceso a derechos (Respuestas).xlsx")
names(datos)                         
??rename
library(tidyverse)
datos = rename(datos,
               tiempo = "Marca temporal",
               encuestador = "Encuestador",
               NSSE = "Nivel Socio Económico",
               edad = "1.1 Edad",
               genero = "1.2 Género",
               tiene_DNI = "1.3 Actualmente, tiene DNI?",
               procedencia = "1.4 Procedencia",
               estado_civil = "1.5 Estado civil",
               tiene_electricidad = "La vivienda de su casa cuenta con [Luz Eléctrica]",
               tiene_gas_natural = "La vivienda de su casa cuenta con [Gas Natural]",
               tiene_gas_envasado = "La vivienda de su casa cuenta con [Gas envasado]",
               tiene_agua_potable = "La vivienda de su casa cuenta con [Agua potable]",
               tiene_cloacas = "La vivienda de su casa cuenta con [Cloacas]",
               como_calefacciona = "2.2 ¿Con qué calefaccionan mayormente su casa?",
               tiene_aire_acondicionado = "Tienen aire acondicionado?",
               cantidad_integrantes_con_cobertura_salud = "3.1 Cuántos integrantes del hogar tienen cobertura de salud? Como obra social, prepaga o pami",
               cobertura_salud = "3.2 Alguno de los integrantes del hogar tiene... (marcar todas las que señalen)",
               alcance_cobertura_salud = "3.3 Su cobertura de salud alcanza para:",
               tiene_servicio_emergencia = "3.4 Cuenta con un servicio médico de emergencia a domicilio?",
               tiene_ambulancia = "3.5 En caso de urgencias, puede acudir a un servicio de ambulancia?",
               tiene_tel_fijo = "4.1 En su hogar cuentan con [Telefonía fija]",
               tiene_celular = "4.1 En su hogar cuentan con [Telefonía celular]",
               tiene_internet = "4.2 En su casa cuentan con conectividad a internet a través de",
               cambio_acceso_internet = "4.3 En el último año, adquirieron nuevos paquetes o mejoraron el acceso a la conectividad?",
               tiene_PC = "5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [PC de escritorio]",
               tiene_notebook = "5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Notebook]",
               tiene_notebook_CI = "5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Notebook de Conectar Igualdad]",
               tiene_tablet = "5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Tablet / Ipad]",
               tiene_smart_TV = "5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Smart TV]",
               tiene_impresora = "5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Impresora]",
               tene_camara_fotos = "5.1 En su hogar, tienen alguno de los dispositivos tecnológicos que voy a nombrarle? [Cámara de fotos]",
               cuantos_celulares = "5.2 Cuántos teléfonos celulares hay en su hogar?",
               compro_dispositivo_ultimo_anio = "5.3 ¿En el último año se adquirieron en el hogar alguno de los dispositivos que le nombré anteriormente?",
               ingresos_del_hogar = "6.1 Me puede decir en promedio, ¿Cuáles son los ingresos totales por mes en el hogar?",
               fuente_ingresos = "6.2 En el último año, los ingresos de las personas provinieron de...?",
               estabilidad_ingresos = "6.3 En general, sus ingresos mensuales son",
               variacion_ingresos_pandemia = "6.4 Ahora le voy a preguntar ¿qué pasó con los ingresos familiares durante la pandemia? Le voy a dar algunas opciones y debe indicarme una.",
               prestaciones_estatales = "6.5 ¿Cuáles de las siguientes prestaciones estatales se percibieron en el último mes en el hogar? Puede marcar todas las que correspondan",
               alguien_trabaja_formal = "7.1 Le voy a mencionar distintas situaciones laborales y le voy a pedir que me diga si algún miembro del hogar trabaja de esa manera (leer opciones) [Formal registrada (en blanco, relación de dependencia, monotributo o autónomo)]",
               alguien_trabaja_informal = "7.1 Le voy a mencionar distintas situaciones laborales y le voy a pedir que me diga si algún miembro del hogar trabaja de esa manera (leer opciones) [Informal (en negro)]",
               alguien_trabaja_cuentapropia = "7.1 Le voy a mencionar distintas situaciones laborales y le voy a pedir que me diga si algún miembro del hogar trabaja de esa manera (leer opciones) [Por cuenta propia (sin registrar)]",
               alguien_trabaja_changas = "7.1 Le voy a mencionar distintas situaciones laborales y le voy a pedir que me diga si algún miembro del hogar trabaja de esa manera (leer opciones) [Eventual / por changas]",
               alguien_trabaja_negocio_propio = "7.1 Le voy a mencionar distintas situaciones laborales y le voy a pedir que me diga si algún miembro del hogar trabaja de esa manera (leer opciones) [Empresa o negocio propio]",
               alguien_no_trabaja_y_busca = "7.1 Le voy a mencionar distintas situaciones laborales y le voy a pedir que me diga si algún miembro del hogar trabaja de esa manera (leer opciones) [No tienen trabajo pero buscan]",
               origen_ingreso_principal = "7.2 ¿La persona que aporta el principal ingreso en el hogar, lo hace por?",
               intensidad_ingreso_principal = "7.3 ¿El trabajo de la persona que aporta el ingreso principal en el hogar, es de.? (leer opciones)",
               prestaciones_estatales_pandemia = "7.4 ¿Algún miembro del hogar percibió alguna de las prestaciones del Estado durante la Pandemia?",
               empleo_en_pandemia = "7.5 Durante el aislamiento social por la pandemia, ¿qué paso mayormente con el trabajo de los miembros de su hogar?",
               suficiencia_ingresos_pre_pandemia = "8.1 Antes de la pandemia, ¿los ingresos del hogar eran?",
               suficiencia_ingresos_en_pandemia = "8.2 Y durante la pandemia, ¿los ingresos del hogar son?",
               conformidad_laboral_pre_pandemia = "8.3 Antes de la pandemia, ¿cuán conforme estaba con la situación laboral de los miembros de su hogar?",
               conformidad_laboral_pandemia = "8.4 Actualmente, ¿cómo se siente con la situación laboral de su familia?",
               dificultades_empleo_pandemia = "8.5 ¿Tuvieron durante la pandemia algunas de las siguientes dificultades para llevar adelante su trabajo?",
               solicitud_ayuda_pandemia = "9.1 ¿Durante la pandemia, han solicitado algún tipo de ayuda en.?",
               difultad_actividades_pandemia = "9.2 Si tuviera que describir  cómo le resultó sobrellevar las  actividades laborales, domésticas, de cuidado de personas y acompañamiento escolar de hijxs en el último año",
               credito_pandemia = "9.3Durante la pandemia, ¿debió recurrir a algunas de las siguientes opciones de créditos y financiación?",
               atencion_salud_tipo = "10.1 Cuando se presenta un problema de salud a Ud. o a un miembro de su familia, ¿acuden a?:",
               atencion_salud_distancia = "10.2 Nos podría indicar a qué distancia aproximada de su casa (en cuadras) se ubica el centro de salud que utiliza?",
               atencion_salud_movilidad = "10.3 Cuando necesita atención médica, Ud o su familia se dirigen a ese centro de salud:",
               atencion_salud_dificultades_pre_pandemia = "11.1 Previo a la pandemia, tuvo alguna de las siguientes dificultades cuando necesitó algún servicio de salud? (leer opciones y marcar todas las que responda)",
               atencion_salud_durante_pandemia = "12.1 Durante la pandemia, ¿Ud o su familia tuvieron problemas de salud que demandaran atención médica?",
               atencion_salud_dificultades_pandemia = "12.2. Y en ese contexto, ¿tuvieron dificultades para acceder a los servicios de salud?",
               pandemia_responsabilidad = "12.2.1.B)¿Quién le parece que es el responsable de esta situación?",
               atencion_salud_dificultades_pre_pandemia_detalle = "12.3 Voy a leerle algunas dificultades que podrían haber aparecido al momento de buscar atención médica, y le voy a pedir que me señale todas aquellas que hayan vivido en su núcleo familiar durante el año pasado:",
               covid_positivo = "12.4 ¿Algún integrante de su familia tuvo COVID19?",
               covid_positivo_dificultades = "12.6 Tuvieron dificultades para: (leer opciones)",
               atencion_salud_acceso_informacion = "13.1  ¿Considera que tuvo información suficiente sobre los programas y servicios de salud durante la cuarentena? (leer opciones)",
               atencion_salud_acceso_informacion_origen = "13.2 ¿Nos podría decir de qué manera obtuvo ese tipo de información? (marcar todas las que señale el entrevistado)",
               educ_hogar_inicial = "14. En su hogar hay integrantes cursando [Jardín de infantes]",
               educ_hogar_primario = "14. En su hogar hay integrantes cursando [Primario]",
               educ_hogar_secundario = "14. En su hogar hay integrantes cursando [Secundario]",
               educ_hogar_universitario = "14. En su hogar hay integrantes cursando [Universitario o terciario]",
               educ_hogar_inicial_establecimiento = "14.2 A qué tipo de establecimiento, público o privado, asisten? [Preescolar]",
               educ_hogar_primario_establecimiento = "14.2 A qué tipo de establecimiento, público o privado, asisten? [Primario]",
               educ_hogar_secundario_establecimiento = "14.2 A qué tipo de establecimiento, público o privado, asisten? [Secundario ]",
               educ_hogar_universitario_establecimiento = "14.2 A qué tipo de establecimiento, público o privado, asisten? [Universitario o terciario]",
               educ_hogar_inicial_distancia = "14.3 ¿Nos podría indicar a qué distancia aproximada (en cuadras) de su hogar se ubica el establecimiento? [Pre escolar]",
               educ_hogar_primario_distancia = "14.3 ¿Nos podría indicar a qué distancia aproximada (en cuadras) de su hogar se ubica el establecimiento? [Primario]",
               educ_hogar_secundario_distancia = "14.3 ¿Nos podría indicar a qué distancia aproximada (en cuadras) de su hogar se ubica el establecimiento? [Secundario]",
               educ_hogar_universitario_distancia = "14.3 ¿Nos podría indicar a qué distancia aproximada (en cuadras) de su hogar se ubica el establecimiento? [Universitario o terciario]",
               educ_hogar_inicial_movilidad = "14.2	Para llevar o traer a los niños al jardín, lo hacen 14.3	Para llevar o traer los niños a la escuela... 14.4	Para ir al secundario 14.5	Para ir a la Universidad o terciario (sólo leer el nivel escolar que corresponda y leer las opciones) [Pre escolar]",
               educ_hogar_primario_movilidad = "14.2	Para llevar o traer a los niños al jardín, lo hacen 14.3	Para llevar o traer los niños a la escuela... 14.4	Para ir al secundario 14.5	Para ir a la Universidad o terciario (sólo leer el nivel escolar que corresponda y leer las opciones) [Primario]",
               educ_hogar_secundario_movilidad = "14.2	Para llevar o traer a los niños al jardín, lo hacen 14.3	Para llevar o traer los niños a la escuela... 14.4	Para ir al secundario 14.5	Para ir a la Universidad o terciario (sólo leer el nivel escolar que corresponda y leer las opciones) [Secundario]",
               educ_hogar_universitario_movilidad = "14.2	Para llevar o traer a los niños al jardín, lo hacen 14.3	Para llevar o traer los niños a la escuela... 14.4	Para ir al secundario 14.5	Para ir a la Universidad o terciario (sólo leer el nivel escolar que corresponda y leer las opciones) [Universitario o terciario]",
               educ_hogar_inicial_continuidad = "14.6. Durante la pandemia¿ qué tipo de  continuidad educativa tuvieron? [Pre escolar]",
               educ_hogar_primario_continuidad = "14.6. Durante la pandemia¿ qué tipo de  continuidad educativa tuvieron? [Primario]",
               educ_hogar_secundario_continuidad = "14.6. Durante la pandemia¿ qué tipo de  continuidad educativa tuvieron? [Secundario]",
               educ_hogar_universitario_continuidad = "14.6. Durante la pandemia¿ qué tipo de  continuidad educativa tuvieron? [Terciario o universitario]",
               educ_hogar_inicial_medios = "1.4.5	¿Nos podría indicar a través de qué medios se realizó esa continuidad? [Pre escolar]",
               educ_hogar_primario_continuidad_medios = "1.4.5	¿Nos podría indicar a través de qué medios se realizó esa continuidad? [Primario]",
               educ_hogar_secundario_medios = "1.4.5	¿Nos podría indicar a través de qué medios se realizó esa continuidad? [Secundario]",
               educ_hogar_universitario_medios = "1.4.5	¿Nos podría indicar a través de qué medios se realizó esa continuidad? [Universitario o terciario]",
               informacion_medidas_educativas_valoracion = "16.1 Durante la vigencia del aislamiento, ¿considera que la información obtenida sobre las medidas educativas en relación a los miembros de su hogar fue...?",
               informacion_medidas_educativas_origen = "16.2 ¿Nos podría indicar de qué manera logró informarse?",
               fecha = "Fecha",
               hora = "Hora",
               id_encuesta = "Número de encuesta",
               vivienda_tipo = "Tipo de vivienda",
               vivienda_ambientes = "Cantidad de ambientes",
               vivienda_habitacion = "Cantidad de ambientes usados para dormir",
               vivienda_tenencia = "Tenencia",
               vivienda_grupos = "Cantidad de grupos familiares que viven en esa casa",
               vivienda_miembros = "Cuántas personas viven en esa casa?",
               jefx_hogar = "Quién aporta el principal ingreso?",
               cant_menores_14 = "Cuántas son menores de 14 años?",
               cant_14_60 = "Cuántas tienen entre 14 y 60 años?",
               cant_mas_60 = "Cuántas tienen más de 60 años?",
               cant_perceptores = "Cuántas personas aportan ingresos?",
               cant_perceptores_varon = "Varones que aportan ingresos",
               cant_perceptores_mujer = "Mujeres que aportan ingresos",
               cant_estudia = "Cuántas personas están cursando algún nivel educativo actualmente?",
               menores_estudia = "Hay personas que tengan entre 3 y 18 años que no estén cursando ningún nivel educativo?",
               discapacidad = "En su casa, hay personas con algún tipo de discapacidad?",
               discapacidad_certificado = "Tienen certificado de discapacidad?",
               discapacidad_pension = "Cuentan con pensión por discapacidad?",
               nombre = "NOmbre de la persona",
               telefono = "Teléfono",
               residencia = "Residencia:",
               cant_varones = "Cuántos son varones?",
               cant_mujeres = "Cuántos son mujeres?",
               cant_educ_inicial = "Personas cursando preescolar",
               cant_educ_primaria = "Personas cursando primaria",
               cant_educ_secundaria = "Personas cursando secundaria",
               cant_educ_universitaria = "Personas cursando universidad o terciario",
               barrio = "Barrio",
               nro_orden_hogar = "Número de orden del hogar encuestado",
               atencion_salud_covid_tipo = "12.5 Ante los síntomas de la enfermedad, a dónde  se atendieron?",
               pandemia_responsabilidad_control = "8.4.3 B Quién debería hacer algo para mejorar esto?",
               pandemia_dificultades_conectividad = "15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ¿Tuvieron alguna de estas dificultades? [15.1.1 Falta de conectividad / falta de datos para sostener las actividades virtuales - Si - No]",
               pandemia_dificultades_sin_dispositivos = "15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ¿Tuvieron alguna de estas dificultades? [15.1.2 No tener dispositivos digitales para sostener las actividades - Si - No]",
               pandemia_dificultades_pocos_dispositivos = "15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ¿Tuvieron alguna de estas dificultades? [15.1.3 Dispositivos digitales insuficientes para sostener las actividades de los distintos miembros del hogar - Si - No]",
               pandemia_dificultades_espacio = "15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ¿Tuvieron alguna de estas dificultades? [15.1.4 Dificultades vinculadas a la falta de espacios adecuados para que los distintos miembros del hogar sostengan la continuidad - Si - No]",
               pandemia_dificultades_brecha_digital_tutores = "15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ¿Tuvieron alguna de estas dificultades? [15.1.5 Dificultades en el conocimiento sobre dispositivos digitales de los padres y personas mayores para acompañar a quienes se están educando - Si - No]",
               pandemia_dificultades_economicas = "15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ¿Tuvieron alguna de estas dificultades? [15.1.6 Dificultades económicas para sostener cuotas escolares - Si - No]",
               pandemia_dificultades_tiempo = "15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ¿Tuvieron alguna de estas dificultades? [15.1.7 Dificultades de tiempo de los adultos para acompañar a los estudiantes del hogar - Si - No]",
               pandemia_dificultades_conectividad_otro = "15.1 Ahora le voy a preguntar sobre las dificultades que hubo en su hogar para sostener la continuidad educativa durante la virtualidad. ¿Tuvieron alguna de estas dificultades? [15.1.8 Otras]",
)




datos=datos[,c("tiempo",
               "encuestador",
               "NSSE",
               "edad",
               "genero",
               "tiene_DNI",
               "procedencia",
               "estado_civil",
               "tiene_electricidad",
               "tiene_gas_natural",
               "tiene_gas_envasado",
               "tiene_agua_potable",
               "tiene_cloacas",
               "como_calefacciona",
               "tiene_aire_acondicionado",
               "cantidad_integrantes_con_cobertura_salud",
               "cobertura_salud",
               "alcance_cobertura_salud",
               "tiene_servicio_emergencia",
               "tiene_ambulancia",
               "tiene_tel_fijo",
               "tiene_celular",
               "tiene_internet",
               "cambio_acceso_internet",
               "tiene_PC",
               "tiene_notebook",
               "tiene_notebook_CI",
               "tiene_tablet",
               "tiene_smart_TV",
               "tiene_impresora",
               "tene_camara_fotos",
               "cuantos_celulares",
               "compro_dispositivo_ultimo_anio",
               "ingresos_del_hogar",
               "fuente_ingresos",
               "estabilidad_ingresos",
               "variacion_ingresos_pandemia",
               "prestaciones_estatales",
               "alguien_trabaja_formal",
               "alguien_trabaja_informal",
               "alguien_trabaja_cuentapropia",
               "alguien_trabaja_changas",
               "alguien_trabaja_negocio_propio",
               "alguien_no_trabaja_y_busca",
               "origen_ingreso_principal",
               "intensidad_ingreso_principal",
               "prestaciones_estatales_pandemia",
               "empleo_en_pandemia",
               "suficiencia_ingresos_pre_pandemia",
               "suficiencia_ingresos_en_pandemia",
               "conformidad_laboral_pre_pandemia",
               "conformidad_laboral_pandemia",
               "dificultades_empleo_pandemia",
               "solicitud_ayuda_pandemia",
               "difultad_actividades_pandemia",
               "credito_pandemia",
               "atencion_salud_tipo",
               "atencion_salud_distancia",
               "atencion_salud_movilidad",
               "atencion_salud_dificultades_pre_pandemia",
               "atencion_salud_durante_pandemia",
               "atencion_salud_dificultades_pandemia",
               "pandemia_responsabilidad",
               "atencion_salud_dificultades_pre_pandemia_detalle",
               "covid_positivo",
               "covid_positivo_dificultades",
               "atencion_salud_acceso_informacion",
               "atencion_salud_acceso_informacion_origen",
               "educ_hogar_inicial",
               "educ_hogar_primario",
               "educ_hogar_secundario",
               "educ_hogar_universitario",
               "educ_hogar_inicial_establecimiento",
               "educ_hogar_primario_establecimiento",
               "educ_hogar_secundario_establecimiento",
               "educ_hogar_universitario_establecimiento",
               "educ_hogar_inicial_distancia",
               "educ_hogar_primario_distancia",
               "educ_hogar_secundario_distancia",
               "educ_hogar_universitario_distancia",
               "educ_hogar_inicial_movilidad",
               "educ_hogar_primario_movilidad",
               "educ_hogar_secundario_movilidad",
               "educ_hogar_universitario_movilidad",
               "educ_hogar_inicial_continuidad",
               "educ_hogar_primario_continuidad",
               "educ_hogar_secundario_continuidad",
               "educ_hogar_universitario_continuidad",
               "educ_hogar_inicial_medios",
               "educ_hogar_primario_continuidad_medios",
               "educ_hogar_secundario_medios",
               "educ_hogar_universitario_medios",
               "informacion_medidas_educativas_valoracion",
               "informacion_medidas_educativas_origen",
               "fecha",
               "hora",
               "id_encuesta",
               "vivienda_tipo",
               "vivienda_ambientes",
               "vivienda_habitacion",
               "vivienda_tenencia",
               "vivienda_grupos",
               "vivienda_miembros",
               "jefx_hogar",
               "cant_menores_14",
               "cant_14_60",
               "cant_mas_60",
               "cant_perceptores",
               "cant_perceptores_varon",
               "cant_perceptores_mujer",
               "cant_estudia",
               "menores_estudia",
               "discapacidad",
               "discapacidad_certificado",
               "discapacidad_pension",
               "nombre",
               "telefono",
               "residencia",
               "cant_varones",
               "cant_mujeres",
               "cant_educ_inicial",
               "cant_educ_primaria",
               "cant_educ_secundaria",
               "cant_educ_universitaria",
               "barrio",
               "nro_orden_hogar",
               "atencion_salud_covid_tipo",
               "pandemia_responsabilidad_control",
               "pandemia_dificultades_conectividad",
               "pandemia_dificultades_sin_dispositivos",
               "pandemia_dificultades_pocos_dispositivos",
               "pandemia_dificultades_espacio",
               "pandemia_dificultades_brecha_digital_tutores",
               "pandemia_dificultades_economicas",
               "pandemia_dificultades_tiempo",
               "pandemia_dificultades_conectividad_otro")]


datos$fuente_ingresos = as.factor(gsub("( IFE, Progresar, PPP)", "", datos$fuente_ingresos))
datos$fuente_ingresos = gsub(" ", "", datos$fuente_ingresos)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$fuente_ingresos), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_fuente_ingresos.csv")
datos = datos %>% 
  mutate(fuente_ingresos_01 = ifelse(grepl(as.character(valores[1,]), datos$fuente_ingresos, fixed = TRUE)==TRUE, 1, 0),
         fuente_ingresos_02 = ifelse(grepl(as.character(valores[2,]), datos$fuente_ingresos, fixed = TRUE)==TRUE, 1, 0),
         fuente_ingresos_03 = ifelse(grepl(as.character(valores[3,]), datos$fuente_ingresos, fixed = TRUE)==TRUE, 1, 0),
         fuente_ingresos_04 = ifelse(grepl(as.character(valores[4,]), datos$fuente_ingresos, fixed = TRUE)==TRUE, 1, 0),
         fuente_ingresos_05 = ifelse(grepl(as.character(valores[5,]), datos$fuente_ingresos, fixed = TRUE)==TRUE, 1, 0))



datos$cobertura_salud = gsub(" ", "", datos$cobertura_salud)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$cobertura_salud), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_cobertura_salud.csv")
datos = datos %>% 
  mutate(cobertura_salud_01 = ifelse(grepl(as.character(valores[1,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_02 = ifelse(grepl(as.character(valores[2,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_03 = ifelse(grepl(as.character(valores[3,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_04 = ifelse(grepl(as.character(valores[4,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_05 = ifelse(grepl(as.character(valores[5,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_06 = ifelse(grepl(as.character(valores[6,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_07 = ifelse(grepl(as.character(valores[7,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_08 = ifelse(grepl(as.character(valores[8,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0)
         )

datos$alcance_cobertura_salud = gsub(" ", "", datos$alcance_cobertura_salud)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$alcance_cobertura_salud), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_alcance_cobertura_salud.csv")
datos = datos %>% 
  mutate(alcance_cobertura_salud_01 = ifelse(grepl(as.character(valores[1,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_02 = ifelse(grepl(as.character(valores[2,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_03 = ifelse(grepl(as.character(valores[3,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_04 = ifelse(grepl(as.character(valores[4,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_05 = ifelse(grepl(as.character(valores[5,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_06 = ifelse(grepl(as.character(valores[6,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_07 = ifelse(grepl(as.character(valores[7,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_08 = ifelse(grepl(as.character(valores[8,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_09 = ifelse(grepl(as.character(valores[9,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_10 = ifelse(grepl(as.character(valores[10,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_11 = ifelse(grepl(as.character(valores[11,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         alcance_cobertura_salud_12 = ifelse(grepl(as.character(valores[12,]), datos$alcance_cobertura_salud, fixed = TRUE)==TRUE, 1, 0))


datos$prestaciones_estatales = gsub(" ", "", datos$prestaciones_estatales)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$prestaciones_estatales), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_prestaciones_estatales.csv")
datos = datos %>% 
  mutate(prestaciones_estatales_01 = ifelse(grepl(as.character(valores[2,]), datos$prestaciones_estatales, fixed = TRUE)==TRUE, 1, 0),
         prestaciones_estatales_02 = ifelse(grepl(as.character(valores[3,]), datos$prestaciones_estatales, fixed = TRUE)==TRUE, 1, 0),
         prestaciones_estatales_03 = ifelse(grepl(as.character(valores[4,]), datos$prestaciones_estatales, fixed = TRUE)==TRUE, 1, 0),
         prestaciones_estatales_04 = ifelse(grepl(as.character(valores[5,]), datos$prestaciones_estatales, fixed = TRUE)==TRUE |
                          grepl(as.character(valores[1,]), datos$prestaciones_estatales, fixed = TRUE)==TRUE, 1, 0))


datos$origen_ingreso_principal = gsub(" ", "", datos$origen_ingreso_principal)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$origen_ingreso_principal), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_origen_ingreso_principal.csv")
datos = datos %>% 
  mutate(origen_ingreso_principal_01 = ifelse(grepl(as.character(valores[1,]), datos$origen_ingreso_principal, fixed = TRUE)==TRUE, 1, 0),
         origen_ingreso_principal_02 = ifelse(grepl(as.character(valores[2,]), datos$origen_ingreso_principal, fixed = TRUE)==TRUE, 1, 0),
         origen_ingreso_principal_03 = ifelse(grepl(as.character(valores[3,]), datos$origen_ingreso_principal, fixed = TRUE)==TRUE, 1, 0),
         origen_ingreso_principal_04 = ifelse(grepl(as.character(valores[4,]), datos$origen_ingreso_principal, fixed = TRUE)==TRUE, 1, 0),
         origen_ingreso_principal_05 = ifelse(grepl(as.character(valores[5,]), datos$origen_ingreso_principal, fixed = TRUE)==TRUE, 1, 0),
         origen_ingreso_principal_06 = ifelse(grepl(as.character(valores[6,]), datos$origen_ingreso_principal, fixed = TRUE)==TRUE, 1, 0),
         origen_ingreso_principal_07 = ifelse(grepl(as.character(valores[7,]), datos$origen_ingreso_principal, fixed = TRUE)==TRUE, 1, 0))


datos$solicitud_ayuda_pandemia = gsub(" ", "", datos$solicitud_ayuda_pandemia)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$solicitud_ayuda_pandemia), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_solicitud_ayuda_pandemia.csv")
datos = datos %>% 
  mutate(solicitud_ayuda_pandemia_01 = ifelse(grepl(as.character(valores[1,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_02 = ifelse(grepl(as.character(valores[2,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_03 = ifelse(grepl(as.character(valores[3,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_04 = ifelse(grepl(as.character(valores[4,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_05 = ifelse(grepl(as.character(valores[5,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_06 = ifelse(grepl(as.character(valores[6,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_07 = ifelse(grepl(as.character(valores[7,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_08 = ifelse(grepl(as.character(valores[8,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_09 = ifelse(grepl(as.character(valores[9,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0),
         solicitud_ayuda_pandemia_10 = ifelse(grepl(as.character(valores[10,]), datos$solicitud_ayuda_pandemia, fixed = TRUE)==TRUE, 1, 0))



datos$atencion_salud_dificultades_pre_pandemia = gsub(" ", "", datos$atencion_salud_dificultades_pre_pandemia)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$atencion_salud_dificultades_pre_pandemia), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_atencion_salud_dificultades_pre_pandemia.csv")
datos = datos %>% 
  mutate(atencion_salud_dificultades_pre_pandemia_01 = ifelse(grepl(as.character(valores[1,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_02 = ifelse(grepl(as.character(valores[2,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_03 = ifelse(grepl(as.character(valores[3,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_04 = ifelse(grepl(as.character(valores[4,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_05 = ifelse(grepl(as.character(valores[5,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_06 = ifelse(grepl(as.character(valores[6,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_07 = ifelse(grepl(as.character(valores[7,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_08 = ifelse(grepl(as.character(valores[8,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_09 = ifelse(grepl(as.character(valores[9,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_10 = ifelse(grepl(as.character(valores[10,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_11 = ifelse(grepl(as.character(valores[11,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_12 = ifelse(grepl(as.character(valores[12,]), datos$atencion_salud_dificultades_pre_pandemia, fixed = TRUE)==TRUE, 1, 0))



datos$atencion_salud_dificultades_pre_pandemia_detalle = gsub(" ", "", datos$atencion_salud_dificultades_pre_pandemia_detalle)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$atencion_salud_dificultades_pre_pandemia_detalle), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_atencion_salud_dificultades_pre_pandemia_detalle.csv")
datos = datos %>% 
  mutate(atencion_salud_dificultades_pre_pandemia_detalle_01 = ifelse(grepl(as.character(valores[1,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_02 = ifelse(grepl(as.character(valores[2,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_03 = ifelse(grepl(as.character(valores[3,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_04 = ifelse(grepl(as.character(valores[4,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_05 = ifelse(grepl(as.character(valores[5,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_06 = ifelse(grepl(as.character(valores[6,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_07 = ifelse(grepl(as.character(valores[7,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_08 = ifelse(grepl(as.character(valores[8,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_09 = ifelse(grepl(as.character(valores[9,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_10 = ifelse(grepl(as.character(valores[10,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_11 = ifelse(grepl(as.character(valores[11,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_12 = ifelse(grepl(as.character(valores[12,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_dificultades_pre_pandemia_detalle_13 = ifelse(grepl(as.character(valores[13,]), datos$atencion_salud_dificultades_pre_pandemia_detalle, fixed = TRUE)==TRUE, 1, 0))



datos$covid_positivo_dificultades = gsub(" ", "", datos$covid_positivo_dificultades)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$covid_positivo_dificultades), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_covid_positivo_dificultades.csv")
datos = datos %>% 
  mutate(covid_positivo_dificultades_01 = ifelse(grepl(as.character(valores[1,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_02 = ifelse(grepl(as.character(valores[2,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_03 = ifelse(grepl(as.character(valores[3,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_04 = ifelse(grepl(as.character(valores[4,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_05 = ifelse(grepl(as.character(valores[5,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_06 = ifelse(grepl(as.character(valores[6,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_07 = ifelse(grepl(as.character(valores[7,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_08 = ifelse(grepl(as.character(valores[8,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_09 = ifelse(grepl(as.character(valores[9,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0),
         covid_positivo_dificultades_10 = ifelse(grepl(as.character(valores[10,]), datos$covid_positivo_dificultades, fixed = TRUE)==TRUE, 1, 0))



datos$atencion_salud_acceso_informacion_origen = gsub(" ", "", datos$atencion_salud_acceso_informacion_origen)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$atencion_salud_acceso_informacion_origen), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_atencion_salud_acceso_informacion_origen.csv")
datos = datos %>% 
  mutate(atencion_salud_acceso_informacion_origen_01 = ifelse(grepl(as.character(valores[1,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_acceso_informacion_origen_02 = ifelse(grepl(as.character(valores[2,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_acceso_informacion_origen_03 = ifelse(grepl(as.character(valores[3,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_acceso_informacion_origen_04 = ifelse(grepl(as.character(valores[4,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE |
                                                              grepl(as.character(valores[5,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE |
                                                              grepl(as.character(valores[6,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_acceso_informacion_origen_05 = ifelse(grepl(as.character(valores[7,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE |
                                                              grepl(as.character(valores[8,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE |
                                                              grepl(as.character(valores[9,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_acceso_informacion_origen_6 = ifelse(grepl(as.character(valores[10,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE, 1, 0),
         atencion_salud_acceso_informacion_origen_7 = ifelse(grepl(as.character(valores[11,]), datos$atencion_salud_acceso_informacion_origen, fixed = TRUE)==TRUE, 1, 0))



datos$informacion_medidas_educativas_origen = gsub(" ", "", datos$informacion_medidas_educativas_origen)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$informacion_medidas_educativas_origen), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_informacion_medidas_educativas_origen.csv")
datos = datos %>% 
  mutate(informacion_medidas_educativas_origen_01 = ifelse(grepl(as.character(valores[1,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE, 1, 0),
         informacion_medidas_educativas_origen_02 = ifelse(grepl(as.character(valores[2,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE, 1, 0),
         informacion_medidas_educativas_origen_03 = ifelse(grepl(as.character(valores[3,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE |
                                                           grepl(as.character(valores[4,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE |
                                                           grepl(as.character(valores[5,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE, 1, 0),
         informacion_medidas_educativas_origen_04 = ifelse(grepl(as.character(valores[6,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE |
                                                           grepl(as.character(valores[7,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE |
                                                           grepl(as.character(valores[8,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE, 1, 0),
         informacion_medidas_educativas_origen_05 = ifelse(grepl(as.character(valores[9,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE, 1, 0),
         informacion_medidas_educativas_origen_6 = ifelse(grepl(as.character(valores[10,]), datos$informacion_medidas_educativas_origen, fixed = TRUE)==TRUE, 1, 0))


datos$cobertura_salud = gsub(" ", "", datos$cobertura_salud)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$cobertura_salud), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_cobertura_salud.csv")
datos = datos %>% 
  mutate(cobertura_salud_01 = ifelse(grepl(as.character(valores[1,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_02 = ifelse(grepl(as.character(valores[2,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_03 = ifelse(grepl(as.character(valores[3,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_04 = ifelse(grepl(as.character(valores[4,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_05 = ifelse(grepl(as.character(valores[5,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_06 = ifelse(grepl(as.character(valores[6,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_07 = ifelse(grepl(as.character(valores[7,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_08 = ifelse(grepl(as.character(valores[8,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0))


datos$alcance_cobertura_salud = gsub(" ", "", datos$alcance_cobertura_salud)
valores <- na.omit(unique(unlist(strsplit(as.character(datos$alcance_cobertura_salud), ","))))
valores = as.data.frame(valores)
valores
write.csv(valores, "valores_cobertura_salud.csv")
datos = datos %>% 
  mutate(cobertura_salud_01 = ifelse(grepl(as.character(valores[1,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_02 = ifelse(grepl(as.character(valores[2,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_03 = ifelse(grepl(as.character(valores[3,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_04 = ifelse(grepl(as.character(valores[4,]), datos$cobertura_salud, fixed = TRUE)==TRUE | grepl(as.character(valores[5,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_05 = ifelse(grepl(as.character(valores[6,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_06 = ifelse(grepl(as.character(valores[7,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_07 = ifelse(grepl(as.character(valores[8,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_08 = ifelse(grepl(as.character(valores[9,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_09 = ifelse(grepl(as.character(valores[10,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_10 = ifelse(grepl(as.character(valores[11,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_11 = ifelse(grepl(as.character(valores[12,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0),
         cobertura_salud_12 = ifelse(grepl(as.character(valores[13,]), datos$cobertura_salud, fixed = TRUE)==TRUE, 1, 0))


for(i in 1:ncol(datos)){
  freq = datos %>% 
    group_by(datos[,i]) %>% 
    summarise(n = n(),
              freq = n / nrow(datos)) %>% 
    as.data.frame()
  assign(paste0("freq",i), freq)
}
