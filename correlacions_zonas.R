# con hacinamiento
modelo1<-aov(datos$personas_por_cuarto ~ datos$nse)
summary(modelo1)
TukeyHSD(modelo1)

modelo2<-lm(datos$personas_por_cuarto ~ datos$nse)
summary(modelo2)


# con ingresos
chisq.test(datos$ingreso, datos$nse)

# personas en el hogar

modelo3<-aov(datos$`Cuántas personas viven en esa casa?`~ datos$nse)
summary(modelo3)
TukeyHSD(modelo1)

modelo4<-lm(datos$`Cuántas personas viven en esa casa?` ~ datos$nse)
summary(modelo4)

summary(datos$`Cuántas personas viven en esa casa?`)
