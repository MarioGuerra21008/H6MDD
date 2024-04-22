# Cargar datos desde un archivo CSV
datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")

View(datos$MiscVal)

summary(datos)

View(datos)

datos_para_clustering <- datos[, c("LotFrontage", "LotArea", "OverallQual", "OverallCond", "YearBuilt",
                                   "YearRemodAdd", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath",
                                   "TotRmsAbvGrd", "Fireplaces", "GarageYrBlt", "GarageCars", "GarageArea",
                                   "PoolArea", "MiscVal", "MoSold", "YrSold", "SalePrice")]

datos_para_clustering <- na.omit(datos_para_clustering) 








normalized_data <- scale(datos_para_clustering)

View(normalized_data)

#Obtener cantidad de clusteres
set.seed(123)
k_values <- 1:10
iner <- numeric(length(k_values))

for (k in k_values) {
  model <- kmeans(normalized_data, centers = k)
  iner[k] <- model$tot.withinss
}

plot(k_values, iner, type = "b", main = "Método del Codo", xlab = "Número de Clústeres (k)", ylab = "Inercia")
abline(v = which.min(diff(iner) > 10) + 1, col = "red", lty = 2)

#Al obtener la cantidad de clusters, realizamos K-Means para encontrar los grupos.

set.seed(123)
num_clusters <- 2  # Número de clústeres determinado anteriormente

# Aplicar el algoritmo de k-means
kmeans_model <- kmeans(normalized_data, centers = num_clusters)

# Añadir las etiquetas de clúster al conjunto de datos
normalized_data$kmeans_cluster <- as.factor(kmeans_model$cluster)

# Visualizar el resultado del clustering
table(normalized_data$kmeans_cluster)

#
# División de grupos train y test para modelos de regresión lineal
#

porcentaje <- 0.52 #Porcentaje con el que se calcularán los grupos de train y test.
datos_cuantitativos<- datos_para_clustering
corte <- sample(nrow(datos_cuantitativos),nrow(datos_cuantitativos)*porcentaje)
train<-datos_cuantitativos[corte,] #Corte para el grupo entrenamiento
test<-datos_cuantitativos[-corte,] #Corte para el grupo prueba

head(train)
head(test)

#Creación del modelo de regresión lineal.
single_linear_model<- lm(SalePrice~OverallQual, data = train) #Modelo lineal singular para SalePrice y OverallQual
summary(single_linear_model)

#Análisis de residuos

head(single_linear_model$residuals)

boxplot(single_linear_model$residuals)

# Análisis de predicción en conjunto prueba.

predSLM<-predict(single_linear_model, newdata = test)
head(predSLM)
length(predSLM)

# Gráfico del Modelo de Regresión Lineal Simple

library(ggplot2)
ggplot(data = train, mapping = aes(x = OverallQual, y = SalePrice)) +
  geom_point(color = "lightgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Calidad Promedio del Material x Precio de Venta", x = "Calidad Promedio", y = "Precio de Venta") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


#Modelo lineal múltiple para SalePrice.
multiple_linear_model<-lm(SalePrice~.,data = train)

summary(multiple_linear_model)


# Analisis de la correlacción

var_independients <- train[, -which(names(train) == "SalePrice")]

correlation_matrix <- cor(var_independients)

print(correlation_matrix)







#HOJA DE TRABAJO 4

# Cargar datos desde un archivo CSV
datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")
datos <- datos[, -1]

View(datos)

porcentaje4 <- 0.70
trainRowsNumber<-sample(1:nrow(datos),porcentaje4*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]

data_tree <- datos
summary(data_tree)
library(rpart)

#Instalar paquetes con rplot
#install.packages("rpart.plot")
library(rpart.plot)

#Crear nuestro modelo_arbol
modelo_arbol <- rpart(SalePrice~.,data=data_tree)

summary(modelo_arbol)

#Mostrar el arbol con la data de SalePrice
rpart.plot(modelo_arbol, digits = 3, fallen.leaves = TRUE)

#Predicción y análisis del resultado.

modelo_train <- rpart(SalePrice~.,data=train)
rpart.plot(modelo_train, digits = 3, fallen.leaves = TRUE)

prediccion <- predict(modelo_arbol, newdata = test)

head(prediccion)

mse <- mean((test$SalePrice - prediccion)^2)
print(paste("Error Cuadrático Medio (MSE):", mse))

r_cuadrado <- 1 - mse / var(test$SalePrice)
print(paste("Coeficiente de Determinación (R^2):", r_cuadrado))

#Añadir 3 modelos más:

# Crear una lista de modelos con diferentes profundidades
modelos <- list()
for (depth in 1:3) {
  modelos[[depth]] <- rpart(SalePrice~., data = data_tree, control = rpart.control(maxdepth = depth))
}
# Evaluar los modelos
for (i in 1:length(modelos)) {
  predicciones <- predict(modelos[[i]], newdata = test)
  mse <- mean((test$SalePrice - predicciones)^2)
  correlacion <- cor(test$SalePrice, predicciones)
  print(paste("Profundidad:", i, "MSE:", mse, "Coeficiente de correlación:", correlacion))
}

#Inciso 6. Clasificar las casas en Económicas, Intermedias y Caras.

# Definir cuartiles
cuartiles <- quantile(datos$SalePrice, probs = c(0.25, 0.5, 0.75))

# Crear variable respuesta
datos$Clasificacion <- cut(datos$SalePrice, breaks = c(0, cuartiles[2], cuartiles[3], max(datos$SalePrice)), labels = c("Económicas", "Intermedias", "Caras"))

View(datos)

# Contar el número de casas en cada categoría
num_casas <- table(datos$Clasificacion)
print(num_casas)

#Añadir Clasificacion a los conjuntos train y test.

trainRowsNumber<-sample(1:nrow(datos),porcentaje4*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]

# Inciso 7 - Creación del árbol

# Crear modelo de árbol de clasificación

modelo_arbol_clasificacion <- rpart(Clasificacion ~ . - SalePrice, data = datos, method = "class")
# Visualizar el árbol
rpart.plot(modelo_arbol_clasificacion, digits = 3, fallen.leaves = TRUE)

# Inciso 8.
# Predecir con el conjunto de prueba
predicciones_clasificacion <- predict(modelo_arbol_clasificacion, newdata = test, type = "class")

head(predicciones_clasificacion)

# Calcular la precisión
precision_arbol <- sum(predicciones_clasificacion == test$Clasificacion) / length(test$Clasificacion)
print(paste("Precisión del árbol de clasificación:", precision))


# Inciso 9
# Analisis de eficiencia del algoritmo con matriz de confusión

#install.packages("caret")
library(caret)

confusion_matrix <- confusionMatrix(predicciones_clasificacion, test$Clasificacion)
print("Matriz de Confusión:")
print(confusion_matrix)

# Mostrar la precisión global y por clase
print(paste("Precisión Global:", confusion_matrix$overall["Accuracy"]))

# Mostrar la precisión global y por clase
precision_global <- confusion_matrix$overall["Accuracy"]
print(paste("Precisión Global:", ifelse(!is.na(precision_global), precision_global, "No disponible")))
print("Precisión por Clase:")
balanced_accuracy <- confusion_matrix$byClass["Balanced Accuracy"]
print(paste("Precisión Balanceada:", ifelse(!is.na(balanced_accuracy), balanced_accuracy, "No disponible")))

# Mostrar errores más comunes
sensibilidad <- confusion_matrix$byClass["Sens"]
print("Sensibilidad (Errores más comunes):")
print(ifelse(!is.na(sensibilidad), sensibilidad, "No disponible"))
especificidad <- confusion_matrix$byClass["Spec"]
print("Especificidad (Errores menos comunes):")
print(ifelse(!is.na(especificidad), especificidad, "No disponible"))


# Inciso 10
# Entrenar modelo con validación cruzada


# Definir el control de la validación cruzada
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation


datos_imputados <- datos
for (col in colnames(datos)) {
  if (any(is.na(datos[[col]]))) {
    datos_imputados[[col]][is.na(datos[[col]])] <- mean(datos[[col]], na.rm = TRUE)
  }
}

print("Valores faltantes después de la imputación:")
print(colSums(is.na(datos_imputados)))

datos_imputados <- datos_imputados[, colSums(is.na(datos_imputados)) == 0]


# Entrenar el modelo con validación cruzada
modelo_cruzado <- train(Clasificacion ~ . - SalePrice, data = datos_imputados, method = "rpart", trControl = ctrl)

# Mostrar el resumen del modelo cruzado
print(modelo_cruzado)

# Realizar predicciones con el modelo cruzado en el conjunto de prueba
predicciones_cruzadas <- predict(modelo_cruzado, newdata = test)

# Calcular la precisión con el modelo cruzado
precision_cruzada <- sum(predicciones_cruzadas == test$Clasificacion) / length(test$Clasificacion)
print(paste("Precisión con validación cruzada:", precision_cruzada))



# Inciso 11
# Añadir 3 modelos más con diferentes profundidades al árbol de clasificación
for (depth in c(4,8,12)) {
  modelo <- rpart(Clasificacion ~ . - SalePrice, data = datos, method = "class", control = rpart.control(maxdepth = depth))
  # Visualizar el árbol
  rpart.plot(modelo, digits = depth, fallen.leaves = TRUE)
  
  # Realizar predicciones con el conjunto de prueba
  predicciones_clasificacion <- predict(modelo, newdata = test, type = "class")
  
  # Calcular la precisión
  precision <- sum(predicciones_clasificacion == test$Clasificacion) / length(test$Clasificacion)
  print(paste("Profundidad:", depth, "Precisión del árbol de clasificación:", precision))
}



# Inciso 12
# Instalar y cargar el paquete randomForest
# install.packages("randomForest")
library(randomForest)


datos_imputados <- datos
for (col in colnames(datos)) {
  if (any(is.na(datos[[col]]))) {
    datos_imputados[[col]][is.na(datos[[col]])] <- mean(datos[[col]], na.rm = TRUE)
  }
}

print("Valores faltantes después de la imputación:")
print(colSums(is.na(datos_imputados)))

datos_imputados <- datos_imputados[, colSums(is.na(datos_imputados)) == 0]
# Ajustar modelo Random Forest con datos imputados
modelo_rf <- randomForest(Clasificacion ~ . - SalePrice, data = datos_imputados)
print(modelo_rf)

# Asegurar niveles de factor consistentes
test$Clasificacion <- factor(test$Clasificacion, levels = levels(datos_imputados$Clasificacion))

# Realizar predicciones con el conjunto de prueba
predicciones_rf <- predict(modelo_rf, newdata = test, type = "response")

# Calcular la precisión con el modelo Random Forest
precision_rf <- sum(predicciones_rf == test$Clasificacion, na.rm = TRUE) / length(test$Clasificacion)
print(paste("Precisión con Random Forest:", precision_rf))


#HOJA DE TRABAJO 5
# Cargar el paquete caret

datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")
datos <- datos[, -1]

install.packages("caret")
library(caret)

# Inciso 2)
# Definir el esquema de partición para la validación cruzada
control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Entrenar el modelo Naive Bayes
modelo_nb <- train(SalePrice ~ ., data = datos_imputados, method = "nb", trControl = control)

# Mostrar el resumen del modelo
print(modelo_nb)

# Realizar predicciones en el conjunto de prueba
predicciones <- predict(modelo_nb, datos_imputados)

# Explorar las predicciones
print(predicciones)


# Calcular la precisión del modelo
precision <- sum(predicciones == datos_prueba$SalePrice) / length(predicciones) * 100

# Mostrar la precisión
print(precision)

datos_imputados$SalePrice <- as.numeric(as.character(datos_imputados$SalePrice))
class(datos_imputados$SalePrice)

predicciones <- as.numeric(as.character(predicciones))


r_squared <- 1 - mean((datos_imputados$SalePrice - predicciones)^2) / var(datos_imputados$SalePrice)
print(r_squared)


# Calcular el error cuadrático medio (MSE)
mse <- mean((predicciones - datos_imputados$SalePrice)^2)

print(mse)



# Hoja de trabajo 6

#1. Variable dicotómica


datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")

# Clasificar las casas en Económicas, Intermedias y Caras.

# Definir cuartiles
cuartiles <- quantile(datos$SalePrice, probs = c(0.25, 0.5, 0.75))

# Crear variable respuesta
datos$Clasificacion <- cut(datos$SalePrice, breaks = c(0, cuartiles[2], cuartiles[3], max(datos$SalePrice)), labels = c("Económicas", "Intermedias", "Caras"))

View(datos)

# Contar el número de casas en cada categoría
num_casas <- table(datos$Clasificacion)
print(num_casas)

# Crear variable dicotómica para vivienda cara
datos$Cara <- ifelse(datos$Clasificacion == "Caras", 1, 0)

# Crear variable dicotómica para vivienda media
datos$Media <- ifelse(datos$Clasificacion == "Intermedias", 1, 0)

# Crear variable dicotómica para vivienda económica
datos$Economica <- ifelse(datos$Clasificacion == "Económicas", 1, 0)

# Ver los primeros registros de los datos con las nuevas variables dicotómicas
head(datos)

## INCISO 2

install.packages("fastDummies")

library(caret)
library(fastDummies)
datos <- dummy_cols(datos,  select_columns = c("Clasificacion"))
Clasificacion_Caras <- as.factor(datos$Clasificacion_Caras)

train_factor$Clasificacion_Intermedias <- as.factor(datos$Clasificacion_Intermedias)
datos$Clasificacion_Económicas <- as.factor(datos$Clasificacion_Económicas)

datos$Clasificacion_Intermedias <- as.factor(datos$Clasificacion_Intermedias)
datos$Clasificacion_Económicas <- as.factor(datos$Clasificacion_Económicas)

levels(datos$Clasificacion_Caras)
levels(datos$Clasificacion_Intermedias)
levels(datos$Clasificacion_Económicas)

head(datos)

porcentaje4 <- 0.70
trainRowsNumber<-sample(1:nrow(datos),porcentaje4*nrow(datos))
train<-datos[trainRowsNumber,]
test<-datos[-trainRowsNumber,]
set.seed(123)

head(train)
levels(train$Clasificacion_Caras)

#Inciso 3
library(e1071)
library(glmnet)
library(ggplot2)
library(lattice)

# Crear el modelo de regresión logística
modeloCaro <- glm(Clasificacion_Caras~Clasificacion, data = train, family = binomial(), maxit=100)


#Inciso 4
summary(modeloCaro)

#Inciso 5

predicciones <- predict(modeloCaro, newdata = test, type = "response")
predicciones_binarias <- ifelse(predicciones > 0.5, 1, 0)

print(predicciones_binarias)

# Calcular la precisión del modelo
precision <- mean(predicciones_binarias == test$Clasificacion_Caras)
print(precision)

#Inciso 6

train_predicciones <- predict(modeloCaro, newdata = train, type = "response")
train_predicciones_binarias <- ifelse(train_predicciones > 0.5, 1, 0)

print(train_predicciones_binarias)

#
#Inciso 7
#

library(boot)

# Definir la fórmula del modelo
formula <- Clasificacion_Caras ~ Clasificacion

# Definir el modelo
modelo <- glm(formula, data = train, family = binomial())

# Realizar la validación cruzada
cv_modelo <- cv.glm(train, modelo)

# Mostrar los resultados
print(cv_modelo)

# Seleccionar el modelo con menor error
mejor_modelo <- cv_modelo$glm

# Resumen del mejor modelo
summary(mejor_modelo)


Clasificacion_Intermedias <- as.factor(datos$Clasificacion_Intermedias)
Clasificacion_Económicas <- as.factor(datos$Clasificacion_Económicas)

# Definir la fórmula del modelo para Clasificacion_Economicas
formula_eco <- Clasificacion_Economicas ~ Clasificacion

# Definir el modelo
modelo_eco <- glm(formula_eco, data = train, family = binomial())

# Realizar la validación cruzada
cv_modelo_eco <- cv.glm(train, modelo_eco)

# Mostrar los resultados
print(cv_modelo_eco)

# Seleccionar el modelo con menor error
mejor_modelo_eco <- cv_modelo_eco$glm

# Resumen del mejor modelo
summary(mejor_modelo_eco)

# Ajusta el primer modelo
modelo_caras <- glm(Clasificacion_Caras ~ Clasificacion, data = train, family = binomial())

# Usa los coeficientes de modelo_caras como valores iniciales para modelo_economicas
# (Nota: Esta es una aproximación, ya que R no permite directamente esta funcionalidad en glm)
# Suponiendo una funcionalidad hipotética o uso de otro software que lo permita:
coef_iniciales <- coef(modelo_caras)
modelo_Intermedias <- glm(Clasificacion_Intermedias ~ Clasificacion, data = train, family = binomial(), start = coef_iniciales)


coef_inicialesEconomicas <- coef(modelo_caras)
modelo_Economicas <- glm(Clasificacion_Económicas ~ Clasificacion, data = train, family = binomial(), start = coef_inicialesEconomicas)


print(modelo_Intermedias)
print(modelo_Economicas)

#
#Inciso 8
#


library(caret)


matriz_confusion <- confusionMatrix(data = factor(test$Clasificacion_Caras), 
                                    reference = factor(predicciones_binarias))


print(matriz_confusion)


install.packages("profvis")
library(profvis)

library(pryr)

profvis({
  # Calcular la matriz de confusión
  memoria_anterior <- mem_used()
  matriz_confusion <- confusionMatrix(data = factor(test$Clasificacion_Caras), 
                                      reference = factor(predicciones_binarias))
  memoria_despues <- mem_used()
  print(memoria_despues - memoria_anterior)
})


#
# Inciso 9
#

aic_caras <- AIC(modelo_caras)
bic_caras <- BIC(modelo_caras)

aic_intermedias <- AIC(modelo_Intermedias)
bic_intermedias <- BIC(modelo_Intermedias)

aic_economicas <- AIC(modelo_Economicas)
bic_economicas <- BIC(modelo_Economicas)

print(data.frame(Modelo = c("Caras", "Intermedias", "Económicas"),
                 AIC = c(aic_caras, aic_intermedias, aic_economicas),
                 BIC = c(bic_caras, bic_intermedias, bic_economicas)))


#
# Inciso 10
#
# Cargar bibliotecas necesarias
library(rpart)      # Para árboles de decisión
library(randomForest) # Para Random Forest
library(e1071)      # Para Naive Bayes

# Crear modelo de árbol de decisión
modelo_arbol <- rpart(Clasificacion_Caras ~ Clasificacion, data = train, method = "class")
print(modelo_arbol)

# Crear modelo de Random Forest
modelo_rf <- randomForest(Clasificacion_Caras ~ Clasificacion, data = train)
print(modelo_rf)

# Crear modelo de Naive Bayes
modelo_nb <- naiveBayes(Clasificacion_Caras ~ Clasificacion, data = train)
print(modelo_nb)




# Hacer predicciones con los modelos
predicciones_arbol <- predict(modelo_arbol, test, type = "class")
print(predicciones_arbol)
predicciones_rf <- predict(modelo_rf, test)
print(predicciones_rf)
predicciones_nb <- predict(modelo_nb, test)
print(predicciones_nb)

# Calcular precisión de cada modelo
precision_arbol <- mean(predicciones_arbol == test$Clasificacion_Caras)
precision_rf <- mean(predicciones_rf == test$Clasificacion_Caras)
precision_nb <- mean(predicciones_nb == test$Clasificacion_Caras)

# Imprimir precisión de cada modelo
print(paste("Precisión del árbol de decisión:", precision_arbol))
print(paste("Precisión de Random Forest:", precision_rf))
print(paste("Precisión de Naive Bayes:", precision_nb))




#
# Inciso 11
#
# Tiempo de procesamiento para cada modelo
inicio <- Sys.time()
modelo_arbol <- rpart(Clasificacion_Caras ~ Clasificacion, data = train_factor2, method = "class")
tiempo_arbol <- Sys.time() - inicio

print(tiempo_arbol)

inicio <- Sys.time()
modelo_rf <- randomForest(Clasificacion_Caras ~ Clasificacion, data = train)
tiempo_rf <- Sys.time() - inicio

inicio <- Sys.time()
modelo_nb <- naiveBayes(Clasificacion_Caras ~ Clasificacion, data = train)
tiempo_nb <- Sys.time() - inicio

inicio <- Sys.time()
modelo_logistico <- glm(Clasificacion_Caras ~ Clasificacion, data = train_factor2, family = binomial(), maxit=100)
tiempo_logistico <- Sys.time() - inicio

# Predicciones para cada modelo
predicciones_arbol <- predict(modelo_arbol, test, type = "class")
predicciones_rf <- predict(modelo_rf, test)
predicciones_nb <- predict(modelo_nb, test)
predicciones_logistico <- predict(modelo_logistico, train_factor2, type = "response")

# Calcular precisión de cada modelo
precision_arbol <- mean(predicciones_arbol == test$Clasificacion_Caras)
precision_rf <- mean(predicciones_rf == test$Clasificacion_Caras)
precision_nb <- mean(predicciones_nb == test$Clasificacion_Caras)
precision_logistico <- mean(predicciones_logistico == train_factor2$Clasificacion_Caras)
precision_logistico <- precision_entrenamiento_polynomial

# Imprimir tiempo de procesamiento y precisión de cada modelo
print("Tiempo de procesamiento:")
print(paste("Árbol de decisión:", tiempo_arbol))
print(paste("Random Forest:", tiempo_rf))
print(paste("Naive Bayes:", tiempo_nb))
print(paste("Regresión Logística:", tiempo_logistico))

print("Precisión en el conjunto de prueba:")
print(paste("Árbol de decisión:", precision_arbol))
print(paste("Random Forest:", precision_rf))
print(paste("Naive Bayes:", precision_nb))
print(paste("Regresión Logística:", precision_logistico))



### HOJA DE TRABAJO 7 - SVM

datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")

datos <- datos[, !names(datos) %in% "Id"]

# Identificar columnas cuantitativas y categóricas
columnas_cuantitativas <- sapply(datos, is.numeric)
columnas_categoricas <- sapply(datos, is.character)

# Seleccionar columnas cuantitativas y categóricas
datos_cuantitativos <- datos[, columnas_cuantitativas]
datos_categoricos <- datos[, columnas_categoricas]

View(datos_cuantitativos)
View(datos_categoricos)

# Convertir columnas categóricas a factores
datos_categoricos <- lapply(datos_categoricos, as.factor)

datos_cuantitativos <- datos_cuantitativos[complete.cases(datos_cuantitativos),]

datos_cuantitativos <- data.frame(datos_cuantitativos)

# Clasificar las casas en Económicas, Intermedias y Caras.

# Definir cuartiles
cuartiles <- quantile(datos_cuantitativos$SalePrice, probs = c(0.25, 0.5, 0.75))

# Crear variable respuesta
datos_cuantitativos$Clasificacion <- cut(datos_cuantitativos$SalePrice, breaks = c(0, cuartiles[2], cuartiles[3], max(datos_cuantitativos$SalePrice)), labels = c("Económicas", "Intermedias", "Caras"))
datos_cuantitativos$Clasificacion <- as.factor(datos_cuantitativos$Clasificacion)
View(datos_cuantitativos)

porcentaje4 <- 0.70
trainRowsNumber<-sample(1:nrow(datos_cuantitativos),porcentaje4*nrow(datos_cuantitativos))
train<-datos_cuantitativos[trainRowsNumber,]
test<-datos_cuantitativos[-trainRowsNumber,]
set.seed(123)

View(train)
View(test)

library(e1071)
library(caret)
library(ggplot2)
library(lattice)

#Inciso 4

modelosvm<-svm(Clasificacion~., data = train, scale = F)

svmModel <- svm(Clasificacion~., data = train, type = "C-classification", kernel = "linear", cost = 1)

svm_radial <- svm(Clasificacion~., data = train, kernel = "radial", cost = 2^-5, gamma = 0.1)

svm_polynomial <- svm(Clasificacion~., data = train, kernel = "polynomial", cost = 1, gamma = 0.1, degree = 2)

#Inciso 5

predictions_linear <- predict(svmModel, newdata=test)
print(predictions_linear)

# Asumiendo que 'test' es tu conjunto de datos de prueba y que contiene las columnas necesarias
predictions_radial <- predict(svm_radial, newdata=test)

# Mostrar las predicciones
print(predictions_radial)

# Asumiendo que 'test_factor' es tu conjunto de datos de prueba y que contiene las columnas necesarias
predictions_polynomial <- predict(svm_polynomial, newdata=test)  # Asegúrate que 'test_factor' tenga la misma estructura que 'train_factor'

# Mostrar las predicciones
print(predictions_polynomial)


# Inciso 6

# Para el modelo SVM lineal
confusion_linear <- confusionMatrix(predictions_linear, test$Clasificacion)

# Para el modelo SVM radial
confusion_radial <- confusionMatrix(predictions_radial, test$Clasificacion)

# Para el modelo SVM polinomial
confusion_polynomial <- confusionMatrix(predictions_polynomial, test$Clasificacion)

# Para el modelo SVM lineal
print("Matriz de Confusión - SVM Lineal:")
print(confusion_linear)

# Para el modelo SVM radial
print("Matriz de Confusión - SVM Radial:")
print(confusion_radial)

# Para el modelo SVM polinomial
print("Matriz de Confusión - SVM Polinomial:")
print(confusion_polynomial)

# Inciso 7

# Calcular la precisión en los datos de entrenamiento
precision_entrenamiento_linear <- sum(predict(svmModel, newdata = train) == train$Clasificacion) / nrow(train)
precision_entrenamiento_radial <- sum(predict(svm_radial, newdata = train) == train$Clasificacion) / nrow(train)
precision_entrenamiento_polynomial <- sum(predict(svm_polynomial, newdata = train) == train$Clasificacion) / nrow(train)

# Calcular la precisión en los datos de prueba
precision_prueba_linear <- sum(predictions_linear == test$Clasificacion) / nrow(test)
precision_prueba_radial <- sum(predictions_radial == test$Clasificacion) / nrow(test)
precision_prueba_polynomial <- sum(predictions_polynomial == test$Clasificacion) / nrow(test)

# Comparar la precisión entre los datos de entrenamiento y prueba
diferencia_linear <- abs(precision_entrenamiento_linear - precision_prueba_linear)
diferencia_radial <- abs(precision_entrenamiento_radial - precision_prueba_radial)
diferencia_polynomial <- abs(precision_entrenamiento_polynomial - precision_prueba_polynomial)

# Imprimir los resultados
print("Análisis de Sobreajuste o Desajuste:")
print(paste("SVM Lineal - Diferencia entre precisión de entrenamiento y prueba:", diferencia_linear))
print(paste("SVM Radial - Diferencia entre precisión de entrenamiento y prueba:", diferencia_radial))
print(paste("SVM Polinomial - Diferencia entre precisión de entrenamiento y prueba:", diferencia_polynomial))

# Inciso 8

library(rpart)      # Para árboles de decisión
library(e1071)      # Para Naive Bayes

# Crear modelo de árbol de decisión
modelo_arbol <- rpart(Clasificacion_Caras ~ Clasificacion, data = train, method = "class")
print(modelo_arbol)

# Crear modelo de Naive Bayes
modelo_nb <- naiveBayes(Clasificacion_Caras ~ Clasificacion, data = train)
print(modelo_nb)

# Crear el modelo de regresión logística
modeloCaro <- glm(Clasificacion_Caras~Clasificacion, data = train, family = binomial(), maxit=100)


# Hacer predicciones con los modelos
predicciones_arbol <- predict(modelo_arbol, test, type = "class")
precision_arbol <- precision_cruzada
# Calcular la precisión del modelo de Baiyes
precision_baiyes <- sum(predicciones == datos_prueba$SalePrice) / length(predicciones) * 100
print(precision_bayes)

#Modelo Logistico
predicciones_logistico <- predict(modelo_logistico, train_factor2, type = "response")
precision_logistico <- mean(predicciones_logistico == train_factor2$Clasificacion_Caras)



print(paste("Precisión del árbol de clasificación:", precision_arbol))
print(paste("Precisión de Naive Bayes:", precision_baiyes))
print(paste("Precisión de regresion logistica:", precision_logistico))
print(paste("Precisión de SVM Lineal:", precision_prueba_linear))
print(paste("Precisión de SVM Polynomial:", precision_prueba_polynomial))
print(paste("Precisión de SVM radial:", precision_prueba_radial))

# Inciso 9

#Arboles de decision
inicio <- Sys.time()
modelo_arbol <- rpart(Clasificacion_Caras ~ Clasificacion, data = train_factor2, method = "class")
tiempo_arbol <- Sys.time() - inicio

print(paste("Tiempo para el modelo de arbol de decision: ", tiempo_arbol))

#Random Forest
inicio <- Sys.time()
modelo_rf <- randomForest(Clasificacion_Caras ~ Clasificacion, data = train_factor2)
tiempo_rf <- Sys.time() - inicio

print(paste("Tiempo para el modelo Random Forest: ", tiempo_rf))

#Naive Bayes
inicio <- Sys.time()
modelo_nb <- naiveBayes(Clasificacion_Caras ~ Clasificacion, data = train_factor2)
tiempo_nb <- Sys.time() - inicio

print(paste("Tiempo para el modelo Naive Bayes: ", tiempo_nb))

#Modelo SVM
inicio <- Sys.time()
svmModel <- svm(Clasificacion_Caras~ Clasificacion, data = train_factor2, type = "C-classification", kernel = "linear", cost = 1)
tiempo_svm <- Sys.time() - inicio
print(paste("Tiempo para el modelo SVM: ", tiempo_svm))



# Inciso 10

datos <- read.csv("train.csv", header = TRUE, encoding = "UTF-8")

datos <- datos[, !names(datos) %in% "Id"]

# Identificar columnas cuantitativas y categóricas
columnas_cuantitativas <- sapply(datos, is.numeric)
columnas_categoricas <- sapply(datos, is.character)

# Convertir columnas categóricas a factores
datos_categoricos <- lapply(datos_categoricos, as.factor)

datos_cuantitativos <- datos_cuantitativos[complete.cases(datos_cuantitativos),]

datos_cuantitativos <- data.frame(datos_cuantitativos)

train<-datos_cuantitativos[trainRowsNumber,]

# Entrenar el modelo de regresión lineal
modelo_regresion <- lm(SalePrice ~ ., data = train)


# Evaluación del modelo
summary(modelo_regresion)

# Validación del modelo
predicciones_regresion <- predict(modelo_regresion, newdata = test_data)
print(predicciones_regresion)
predicciones_regresion <- na.omit(predicciones_regresion)
mse <- mean((test_data$SalePrice - predicciones_regresion)^2)

#test_data_clean <- na.omit(test_data)
#predicciones_regresion_clean <- predicciones_regresion[!is.na(test_data$SalePrice)]

print(paste("Error cuadrático medio (MSE):", mse))



# Inciso 11

# Carga de paquetes necesarios
library(MASS) # Para la función lda
library(caret) # Para la función trainControl
library(glmnet) # Para la función cv.glmnet


# Predicciones de los modelos lineales
pred_single <- predict(single_linear_model, newdata = test)
pred_multiple <- predict(multiple_linear_model, newdata = test)

# Predicciones del modelo logístico
pred_logistic <- predict(modeloCaro, newdata = test, type = "response")

# Calcula el error cuadrático medio (MSE) para cada modelo
mse_single <- mean((test$SalePrice - pred_single)^2)
mse_multiple <- mean((test$SalePrice - pred_multiple)^2)
mse_logistic <- mean((test$Clasificacion_Caras - pred_logistic)^2)

# Muestra los resultados
print("MSE del modelo lineal singular:")
print(mse_single)
print("MSE del modelo lineal múltiple:")
print(mse_multiple)
print("MSE del modelo logístico:")
print(mse_logistic)




# Inciso 12
