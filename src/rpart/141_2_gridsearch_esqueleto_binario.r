# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros
install.packages("data.table")
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(100151, 100109, 100129, 100153, 100213)
# Define las semillas
#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # Particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria1", seed = semilla)
 

  # Genero el modelo para predecir clase_binaria1
  modelo <- rpart("clase_binaria1 ~ .",
    data = dataset[fold == 1], # Excluyo columna clase_ternaria, fold==1 es training
    xval = 0,
    control = param_basicos
  )

  # Aplico el modelo a los datos de testing
  prediccion <- predict(modelo,
    dataset[fold == 2], # Excluyo columna clase_ternaria, fold==2 es testing
    type = "prob"
  )

  # Calculo la ganancia en testing que es fold==2
  ganancia_test <- dataset[fold == 2, ]
  ganancia_test$ganancia <- ifelse(prediccion[, "Positivo"] > 0.025,
    ifelse(ganancia_test$clase_binaria1 == "Positivo", 117000, -3000), 
    0
  )
  ganancia_test_normalizada <- sum(ganancia_test$ganancia) / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 3
  ) # se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("/Users/mpb/Desktop/Maestrìa/7-Laboratorio_de_implemetacion_I/") # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

dataset$clase_binaria1 <- ifelse(dataset$clase_ternaria %in% c("BAJA+1", "CONTINUA"), "Negativo", "Positivo")


dataset$clase_ternaria <- NULL


dataset[is.na(dataset)] <- 0 # reemplazo los NA por 0


# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_binaria1.txt"

# Escribo los titulos al archivo donde van a quedar los resultados
# atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE,
#  y lo que estaba antes se pierde
# la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "max_depth", "\t",
  "min_split", "\t",
  "cp", "\t",
  "minbucket", "\t",
  "ganancia_promedio", "\n"
)


# itero por los loops anidados para cada hiperparametro

for (vmax_depth in c(7,8)) {
  for (vmin_split in c(1000,900,750,500,250)) {
    for(cp in c(-0.05,-0.001)) {
        for(minbucket in c(vmin_split,vmin_split/2,vmin_split)) {
    # notar como se agrega

    # vminsplit  minima cantidad de registros en un nodo para hacer el split
    param_basicos <- list(
      "cp" = cp, # complejidad minima
      "minsplit" = vmin_split,
      "minbucket" = minbucket, # minima cantidad de registros en una hoja
      "maxdepth" = vmax_depth
    ) # profundidad máxima del arbol

    # Un solo llamado, con la semilla 17
    ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas , param_basicos)

    # escribo los resultados al archivo de salida
    cat(
      file = archivo_salida,
      append = TRUE,
      sep = "",
      vmax_depth, "\t",
      vmin_split, "\t",
      cp, "\t",
      minbucket, "\t",
      ganancia_promedio, "\n"
    )
    # Mostrar información en la consola
            cat("Iteración:", vmax_depth, "-", vmin_split, "-", cp, "-", minbucket, "\t Ganancia promedio:", ganancia_promedio, "\n") # nolint
            }
        }
    }
}




