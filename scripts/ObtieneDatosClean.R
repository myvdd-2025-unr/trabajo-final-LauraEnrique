# —-------------------------—---------------------------------------------------
# Configura wd (working directory) y librerias necesarias
# —-------------------------—---------------------------------------------------
# Declara working directory
setwd("C:/trabajo-final-LauraEnrique")
# DEclara en un vector todos los paquetes de librerias necesarias 
paquetes <- c("tidyverse", "ggplot2", "fs")
# Instala librerias que no están instaladas
for (p in paquetes) {if (!requireNamespace(p, quietly = TRUE)) {install.packages(p)}}
# Carga las librerias
purrr::walk(paquetes, library, character.only = TRUE)

# —-------------------------—---------------------------------------------------
#Crea carpetas para datos limpios e informe final, si no existen. 
# Configura carpeta origen de datos.
# —-------------------------—---------------------------------------------------
# Elimina carpetas de resultados de corrida anterior
dir_delete("DatosClean")
# Crea carpetas para datos limpios e informe final en nueva corrida
folders <- c("DatosClean", "ReporteQuarto")
purrr::walk(folders, ~ dir.create(.x, showWarnings = FALSE))

# —-------------------------—---------------------------------------------------
# Pasos para procesar cada archivo
# —-------------------------—---------------------------------------------------
# Obtiene carpeta de datos de origen. 
 carpetaOrigen <- "DatosRelevados"
# Obtiene lista de archivos en carpeta de origen con ruta completa
listaOrigen <- list.files(carpetaOrigen,
                       full.names = TRUE,
                       recursive = TRUE)
# Obtiene lista de nombres archivos en carpeta de origen, sin ruta
nombreOrigen <- list.files(carpetaOrigen,
                           full.names = FALSE,
                           recursive = TRUE)
#Configura vector de encabezados
encabezados <- c("LATITUD", "LONGITUD", "TIPO_DE_TRABAJO", 
                 "FA_ELEMENTOS_EN_DESUSO", "T_ELEMENTOS_EN_DESUSO", "I_ELEMENTOS_EN_DESUSO", 
                 "FA_BOTELLAS_VARIAS", "T_BOTELLAS_VARIAS", "I_BOTELLAS_VARIAS", 
                 "FA_RECIPIENTES_PLASTICOS", "T_RECIPIENTES_PLASTICOS", "I_RECIPIENTES_PLASTICOS", 
                 "FA_MACETA_FLORERO_PLANTA_ACUATICA", "T_MACETA_FLORERO_PLANTA_ACUATICA", "I_MACETA_FLORERO_PLANTA_ACUATICA", 
                 "FA_HUECO_DE_ARBOL", "T_HUECO_DE_ARBOL", "I_HUECO_DE_ARBOL", 
                 "FA_CANALETA_PARA_LLUVIA", "T_CANALETA_PARA_LLUVIA", "I_CANALETA_PARA_LLUVIA", 
                 "FA_CUBIERTAS", "T_CUBIERTAS", "I_CUBIERTAS", 
                 "FA_PILETAS_CISTERNAS_ALJIBES", "T_PILETAS_CISTERNAS_ALJIBES", "I_PILETAS_CISTERNAS_ALJIBES", 
                 "FA_TANQUES_ BAJOS_ Y_ ELEVADOS_BARRILES_TONELES", "T_TANQUES_ BAJOS_ Y_ ELEVADOS_BARRILES_TONELES", "I_TANQUES_ BAJOS_ Y_ ELEVADOS_BARRILES_TONELES", 
                 "FA_TOTALES", "T_TOTALES", "I_TOTALES", 
                 "DESTRUIDOS")

# —-------------------------—---------------------------------------------------
#Pasos para procesar cada archivo relevado
# —-------------------------—---------------------------------------------------
#variable para obtener en nombre sin extensión para guardar archivo limpio
j = 0
#REcorre cada archivo y lo transforma
for (elementoLista in listaOrigen) {
  #Lee archivo y asigna a df
  df <- read.csv(elementoLista, header=FALSE, quote="")
  # elimina cols sobrantes
  df <- df[ ,-c(4:6)] 
  #Aplica vector encabezados en fila 4
  df[4, ] <- encabezados
  
  # --------------------------------------------------
  #Pasos para agregar columna FECHA DE LOS REGISTROS
  # --------------------------------------------------
    # Obtiene nombre variable
    head_fecha <- df[2,1]
    # Obtiene fecha
    var_fecha <- df[2,2]
    # Elimina filas con datos sobrantes
    df <- df[-c(1:3), ]
    #Agrega col FECHA DE LOS REGISTROS
    colnames(df) <- df[1, ]
    #Elimina la fila sobrante
    df <- df[-1, ]
    # Agrega col de fecha del RElevamiento asignando la fecha obtenida
    head_fecha = "FECHA_REGISTRO"
    df[ ,head_fecha] <- var_fecha
    df <-filter(df, TIPO_DE_TRABAJO != "")
    
    # --------------------------------------------------
    #Pasos para guardar ruta de archivo limpio
    # --------------------------------------------------        
    #Setea la variable j
    j <- j + 1
    #Obtiene el nombre de archivo sin extensión
    nombreSinExtension <- str_sub(nombreOrigen[j], 1, nchar(nombreOrigen[j]) - 4)
    #Crea ruta para guardar archivo limpio
    nombreCompleto <- paste("DatosClean/", nombreSinExtension, "_Clean.csv", sep = "")
    #Guarda archivo limpio
    write_csv(df, nombreCompleto)
}

# —-------------------------—---------------------------------------------------
#Pasos para unir todos los archivos limpios en un archivo que se guarda en wd
# —-------------------------—---------------------------------------------------
#Crea lista de archivos para unir
df_lista <- list.files("DatosClean", full.names = TRUE)
#Lee los archivos de la lista
df <- map_df(df_lista, ~ read.csv(.x))
# Guarda archivo de resultados en formato csv en wd
write_csv(df, "DatosParaAnalisis.csv")

# —-------------------------—---------------------------------------------------
#Pasos para crear una tabla de resultados
# —-------------------------—---------------------------------------------------
#Lee archivo de resultados
datosAnalisis <- read_csv("DatosParaAnalisis.csv")
#Crea tabla de resultados: suma FA: Foco Aedico, T: Tratado, I:INspeccionado
datosTrabajo <- datosAnalisis %>% 
  group_by(TIPO_DE_TRABAJO) %>% 
  summarise(across(c(FA_TOTALES, T_TOTALES, I_TOTALES), sum))
write_csv(datosTrabajo, "tablaTrabajo.csv")
# —-------------------------—---------------------------------------------------
#Pasos para crear grafico de barras
# —-------------------------—---------------------------------------------------
#Obtiene datos de tabla creada ad hoc
ggplot(data = datosTrabajo) + 
  aes(x = TIPO_DE_TRABAJO, y = FA_TOTALES, fill = TIPO_DE_TRABAJO) +
  #grafico de barras
  geom_bar(stat="identity") +
  scale_x_discrete(name = "Fecha de registro") + 
  scale_y_discrete(name = "Foco Aedico", breaks = c(0, 50, 100, 150, 200, 250, 300, 350,400)) + 
  #facet_wrap(~ TIPO_DE_TRABAJO) +
  scale_fill_manual(values=c("orange", "yellow", "red"))

  