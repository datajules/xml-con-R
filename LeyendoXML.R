
library("readtext")
library('tidyverse')
library('rebus')



# 1. Leemos el archivo XML
carpeta <- 'C:/Users/jvice/Documents/Pruebas data/scopus/'
corpus <- readtext(file = paste0(carpeta, "XML.txt"))
texto <- corpus[2]

# Empleamos la estructura de patrones <variable> contenido </variable>
pattern <- '<' %R% one_or_more(WRD) %R% '>'

# Extraemos los nombres de las columnas
# Извлекаем названия столбцов
columnas <- texto %>% str_extract_all(pattern) 
columnas <- data.frame(columnas, stringsAsFactors = FALSE)
names(columnas)[1] <- "inicio"
columnas <- columnas %>% mutate(fin = str_replace(inicio, '<','</'))
columnas <- columnas %>% mutate(columna = str_remove_all(inicio, '<|>'))


# Funciones ---------------------------------------------------------------


# Esta función permite extraer el contenido de una variable de un texto xml
contenido <- function(texto, variable){
  patterni <- paste0('<', variable, '>')
  patternf <- paste0('</', variable, '>')
  resultado <- texto %>% str_extract_all(patterni %R% zero_or_more(PRINT) %R% patternf) 
  resultado <- data.frame(texto = resultado[[1]], stringsAsFactors = FALSE)
  resultado <- resultado %>% mutate(texto = str_extract(texto,'>' %R% one_or_more(PRINT) %R% '<' ))
  resultado <- resultado %>% mutate(texto = str_remove_all(texto, '<|>'))
  names(resultado) <- paste0(variable)
  return(resultado)
}

prueba <- contenido(texto, 'eid')




