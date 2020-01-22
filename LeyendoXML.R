
library("readtext")
library('tidyverse')
library('rebus')

carpeta <- 'C:/Users/jvice/Documents/Pruebas data/scopus/'

corpus <- readtext(file = paste0(carpeta, "XML.txt"))

texto <- corpus[2]

# pattern <- '<' %R% one_or_more(PRINT) %R% '>'
pattern <- '<' %R% one_or_more(WRD) %R% '>'

# Extraemos los nombres de las columnas
# Извлекаем названия столбцов
columnas <- texto %>% str_extract_all(pattern) 

columnas <- data.frame(columnas, stringsAsFactors = FALSE)
names(columnas)[1] <- "inicio"
columnas <- columnas %>% mutate(fin = str_replace(inicio, '<','</'))
columnas <- columnas %>% mutate(columna = str_remove_all(inicio, '<|>'))

# Ahora empleamos el patron para buscar las bases

contenido <- function(texto, variable){
  patterni <- paste0('<', variable, '>')
  patternf <- paste0('</', variable, '>')
  resultado <- texto %>% str_extract_all(patterni %R% zero_or_more(PRINT) %R% patternf) 
}

columnas[1,1]


variable <- texto %>% str_extract_all(columnas[2,1] %R% zero_or_more(PRINT) %R% columnas[2,2]) 
variable <- data.frame(texto = variable[[1]], stringsAsFactors = FALSE)
variable <- variable %>% mutate(texto = str_extract(texto,'>' %R% one_or_more(PRINT) %R% '<'))
variable <- variable %>% mutate(texto = str_remove_all(texto, '>|<'))




