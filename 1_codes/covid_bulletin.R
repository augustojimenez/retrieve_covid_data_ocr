rm(list = ls())

# Importing libraries
library(tesseract)
library(magick)
library(dplyr)
library(purrr)
library(reticulate)
library(stringi)

# Setting working directory
setwd("Z:/Datalab New Structure/2_working/7_covid19/1_code")

# This function normalizes list's length (all to 10) by pasting complex province names (two or more words per name) into one. e.g.:
# [1] "01" [2] "Distrito" [3] "Nacional" [4] "100" ... [11] "526"  would become as follows:
# [1] "01" [2] "Distrito Nacional" [3] "100" ... [10] "526"
compress <- function(lista){
   # Getting list's length. A non-complex province name would result in a list with length 10
   len = length(lista)
   # If len == 10, no transformation is necessary
   if(len > 10){
      # The first value in the list correspond to the index. The name starts at the second value
      # The province name's positions goes from 2 to len - 8 (8 being the remaining variable)
      x <- 2:(len - 8)
      
      # Concatenates all the components of the province name, and assigns it to the first position (2)
      lista[x[1]] <- reduce(lista[x], paste)
      
      # Removes all components of the province name but the first one, where the complete name was stored
      lista <- lista[-(x[-1])]
   }
   return(lista)
}

get_date <- function(){
   # Reference date and corresponding bulletin number
   fecha_0 <- as.Date("2021-04-22")
   boletin_0 <- 400
   
   # Calculate the new bulletin's date from the last update in RD_Suavizado.csv
   fecha_df <- read.csv("../2_data/RD_Suavizado.csv")
   
   fecha_t <- try(as.Date(max(as.Date(fecha_df$fecha, format = "%m/%d/%Y"))) + 1,
                  silent = TRUE)
   if(is.na(fecha_t)) fecha_t = as.Date(max(as.Date(fecha_df$fecha))) + 1
   
   # Calculates the new bulletin number
   boletin_t <- boletin_0 + as.numeric(fecha_t - fecha_0)
   
   values <- list(boletin_t = boletin_t,
                fecha_t = fecha_t)
   
   return(values)
}

r_conversion <- function(boletin_t, fecha){
   # Importing provinces' names
   provincias <- read.csv(file = "../2_data/provincias.csv", 
                          col.names = "provincia") %>%
      .$provincia
   
   # Importing image
   img_dir <- paste0("../0_source/bulletin_jpg/boletin", boletin_t, ".jpg")
   img <- image_read(img_dir)
   
   # Displaying image to compare it against the resulting data frame
   print(img)
   
   # Transforming the image and then converting it to text
   text <- img %>%
      image_resize("2400") %>%
      image_convert(type = "Grayscale") %>%
      image_quantize() %>%
      image_reducenoise(radius = 1) %>%
      image_ocr() %>%
      stri_split(fixed = "\n")
   
   # Locating the table's position in image
   i <- which(sapply(text[[1]],
                     function(x, ...) grepl(x = x, ...),
                     pattern = "^(01|OF|oF|0F|o1|O1)"))
   
   # Extracting the values from the table from i to i + 32 (there are 322 obs in the table)
   text <- text[[1]][i:(i + 32)] %>%
      stri_replace_all("", fixed = ",") %>% # removing ',' from numbers to coerce them from string to numeric
      stri_replace_all("0 ", fixed = "- ") %>% # same as previously. A space is added at the end not to replace every occurrence
      sapply(stri_split, fixed = " ") %>% # splits by space
      lapply(compress) # Reformats province
   
   # New data frame's names
   names <- c("cod_provincia", "provincia", "procesadas", "casos_nuevos", "casos_acum",
              "incidencia", "positividad", "recuperados", "defun_nuevos", "defun_acum")
   
   # Converting `text` from list to data.frame
   # Then transforming `df` and reordering variables
   df <- tibble::as_tibble(matrix(unlist(text),
                                  nrow = length(text),
                                  byrow = TRUE,
                                  dimnames = list(provincias, names))) %>%
      mutate(across(c(3:10), as.numeric),
             fecha = fecha,
             provincia = provincias) %>%
      select(fecha, provincia, casos_nuevos, casos_acum, incidencia,
             positividad, recuperados, defun_nuevos, defun_acum, procesadas)
   
   # Summarising data frame to then compare it with `img`
   print(knitr::kable(format(round(colSums(df[3:10],
                                     na.rm = TRUE),
                                   0),
                             big.mark = ",")))
   
   # Adding a Total row to the data frame
   df <- rbind(df, c(as.character(fecha), "Total", colSums(df[3:10])))
   
   return(df)
}

boletin_t <- get_date()$boletin_t
fecha_t <- get_date()$fecha_t

file_pdf <- paste("../0_source/Boletin especial", boletin_t, "- COVID-19.pdf")
file_jpg <- paste0("../0_source/bulletin_jpg/boletin", boletin_t, ".jpg")

updated <- TRUE
if (file.exists(file_pdf)) {
   source_python("covid_bulletin.py")
   print("Se actualizó el archivo boletin.csv, usando el .pdf")
} else if (file.exists(file_jpg)) {
   df <- r_conversion(boletin_t, fecha_t)
   # If needed, fix data frame
   fix(df)
   
   print(knitr::kable(format(round(colSums(mutate(df[1:33, 3:10], 
                                                  across(everything(), as.numeric)),
                                           na.rm = TRUE),
                                   0),
                             big.mark = ",")))
   df <- rbind(df[1:33,], c(as.character(fecha_t), 
                            "Total",
                            colSums(mutate(df[1:33, 3:10], 
                                           across(everything(), as.numeric)),
                                    na.rm = TRUE)))
   
   # Exporting the data frame as a .csv file
   write.csv(df, "../2_data/boletin.csv", row.names = FALSE)
   print("Se actualizó el archivo boletin.csv, usando el .jpg")
} else {
   print(paste("No se encontró el boletin", boletin_t))
   updated <- FALSE
}
