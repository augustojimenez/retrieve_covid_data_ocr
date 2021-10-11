########################################################################
#\ Project              : COVID-19                                    /#
#\ Autor                : Cesar Augusto Jimenez Sanchez               /#
#\ Created              : Apr 04 2021                                 /#
#\ Modified             : Ago 30 2021                                 /#
#\ Objetives            : Converting COVID-19 bulletin, in jpg format,/#
#\                        to csv. Bulletins are extracted from the    /#
#\                        Ministry of Health of the Dominican         /#
#\                        Republic's Instagram webpage.               /#
########################################################################

rm(list = ls())

# Importing libraries
library(tesseract)
library(magick)
library(dplyr)
library(purrr)
library(reticulate)
library(stringi)

# This function normalizes list's length (all to 10) by pasting complex province
# names (two or more words per name) into one. e.g.:
# [1] "01" [2] "Distrito" [3] "Nacional" [4] "100" ... [11] "526"  
# would become as follows:
# [1] "01" [2] "Distrito Nacional" [3] "100" ... [10] "526"
compress <- function(lista){
   # Getting list's length. A non-complex province name would result in a list
   # with length 10
   len = length(lista)
   # If len == 10, no transformation is necessary
   if (len > 10) {
      # The first value in the list correspond to the index. The name starts at
      # the second value
      # The province name's positions goes from 2 to len - 8 
      # (8 being the remaining variable)
      x <- 2:(len - 8)
      
      # Concatenates all the components of the province name, and assigns it to
      # the first position (2)
      lista[x[1]] <- reduce(lista[x], paste)
      
      # Removes all components of the province name but the first one, where 
      # the complete name was stored
      lista <- lista[-(x[-1])]
   }
   return(lista)
}

# Gets the date and number corresponding to the bulletin to be updated
get_current <- function(){
   # Reference date and corresponding bulletin number
   date_0 <- as.Date("2021-04-22")
   bulletin_0 <- 400
   
   # Determining last processed bulleting, and then the new bulletin and date
   bulletin_csv <- list.files("./1_data/1_processed/")
   bulletin <- max(as.numeric(stri_replace_all(bulletin_csv,
                                               "",
                                               fixed = ".csv"))) + 1
   date <- date_0 + bulletin - bulletin_0
   
   values <- list(bulletin = bulletin,
                  date = date)
   
   return(values)
}

bulletin_conversion <- function(bulletin, date, img_dir){
   # Importing provinces' names
   provinces <- read.csv(file = "./1_data/0_raw/1_other/province.csv", 
                          col.names = "province") %>%
      .$province
   
   # Importing image
   img <- image_read(img_dir)
   
   # Displaying image to compare it against the resulting data frame
   print(img)
   
   # Preprocessing the image and then converting it to text
   text <- img %>%
      image_resize("2400") %>%
      image_convert(type = "Grayscale") %>%
      image_quantize() %>%
      image_reducenoise(radius = 1) %>%
      image_ocr() %>%
      stri_split(fixed = "\n")
   
   # Removing empty spaces
   text = text[[1]][text[[1]][] != ""]
   
   # Locating the table's position in the image
   i <- which(sapply(text,
                     function(x, ...) grepl(x = x, ...),
                     # The table starts at "01", which Tesseract sometimes
                     # interprets it as: OF, oF, 0F, o1, or O1
                     pattern = "^(01|OF|oF|0F|o1|O1)"))
   
   # Extracting the values from the table from i to i + 32 (there are 322 obs in
   # the table)
   text <- text[i:(i + 32)] %>%
      # removing ',' from numbers to coerce them from string to numeric
      stri_replace_all("", fixed = ",") %>%
      # A space is added at the end not to replace every occurrence
      stri_replace_all("0 ", fixed = "- ") %>%
      # splits by space
      sapply(stri_split, fixed = " ") %>%
      # Reformats province's name
      lapply(compress) 
   
   # New data frame's names
   names <- c("cod_province", "province", "processed", "new_cases",
              "accu_cases", "incidence", "positivity", "recovered",
              "new_deaths", "accu_deaths")
   
   # Converting `text` from list to data.frame
   # Then transforming `df` and reordering variables
   df <- tibble::as_tibble(matrix(unlist(text),
                                  nrow = length(text),
                                  byrow = TRUE,
                                  dimnames = list(province, names))) %>%
      mutate(across(c(3:10), as.numeric),
             date = date,
             province = provinces) %>%
      select(date, province, new_cases, accu_cases, incidence,
             positivity, recovered, new_deaths, accu_deaths, processed)
   
   # Summarising data frame to then compare it with `img`
   print(knitr::kable(format(round(colSums(df[3:10],
                                     na.rm = TRUE),
                                   0),
                             big.mark = ",")))
   
   # Adding a Total row to the data frame
   df <- rbind(df, c(as.character(date), "Total", colSums(df[3:10])))
   
   return(df)
}

# Bulletin and date to be updated
bulletin <- get_current()$bulletin
date <- get_current()$date

# Direction of the bulletin in jpg format
img_dir <- paste0("./1_data/0_raw/0_bulletin/", bulletin, ".jpg")

if (file.exists(img_dir)) {
   # Converting image to data frame
   df <- bulletin_conversion(bulletin, date, img_dir)
   
   # If needed, fix data frame
   fix(df)
   
   # Printing a summary to compare it against the image
   print(knitr::kable(format(round(colSums(mutate(df[1:33, 3:10], 
                                                  across(everything(),
                                                         as.numeric)),
                                           na.rm = TRUE),
                                   0),
                             big.mark = ",")))
   
   # Excluding "Total" row
   df <- df[1:33,]
   
   # Exporting the data frame as a .csv file
   write.csv(df,
             paste0("./1_data/1_processed/", bulletin, ".csv"), 
             row.names = FALSE)
   print(paste0("The bulletin file (",
                bulletin,
                ".csv) was successfully saved."))
} else {
   print(paste("The bulletin", bulletin, "was not found."))
}
