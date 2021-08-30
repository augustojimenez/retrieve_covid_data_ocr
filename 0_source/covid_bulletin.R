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
   if(len > 10){
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

get_current_bulletin <- function(){
   # Reference date and corresponding bulletin number
   date_0 <- as.Date("2021-04-22")
   bulletin_0 <- 400
   
   # Calculates the new bulletin's date from the last update in RD_Suavizado.csv
   date_df <- read.csv("./2_data/processed/RD_Suavizado.csv")
   
   date <- try(as.Date(max(as.Date(date_df$date, format = "%m/%d/%Y"))) + 1,
                  silent = TRUE)
   if(is.na(date)) date = as.Date(max(as.Date(date_df$date))) + 1
   
   # Calculates the new bulletin number
   bulletin <- bulletin_0 + as.numeric(date - date_0)
   
   values <- list(bulletin = bulletin,
                date = date)
   
   return(values)
}

bulletin_conversion <- function(bulletin, date, img_dir){
   # Importing provinces' names
   province <- read.csv(file = "./1_data/0_raw/1_other/province.csv", 
                          col.names = "province") %>%
      .$province
   
   # Importing image
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
   
   # Extracting the values from the table from i to i + 32 (there are 322 obs in
   # the table)
   text <- text[[1]][i:(i + 32)] %>%
      stri_replace_all("", fixed = ",") %>% # removing ',' from numbers to coerce them from string to numeric
      stri_replace_all("0 ", fixed = "- ") %>% # same as previously. A space is added at the end not to replace every occurrence
      sapply(stri_split, fixed = " ") %>% # splits by space
      lapply(compress) # Reformats province
   
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
             province = province) %>%
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

bulletin <- get_last_updated()$bulletin
date <- get_last_updated()$date

img_dir <- paste0("./1_data/0_raw/0_bulletin/bulletin", bulletin, ".jpg")

if (file.exists(img_dir)) {
   df <- bulletin_conversion(bulletin, date, img_dir)
   
   # If needed, fix data frame
   fix(df)
   
   print(knitr::kable(format(round(colSums(mutate(df[1:33, 3:10], 
                                                  across(everything(),
                                                         as.numeric)),
                                           na.rm = TRUE),
                                   0),
                             big.mark = ",")))
   
   df <- rbind(df[1:33,], c(as.character(date), 
                            "Total",
                            colSums(mutate(df[1:33, 3:10], 
                                           across(everything(), as.numeric)),
                                    na.rm = TRUE)))
   
   # Exporting the data frame as a .csv file
   write.csv(df,
             paste0("./1_data/0_raw/bulletin", date, ".csv"), 
             row.names = FALSE)
   print("The bulletin.csv file was successfully updated.")
} else {
   print(paste("The bulletin", bulletin, "was not found."))
}
