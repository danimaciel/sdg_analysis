pacotes <- c("geobr", "widyr", "igraph", "ggraph", "tidyr", "SnowballC", "stopwords", "tm", "tidytext", "lubridate", "ggthemes", "treemapify", "openxlsx", "rgdal", "sp", "geobr", "maps", "ggmap", "wordcloud", "stopwords", "tm", "plm", "ggplot2", "tidyverse", "dplyr", "readr","readxl", "lme4", "data.table", "tidyr", "stringr")
lapply(pacotes, require, character.only = TRUE)

setwd("C:/Users/danie/Projetos R/Fapesp/publicacoes_ods/Processados_Carol_Evandro")

db <- read.xlsx("sdg_database.xlsx", sheet = 1)

comparar_ods <- function(ods_fapesp, ods_overton) {
  ods_fapesp <- ifelse(is.na(ods_fapesp) || ods_fapesp == "-", "", ods_fapesp)
  ods_overton <- ifelse(is.na(ods_overton) || ods_overton == "", "", ods_overton)
  if (ods_fapesp == "" && ods_overton == "") {
    return("100% Match - vazias")
  }
  else if (ods_fapesp != "" && ods_overton == "") {
    return("Sem Match - Fapesp preenchido")
  }
  else if (ods_fapesp == "" && ods_overton != "") {
    return("Sem Match - Overton preenchido")
  }
  else {
    ods_fapesp_lista <- unlist(strsplit(ods_fapesp, ";", fixed = TRUE))
    ods_overton_lista <- unlist(strsplit(ods_overton, ";", fixed = TRUE))
    intersecao <- intersect(ods_fapesp_lista, ods_overton_lista)
    if (length(intersecao) == 0) {
      return("Sem Match")
    } else if (setequal(ods_fapesp_lista, ods_overton_lista)) {
      return("100% Match")
    } else {
      return("Match Parcial")
    }
  }
}

# Aplicando a função ajustada ao dataframe
overton_cruza_fapesp <- db %>%
  rowwise() %>%
  mutate(cruza_fapesp_overton = comparar_ods(`ODS FAPESP`, ODS_overton))
