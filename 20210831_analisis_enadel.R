# *-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-**-*
# ++ RESULTADOS PRELIMINARES ENADEL 2021 ++ ----------------------------------------------------
# *-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-*-**-*-**-*
#
#
# Héctor Garrido Henríquez
# Analista Cuantitativo. Observatorio Laboral Ñuble
# Docente. Facultad de Ciencias Empresariales
# Universidad del Bío-Bío
# Avenida Andrés Bello 720, Casilla 447, Chillán
# Teléfono: +56-942353973
# http://www.observatoriolaboralnuble.cl

rm(list = ls())


# Si al momento de actualizar los archivos arroja un error la primera vez,
# ejecutar el siguiente código en la powershell de windows de la carpeta del repositorio:
# git pull nombre_repositorio --allow-unrelated-histories


#
Sys.setenv(LANG = "en")


load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

devtools::install_github("martinctc/surveytoolbox")

packages = c("tidyverse", "stringi", "lubridate", 
             "data.table", "srvyr", "pbapply", 
             "ggrepel", "RColorBrewer", "readstata13", 
             "gtable", "gridExtra", "tidytext", 
             "wordcloud", "kableExtra", "captioner", 
             "foreign", "RPostgres", "haven", 
             "rJava", "openxlsx", "timetk", 
             "forecast","sweep", "tidyquant", 
             "ggmap", "rgeos", "ggalt", "maptools", 
             "rgdal", "readxl", "grid", "scales", 
             "fuzzyjoin", "survey", "directlabels", "microbenchmark", 
             "haven", "sjlabelled", "labelled", "surveytoolbox", "multcomp", "XLConnect")

load_pkg(packages)


file_path = "C:/Users/omen03/Documents/enadel2021/SurveyReport-SPSS-8430326-08-31-2021-T061659.sav"

enadel <- haven::read_sav(file_path)

directorio = enadel %>% varl_tb()



directorio$var_label[72]
enadel$Q33 %>% attr('labels')

##############################################################################################
#"C1 - C1. ¿Su empresa tuvo vacantes o contrató personal nuevo durante los últimos 12 meses? "
##############################################################################################s

directorio$var_label[72]
enadel$Q33 %>% attr('labels')

enadel %>% 
  mutate(total = n()) %>% 
  group_by(Q33) %>% 
  summarise(prop = n()/total[1])

#############################################################################################################################
# "B1a - B1a. Por favor indique el rubro o actividad principal de su empresa. Considere la actividad de mayor valor agregad"
#############################################################################################################################

directorio$var_label[43]

cat_rub = enadel %>% extract_vallab("Q24")
cat_q34 = lapply(paste0("Q34A",1:8), function(x) enadel %>% extract_vallab(x))

enadel$Q34A1 %>% attr('labels')

# Cruce entre principales 


cruces = list(c("Q24", "Q34A1"),c("Q24", "Q34A2"),
              c("Q24", "Q34A3"), c("Q24", "Q34A4"),
              c("Q24", "Q34A5"), c("Q24", "Q34A6"),
              c("Q24", "Q34A7"), c("Q24", "Q34A8"))


lapply(1:8, function(x) enadel %>% 
  group_by(Q24) %>% 
  mutate(total = n()) %>% 
  group_by_at(cruces[[x]], .drop = TRUE) %>% 
  summarise(prop = n()/total[1]) %>% 
    filter(eval(str2expression(paste0(paste0(cruces[[x]][2], " == 1"))))) %>% 
    rename(id = Q24) %>% 
    left_join(cat_rub) %>% 
    rename(sector = id, id =eval(paste0(cruces[[x]][2]))) %>% 
    left_join(cat_q34[[x]]) %>% 
    rename(dificultades = eval(paste0(cruces[[x]][2])))
  ) %>% bind_rows() %>% View
  
  



