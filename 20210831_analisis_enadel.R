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

# Cruce entre 

enadel %>% 
  group_by(Q24) %>% 
  mutate(total = n()) %>% 
  group_by(Q33, Q24) %>% 
  summarise(prop = n()/total[1])



