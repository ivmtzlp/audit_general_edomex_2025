# Preambulo -----------------------------------------------------------------------------------

library(dplyr)

# Rutas ---------------------------------------------------------------------------------------

link_aprobadas <- "https://docs.google.com/spreadsheets/d/1x1L-2Gsh5KcYksua05Bc0NNgEGAzSiPMble9B77CW9E/edit?usp=sharing"
bot_path <- "../categoriza_bot/data/enc_edomex_estatal_2025/Inputs/"


source(file = "./R/nubes/nubes_preproc.R")

# Insumos -------------------------------------------------------------------------------------

path_bd_categorias <-
  "./data-raw/bd_categorias.xlsx"

googledrive::drive_download(file = link_aprobadas,
                            path = path_bd_categorias,
                            overwrite = T)
2

bd_categorias_raw_gene <-
  readxl::read_xlsx(path = path_bd_categorias, skip = 2) 

bd_categorias_raw_mun <-
  readxl::read_xlsx(path = path_bd_categorias, skip = 1,sheet = 2) 

# /-/-/-/-/-/-/-/ Archivos para categorizar respuestas /-/-/-/-/-/-/-/-/-/- ------------------------------



vec_cats_pros <- c()

# +-+-+-+-+-+-+- Generales +-+-+-+-+-+-+-+-+- --------------------------------------------------------------------------------------


# Rutina --------------------------------------------------------------------------------------


# Aprueba caludia delfina


asepctos_nubes<-c("claudia","delfina")
  
#################3


for(aspecto in asepctos_nubes){
  
  
  razon_op_buena_cat<- paste0("razon_aprueba_per_",aspecto,"_buena")
  razon_op_mala_cat<- paste0("razon_aprueba_per_",aspecto,"_mala")
  razon_op_personaje <- paste0("razon_aprueba_per_",aspecto)
  op_personaje <- paste0("aprueba_per_",aspecto)
  
  
  # Razon opinion positiva
  categorias <-
    bd_categorias_raw_gene |>
    select(!!rlang::sym(razon_op_buena_cat)) |>
    na.omit()
  
  writexl::write_xlsx(x = list("encuesta" = bd_resp_efect |>
                                 as_tibble() |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Aprueba mucho")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_buena_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_buena_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    razon_op_buena_cat,
                                    ".xlsx"))
  
  
  # Razon opinion negativa
  
  categorias <-
    bd_categorias_raw_gene |>
    select(!!rlang::sym(razon_op_mala_cat)) |>
    na.omit()
  
  writexl::write_xlsx(x = list("encuesta" =  bd_resp_efect |>
                                 as_tibble() |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Desaprueba mucho")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_mala_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_mala_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    razon_op_mala_cat,
                                    ".xlsx"))
  
  
  
  vec_cats_pros <- c(vec_cats_pros,razon_op_buena_cat,razon_op_mala_cat)
}





# opinion horacio


asepctos_nubes<-c("horacio")

#################3


for(aspecto in asepctos_nubes){
  
  
  razon_op_buena_cat<- paste0("razon_opinion_per2_",aspecto,"_buena")
  razon_op_mala_cat<- paste0("razon_opinion_per2_",aspecto,"_mala")
  razon_op_personaje <- paste0("razon_opinion_per2_",aspecto)
  op_personaje <- paste0("opinion_per2_",aspecto)
  
  
  # Razon opinion positiva
  categorias <-
    bd_categorias_raw_gene |>
    select(!!rlang::sym(razon_op_buena_cat)) |>
    na.omit()
  
  writexl::write_xlsx(x = list("encuesta" = bd_resp_efect |>
                                 as_tibble() |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Buena")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_buena_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_buena_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    razon_op_buena_cat,
                                    ".xlsx"))
  
  
  # Razon opinion negativa
  
  categorias <-
    bd_categorias_raw_gene |>
    select(!!rlang::sym(razon_op_mala_cat)) |>
    na.omit()
  
  writexl::write_xlsx(x = list("encuesta" =  bd_resp_efect |>
                                 as_tibble() |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Mala")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_mala_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_mala_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    razon_op_mala_cat,
                                    ".xlsx"))
  
  
  
  vec_cats_pros <- c(vec_cats_pros,razon_op_buena_cat,razon_op_mala_cat)
}



# +-+-+-+-+-+-+- Municipales +-+-+-+-+-+-+-+-+- --------------------------------------------------------------------------------------


# Rutina --------------------------------------------------------------------------------------


vec_munis_cto<- c("atiz","nauc","tulti","toluca","tlane","texcoco","neza","ecatepec","chimal","cuatli")


vec_munis_comp<- c("ATIZAPAN DE ZARAGOZA","NAUCALPAN DE JUAREZ","TULTITLAN", "TOLUCA", "TLALNEPANTLA DE BAZ",
                   "TEXCOCO", "NEZAHUALCOYOTL", "ECATEPEC DE MORELOS", "CHIMALHUACAN", "CUAUTITLAN IZCALLI")


i <-1 

#################3


for(i in 1:length(vec_munis_cto)){
  
  
  razon_op_buena_cat<- paste0("razon_aprueba_per_pm_",vec_munis_cto[i],"_buena")
  razon_op_mala_cat<- paste0("razon_aprueba_per_pm_",vec_munis_cto[i],"_mala")
  razon_op_personaje <- paste0("razon_aprueba_per_pm")
  op_personaje <- paste0("aprueba_per_pm")
  
  
  # Razon opinion positiva
  categorias <-
    bd_categorias_raw_mun |>
    select(!!rlang::sym(razon_op_buena_cat)) |>
    na.omit()
  
  writexl::write_xlsx(x = list("encuesta" = bd_resp_efect |>
                                 as_tibble() |>
                                 filter(municipio ==   vec_munis_comp[i]) |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Aprueba mucho")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_buena_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_buena_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    razon_op_buena_cat,
                                    ".xlsx"))
  
  
  # Razon opinion negativa
  
  categorias <-
    bd_categorias_raw_mun |>
    select(!!rlang::sym(razon_op_mala_cat)) |>
    na.omit()
  
  writexl::write_xlsx(x = list("encuesta" =  bd_resp_efect |>
                                 as_tibble() |>
                                 filter(municipio ==   vec_munis_comp[i]) |>
                                 filter(!!rlang::sym(op_personaje) %in% c("Desaprueba mucho")) |>
                                 select(id = SbjNum,
                                        !!rlang::sym(razon_op_mala_cat) := !!rlang::sym(razon_op_personaje)) |>
                                 na.omit(!!rlang::sym(razon_op_mala_cat)),
                               "categorias" = categorias),
                      path = paste0(bot_path,
                                    razon_op_mala_cat,
                                    ".xlsx"))
  
  
  
  vec_cats_pros <- c(vec_cats_pros,razon_op_buena_cat,razon_op_mala_cat)
}




vec_cats_pros |> 
  paste(collapse = "','")



















