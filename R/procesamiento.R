# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
# library(encuestar)

Sys.setlocale(locale = "es_ES.UTF-8")

devtools::load_all(path = "../encuestar/")

# Rutas ---------------------------------------------------------------------------------------

# GOOGLE DRIVE
wd_proyecto_drive = "H:/Shared drives/Morant Consultores/Clientes/CesarFaz_EdoMex/Encuesta/2025/Enero/Muestra/"
# edo_name <- "Nezahualcoyotl"
# path_insumos_drive <- paste0(wd_proyecto_drive,edo_name,"/Insumos/")

# LOCALES
path_api_key <- "Insumos/api-key.json"

# link_dicc <- "https://docs.google.com/spreadsheets/d/1cNcfNgkLEV42k7QYc62dnmfNdPmCEofn/edit?usp=drive_link&ouid=107325048037311002262&rtpof=true&sd=true"
# path_dicc <- "Insumos/diccionario_edomex_enero_2025.xlsx"

# link_eliminadas <- "https://docs.google.com/spreadsheets/d/1gWPqb48NvoMz11_L_invde4RmbrQmZyMsj4t5wWY4Hg/edit?gid=0#gid=0"
# path_eliminadas <- "data-raw/bd_eliminadas_enc_edomex_general_ene_2025.xlsx"


# Insumos -----------------------------------------------------------------


dicc <- 
  readxl::read_excel(path = path_dicc) |> 
filter(!grepl(pattern = "Registro", x = bloque)) |>
  # filter(grepl(pattern = "Filtros", x = bloque)) |>
  filter(!llaves %in% c("vivienda", "ine")) |> 
  mutate(tema = gsub(pattern = "NA", replacement = NA, x = tema),
         tema = gsub(pattern = "NANA", replacement = NA, x = tema),
         tema = if_else(is.na(tema), NA_character_, tema),
         tema = stringr::str_trim(string = tema, side = "both"),
         respuestas = stringr::str_split(respuestas, "_")) |>
  filter(!grepl('programa_social_O',llaves)) |> 
  filter(!grepl('problemas_edomex_O',llaves)) |> 
  filter(bloque != "Sitema") |> 
  mutate(llaves =  gsub("demo_","",llaves)) |> 
  mutate(llaves =  gsub("amai_jefe","jefe_grado",llaves),
         llaves =  gsub("amai_wc","cantidad_wc",llaves),
         llaves =  gsub("amai_autos","cantidad_autos",llaves),
         llaves =  gsub("amai_internet","internet",llaves),
         llaves =  gsub("amai_trabajo","trabajo",llaves),
         llaves =  gsub("amai_cuartos","cantidad_cuartos",llaves)
         ) |> 
  filter(llaves!='ocupacion_otro') |> 
  mutate(llaves = gsub("conoce_per_ ","conoce_per_",llaves) ) |> 
  mutate(llaves = gsub("medios_com_o","medios_com_O",llaves) ) |> 
  mutate(llaves = gsub("problemas_edomex_o","problemas_edomex_O",llaves) ) |> 
  mutate(llaves = gsub("programa_social_o","programa_social_O",llaves) )
  
#        respuestas = gsub(pattern = "\\(No leer\\)| \\(No leer\\)|\\(ROTAR\\)|\\(No leer)|:",
#                                   replacement = "",
#                                   x = respuestas))

# Data raw ------------------------------------------------------------------------------------

# Respuestas
bd_respuestas_campo <-
  openxlsx2::read_xlsx(file = "./data-raw/bd_respuestas_enc_edomex_gen_ene_2024.xlsx", na.strings = "-1") |>
  as_tibble()  |> 
  mutate(SECCION = as.character(as.numeric(cluster))) |> 
  rename("jefe_grado" = "amai_jefe", "cantidad_wc"="amai_wc","cantidad_autos"="amai_autos",
         "internet"="amai_internet","trabajo"= "amai_trabajo","cantidad_cuartos"= "amai_cuartos", 
         "indigena"="demo_indigena","estudios" =  "demo_estudios",  "ocupacion"="demo_ocupacion", 
         "salario"="demo_salario"  ) |> 
  rename_with( ~ gsub("conoce_per_ ", "conoce_per_", .),
    .cols = contains("conoce_per_ ")
  ) |> 
  #Bloque AMAI
  mutate(generacion = case_when(edad >= 18 & edad <= 25 ~ "Generación Z (18 a 25 años)",
                                edad >= 26 & edad <= 40 ~ "Millenials (26 a 40 años)",
                                edad >= 41 & edad <= 55 ~ "Generación X (41 a 55 años)",
                                edad >= 56  ~ "Baby Boomers  (56 años o más)"),
         generacion = factor(generacion, levels = c("Generación Z (18 a 25 años)",
                                                    "Millenials (26 a 40 años)",
                                                    "Generación X (41 a 55 años)",
                                                    "Baby Boomers  (56 años o más)"))) |>
  mutate(grado2=case_when(grepl("Primaria",estudios)~"Educación básica",
                          grepl("Secundaria",estudios)~"Educación básica",
                          estudios=="No estudió"~"Educación básica",
                          grepl("Preparatoria",estudios)~"Educación media superior",
                          estudios %in% c("Licenciatura completa","Licenciatura incompleta",
                                          "Doctorado","Diplomado o maestría")~"Educación superior o más",
                          .default = "No contesta")) |>
  mutate(amai_jefegrado=case_when(jefe_grado %in% c("No estudió","No contesta")~0,
                                  jefe_grado=="Primaria incompleta"~6,
                                  jefe_grado=="Primaria completa"~11,
                                  jefe_grado=="Secundaria incompleta"~12,
                                  jefe_grado=="Secundaria completa"~18,
                                  jefe_grado=="Preparatoria incompleta"~23,
                                  jefe_grado=="Preparatoria completa"~27,
                                  jefe_grado=="Licenciatura incompleta"~36,
                                  jefe_grado=="Licenciatura completa"~59,
                                  jefe_grado=="Diplomado o maestría"~85,
                                  jefe_grado=="Diplomado o maestría"~85,
                                  jefe_grado=="Doctorado"~85,.default = NA),
         amai_cantidadwc=case_when(cantidad_wc=="0"~0,
                                   cantidad_wc=="1"~24,
                                   cantidad_wc=="2 o más"~47,
                                   .default = NA),
         amai_cantidadautos=case_when(cantidad_autos=="0"~0,
                                      cantidad_autos=="1"~22,
                                      cantidad_autos=="2 o más"~43,
                                      .default = NA),
         amai_internet=case_when(internet=="No tiene"~0,
                                 internet=="Sí tiene"~32,.default = NA),
         amai_trabajo=case_when(trabajo=="0"~0,
                                trabajo=="1"~15,
                                trabajo=="2"~31,
                                trabajo=="3"~46,
                                trabajo=="4 o más"~61,
                                .default = NA
         ),
         amai_cantidadcuartos=case_when(cantidad_cuartos=="0"~0,
                                        cantidad_cuartos=="1"~8,
                                        cantidad_cuartos=="2"~16,
                                        cantidad_cuartos=="3"~24,
                                        cantidad_cuartos=="4 o más"~32,
                                        .default = NA)) %>%
  mutate(suma_amai = rowSums(select(., contains("amai_")), na.rm = TRUE),
         nivel_socioec=case_when(
           (suma_amai>=0 & suma_amai<=47)~"E",
           (suma_amai>=48 & suma_amai<=94)~"D",
           (suma_amai>=95 & suma_amai<=115)~"D_mas",
           (suma_amai>=116 & suma_amai<=140)~"C_menos",
           (suma_amai>=141 & suma_amai<=167)~"C",
           (suma_amai>=168 & suma_amai<=201)~"C_mas",
           suma_amai>=202~"A_B",.default = NA)) |> 
  mutate(sexo_cat = sexo)


# Categorias procesadas
source(file = "./R/nubes/03_bds_categorias_procesadas.R")


bd_respuestas_campo <-
  bd_respuestas_campo |>
  left_join(bd_categorias_procesada, by = c("SbjNum"="id"))



############################################################
############################################################
############################################################
#
#Reparticion a todos lo municipios
#
############################################################
############################################################
############################################################


vec_muns<- c("NEZAHUALCOYOTL","TOLUCA","ECATEPEC DE MORELOS","NAUCALPAN DE JUAREZ","TEXCOCO","TULTITLAN",
             "ATIZAPAN DE ZARAGOZA","CUAUTITLAN IZCALLI","TLALNEPANTLA DE BAZ","CHIMALHUACAN")


# atizapan

sufij <- "atiz"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "ATIZAPAN DE ZARAGOZA") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_atizapan_ene_2025/data-raw/",excel_nom) )

muni <- "ATIZAPAN DE ZARAGOZA"

bd_respuestas_campo |> 
  filter(municipio == "ATIZAPAN DE ZARAGOZA") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )

# resto edomex

sufij <- "rest_edomex"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(!municipio %in% vec_muns) |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )

muni <- "Resto Del Estado"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(!municipio %in% vec_muns) |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )

 
  
# Neza

sufij <- "neza"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "NEZAHUALCOYOTL") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )

muni <- "NEZAHUALCOYOTL"

bd_respuestas_campo |> 
  filter(municipio == "NEZAHUALCOYOTL") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )

# toluca

sufij <- "toluca"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "TOLUCA") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )

muni <- "TOLUCA"

bd_respuestas_campo |> 
  filter(municipio == "TOLUCA") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )

# ecatepec

sufij <- "ecatepec"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "ECATEPEC DE MORELOS") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )

muni <- "ECATEPEC DE MORELOS"

bd_respuestas_campo |> 
  filter(municipio == "ECATEPEC DE MORELOS") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )

# naucalpan

sufij <- "nauc"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "NAUCALPAN DE JUAREZ") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )


muni <- "NAUCALPAN DE JUAREZ"


bd_respuestas_campo |> 
  filter(municipio == "NAUCALPAN DE JUAREZ") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )


# texcoco

sufij <- "texcoco"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "TEXCOCO") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )


muni <- "TEXCOCO"

bd_respuestas_campo |> 
  filter(municipio == "TEXCOCO") |> #distinct(municipio)
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )

# tultitlan

sufij <- "tulti"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "TULTITLAN") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )


muni <- "TULTITLAN"

bd_respuestas_campo |> 
  filter(municipio == "TULTITLAN") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )



# cuautli

sufij <- "cuatli"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "CUAUTITLAN IZCALLI") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )


muni <- "CUAUTITLAN IZCALLI"


bd_respuestas_campo |> 
  filter(municipio == "CUAUTITLAN IZCALLI") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )



# tlanepantla

sufij <- "tlane"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "TLALNEPANTLA DE BAZ") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )

muni <- "TLALNEPANTLA DE BAZ"


bd_respuestas_campo |> 
  filter(municipio == "TLALNEPANTLA DE BAZ") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )


# chimalhuacan

sufij <- "chimal"

excel_nom<-paste0("bd_respuestas_enc_",sufij,"_ene_2024.xlsx") 
bd_respuestas_campo |> 
  filter(municipio == "CHIMALHUACAN") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_",sufij,"_ene_2025/data-raw/",excel_nom) )


muni <- "CHIMALHUACAN"


bd_respuestas_campo |> 
  filter(municipio == "CHIMALHUACAN") |> 
  writexl::write_xlsx(path =paste0("C:/Users/ivans/GitHub/enc_edomex_estatal_ene_2025/Enero/",muni,"/data-raw/",excel_nom) )
  
  

##################################################################

##################################################################






