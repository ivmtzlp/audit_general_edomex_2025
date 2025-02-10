


# Se carga la base de Eliminadas
link_eliminadas <- "https://docs.google.com/spreadsheets/d/1gWPqb48NvoMz11_L_invde4RmbrQmZyMsj4t5wWY4Hg/edit?gid=0#gid=0"
path_eliminadas <- paste0("./data-raw/bd_eliminadas_enc_edomex_general_ene_2025.xlsx")

path_api_key <- "Insumos/api-key.json"

# Eliminadas
googledrive::drive_auth(path = path_api_key)
2
googledrive::drive_download(file = link_eliminadas,
                            path = path_eliminadas,
                            overwrite = T)

bd_eliminadas <-
  readxl::read_xlsx(path = path_eliminadas) |>
  transmute(SbjNum = ID,
            razon = Observaciones)


# Se utiliza la base de respuestas de campo para filtrar las encuestas eliminadas.
bd_resp_efect <- 
bd_respuestas_campo |> 
  filter(!SbjNum %in% bd_eliminadas$SbjNum)


