# Generar glosarios ---------------------------------------------------------------------------
path_bd_categorias <- "./data-raw/bd_categorias.xlsx"

bd_categorias_raw_gene <-
  readxl::read_xlsx(path = path_bd_categorias, skip = 2) 

bd_categorias_raw_mun <-
  readxl::read_xlsx(path = path_bd_categorias, skip = 1,sheet = 2) 

##


for (cate in vec_cats_pros[1:6]) {
  #print(cate)


encuestar:::generarGlosario_preguntaAbierta(folder = "./R/nubes/glosarios/",
                                            prefijo = "glosario_",
                                            bd_categorias_raw = bd_categorias_raw_gene,
                                            variable = cate)

}


for (cate in vec_cats_pros[7:26]) {
  #print(cate)
  
  
  encuestar:::generarGlosario_preguntaAbierta(folder = "./R/nubes/glosarios/",
                                              prefijo = "glosario_",
                                              bd_categorias_raw = bd_categorias_raw_mun,
                                              variable = cate)
  
}



