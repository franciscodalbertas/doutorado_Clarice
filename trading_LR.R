# autor: Francisco d'Albertas

#===============================================================================

# arranjos de compensacao de RL para o Estado de SP

#===============================================================================

#==== pacotes ==================================================================

library(dplyr)

#===============================================================================

# abrindo os dados

p <- dirname(getwd()) #diretorio principal

pasta <- "doutorado_Clarice" # pasta onde a tabela esta salva

# abrindo df

df <- read.table(file = file.path(p,pasta,
                                  'Tabela_variaveis_cenarios_3atributos.txt'),
                 sep ="\t" ,header = T)


# dividindo metricas em 12 classes


f <- function(x)cut(x,12) # funcao q divide variavel continua em classes

df <- cbind(df,lapply(df[,c(21,22,23)],FUN = f)) # aplicando funcao nas metricas

# nomeando as novas colunas com as classes
names(df)[c(24:26)] <- c("riqueza_aves_cl","conect_PC400m_cl","polinizacao_cl")

# ordenando dados pela area de excedente, decrescente

df2 <-df %>% arrange(desc(area_exced_foraRL))


#==== encontrando hexagonos equivalentes =======================================

# aqui estou considerando apenas compensacao


i=317 #bom pra testar

lista_exedente <- list() # lista vazia para guardar os resultados
lista_deficit <- list()

c <- 0
c_2 <- 0

# a funcao tem q ser iterativa e ja modificar o df original!
# isso implica que nao da pra guardar o resultado numa lista!

for(i in seq(1,nrow(df2))){
  
  c <- c +1 # contador pra lista de hexagonos com excedente
  
  hex <- df2[i,]
  
  #---- subset hexagonos  com mesmos valores de classe -------------------------
  
  # selecionando apenas hexagonos com deficit
  eq <- df2[df2$riqueza_aves_cl==hex$riqueza_aves_cl&
             df2$conect_PC400m_cl==hex$conect_PC400m_cl&
             df2$polinizacao_cl==hex$polinizacao_cl&df2$area_deficit_oficial>0,]
  
  if(length(eq)==0){
    hex$valor_comp <- 0
    hex$area_exced_foraRL_nv <- hex$area_exced_foraRL
    hex$id_hex_def <- NA
    lista_exedente[[c]] <- hex
  } else{
  #---- ordenando hexagonos equivalentes pela area de deficit ------------------
    eq <- eq %>% arrange(desc(area_deficit_oficial))
    
  #---- selecionando valores pra serem compensados -----------------------------
    
    # se a area de deficit for maior ou igual  a area de excedente:
    if (eq$area_deficit_oficial>=hex$area_exced_foraRL){
      
      c_2 <- c_2+1 # contador pra lista de hexagonos com deficit compensados
      
      # selecionar apenas 1 hexagono
      
      comp <- eq[1,]
 
      #---- atualizando valores do dataframe -----------------------------------
      
      # criando novo objeto com o hexagono com excedente
      hex2 <- hex
      # descontando valor compensado
      hex2$area_exced_foraRL_nv <- 0
      # valor compensado
      hex2$valor_comp <- hex$area_exced_foraRL*hex$preco_compensacao
      hex2$id_hex_def <- comp$id_hexagono
      # descontando valor deficit!
      comp$area_deficit_oficial_nv <- comp$area_deficit_oficial-hex$area_exced_foraRL
      lista_exedente[[c]] <- hex2
      lista_deficit <- comp[[c_2]]
      
    }else{
        # se a area de deficit for menor q a de excedente
        #continuar
      
      }
    
  }
  
  #---- ordenando pelo valor do deficit ----------------------------------------
  
  
  
  
  
  # #---- armazenando ids dos hexagonos ----------------------------------------
  # 
  # ids <- eq$id_hexagono # quais ids sao equivalentes
  # hex$eq_hex_ids <- paste(ids,collapse = ",") # inserindo ids em uma coluna
  # hex$n_eq <- length(ids) # colocando n de hexagonos equivalentes
  # #---- guarando resultados na lista vazia
  lista_hexagonos[[i]] <- hex
}

df3 <- do.call(rbind,lista_hexagonos)
                     
hist(df3$area_comp)            
summary(as.factor(df3$area_comp))         
