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

# ordenando dados pelo custo de compensacao, decrescente

df2 <-df %>% arrange(desc(preco_CO))

#==== encontrando hexagonos equivalentes =======================================

# aqui estou considerando apenas compensacao


i=317 #bom pra testar
i=1574 # id 1446

df3 <- df2 # df para ser modificado pela funcao!


# limite superior do for

lim <- nrow(df2)
lim <- 60

i=61

for(i in seq(1,lim)){
  
  
  
    # hex q recebera compensacao
    hex <- df3[i,]
    # se o excedente for ==0, pular
    if(hex$area_exced_foraRL==0){next}else{
    
      #---- subset hexagonos  com mesmos valores de classe -------------------------
    
    # selecionando apenas hexagonos com deficit
    eq <- df3[df3$riqueza_aves_cl==hex$riqueza_aves_cl&
               df3$conect_PC400m_cl==hex$conect_PC400m_cl&
               df3$polinizacao_cl==hex$polinizacao_cl&df3$area_deficit_oficial>0,]
    
    if(nrow(eq)==0){
      # substituindo df original!
      df3$valor_comp[df3$id_hexagono==hex$id_hexagono] <- 0
      df3$id_hex_def[df3$id_hexagono==hex$id_hexagono] <- NA
      
    } else{
    #---- ordenando hexagonos equivalentes pela area de deficit ------------------
      eq <- eq %>% arrange(desc(area_deficit_oficial))
      
    #---- selecionando valores pra serem compensados -----------------------------
      
      # se a area de deficit do primeiro hex for maior ou igual  a area de excedente:
      if (eq$area_deficit_oficial[1]>=hex$area_exced_foraRL){
        
        # selecionar apenas 1 hexagono
        
        def <- eq[1,] # deficit a ser compensado ( ta errado pq nao pode ter deficit 0)
   
        #---- atualizando valores do dataframe -----------------------------------
        # descontando valor compensado
        df3$area_exced_foraRL[df3$id_hexagono==hex$id_hexagono] <- 0
        # valor compensado
        df3$valor_comp[df3$id_hexagono==hex$id_hexagono] <- hex$area_exced_foraRL*hex$preco_compensacao
        df3$id_hex_def[df3$id_hexagono==hex$id_hexagono] <- def$id_hexagono
        # descontando valor deficit! aqui precisa ser no hexagono q foi compensado!
        df3$area_deficit_oficial[df3$id_hexagono==def$id_hexagono] <- 
        df3$area_deficit_oficial[df3$id_hexagono==def$id_hexagono]-
        hex$area_exced_foraRL
        
        
      }else{
        # se a area de deficit for menor que o excedente, pode ser q um hexagono
        # compense mais de 1, ai precisa incluir na coluna.
        
        # area de excedente:
        area_exced <- hex$area_exced_foraRL
        eq$cumsum <- cumsum(eq$area_deficit_oficial)
        # subset apenas com areas q completam o excedente. Caso nao complete, o hexagono
        # continua na proxima
        def <- eq[eq$cumsum<=area_exced,]
        # area total compensada
        area_compensada <- sum(def$area_deficit_oficial)
        # descontando valor compensado ( ta gerando valores negativos!)
        df3$area_exced_foraRL[df3$id_hexagono==hex$id_hexagono] <- area_exced-
          area_compensada
        # valor compensado
        df3$valor_comp[df3$id_hexagono==hex$id_hexagono] <- area_compensada*hex$preco_compensacao
        df3$id_hex_def[df3$id_hexagono==hex$id_hexagono] <- paste(eq2$id_hexagono,collapse = ",")
        #print("vai dar certo")
        # descontando valor deficit! compensa o deficit todo, logo é 0
        df3$area_deficit_oficial[df3$id_hexagono %in%def$id_hexagono] <- 0
          
        }
      }
    }
}

# salvando dados pos compensacao em nova planilha

write.csv(df3,"Tabela_variaveis_cenarios_3atributos_pos_comp.csv",row.names = F)
