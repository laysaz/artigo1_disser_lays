# ANÁLISE DOS DADOS DE VOLÊNCIA DOMÉSTICA CONTRA A MULHER NO BRASIL - 2015 A 2020 - BRASIL
## Projeto de dissertação
#Lays Silva de Azevedo
#laysaz@outlook.com
#
#--------------------------------------------------------------------------------------------------------------

install.packages("data.table")
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
install.packages("lubridate")
library(data.table)
library(lubridate)
library(stringr)
getwd()
setwd("~/Enfermagem/Mestrado/Dados/dados_limpos/violência_doméstica_contra_a_mulher_12_59_anos")

#unificar base de dados

dados15 <- fread("viol_br_2015.csv")
dados16 <- fread("viol_br_2016.csv")
dados17 <- fread("viol_br_2017.csv")
dados18 <- fread("viol_br_2018.csv")
dados19 <- fread("viol_br_2019.csv")
dados20 <- fread("viol_br_2020.csv")

dados <- rbind(dados15, dados16, dados17,
               dados18, dados19, dados20, 
               fill=T)
write.csv2(dados,"dados.csv")

#_______________________________________________________________________________

# processamento

dados_mutate <- dados %>% mutate(
  
  idade = case_when(
    NU_IDADE_N <= 4014 ~ "1", 
    NU_IDADE_N <= 4019 ~ "2",
    NU_IDADE_N <= 4029 ~ "3",
    NU_IDADE_N <= 4039 ~ "4",
    NU_IDADE_N <= 4049 ~ "5",
    NU_IDADE_N <= 4059 ~ "6"
  ), 
  
  regiao = substr(SG_UF,1,1), 
  
  raca = case_when(
    CS_RACA == 1 ~ "1 - branca",
    CS_RACA == 2 ~ "2 - preta",
    CS_RACA == 3 ~ "3 - amarela",
    CS_RACA == 4 ~ "4 - parda",
    CS_RACA == 5 ~ "5 - indigena"
    ),
  
  escolaridade = case_when(
    as.numeric(CS_ESCOL_N) <= 3 ~ "1 - < 8 anos",
    as.numeric(CS_ESCOL_N) <= 8 ~ "2 - >= 8 anos"
  ),
  
  sit_conj = case_when(
    SIT_CONJUG == 1 ~ "1 - solteira",
    SIT_CONJUG == 2 ~ "2 - casada",
    SIT_CONJUG == 3 ~ "3 - viúva",
    SIT_CONJUG == 4 ~ "4 - separada"
  ),
  
  gestante = case_when(
    CS_GESTANT <= 4 ~ "1 - sim",
    CS_GESTANT <= 6 ~ "2 -nao"
  ),
  
  viol_repet = case_when(
    OUT_VEZES == 1 ~ "1 - sim",
    OUT_VEZES == 2 ~ "2 - nao"
  ),
  
  viol_fisica = case_when(
    VIOL_FISIC == 1 ~ "1 - sim",
    VIOL_FISIC == 2 ~ "2 - nao"
  ),
  
  viol_psicologica = case_when(
    VIOL_PSICO == 1 ~ "1 - sim",
    VIOL_PSICO == 2 ~ "2 - nao"
  ),
  
  viol_tortura = case_when(
    VIOL_TORT == 1 ~ "1 - sim",
    VIOL_TORT == 2 ~ "2 - nao"
  ),
  
  viol_sexual = case_when(
    VIOL_SEXU == 1 ~ "1 - sim",
    VIOL_SEXU == 2 ~ "2 - nao"
  ),
  
  viol_trafico = case_when(
    VIOL_TRAF == 1 ~ "1 - sim",
    VIOL_TRAF == 2 ~ "2 - nao"
  ),
  
  viol_financeira = case_when(
    VIOL_FINAN == 1 ~ "1 - sim",
    VIOL_FINAN == 2 ~ "2 - nao"
  ),
  
  viol_negligencia = case_when(
    VIOL_NEGLI == 1 ~ "1 - sim",
    VIOL_NEGLI == 2 ~ "2 - nao"
  ),
  
  viol_trab_infan = case_when(
    VIOL_INFAN == 1 ~ "1 - sim",
    VIOL_INFAN == 2 ~ "2 - nao"
  ),
  
  viol_interv_legal = case_when(
    VIOL_LEGAL == 1 ~ "1 - sim",
    VIOL_LEGAL == 2 ~ "2 - nao"
  ),
  
  viol_outros = case_when(
    VIOL_OUTR == 1 ~ "1 - sim",
    VIOL_OUTR == 2 ~ "2 - nao"
  ),
  
  agressao_forca = case_when(
    AG_FORCA == 1 ~ "1 - sim",
    AG_FORCA == 2 ~ "2 - nao"
  ),
  
  agressao_enforcamento = case_when(
    AG_ENFOR == 1 ~ "1 - sim",
    AG_ENFOR == 2 ~ "2 - nao"
  ),
  
  agressao_obj_cont = case_when(
    AG_OBJETO == 1 ~ "1 - sim",
    AG_OBJETO == 2 ~ "2 - nao"
  ),
  
  agressao_obj_corte = case_when(
    AG_CORTE == 1 ~ "1 - sim",
    AG_CORTE == 2 ~ "2 - nao"
  ),
  
  agressao_quente = case_when(
    AG_QUENTE == 1 ~ "1 - sim",
    AG_QUENTE == 2 ~ "2 - nao"
  ),
  
  agressao_envenen = case_when(
    AG_ENVEN == 1 ~ "1 - sim",
    AG_ENVEN == 2 ~ "2 - nao"
  ),
  
  agressao_fogo = case_when(
    AG_FOGO == 1 ~ "1 - sim",
    AG_FOGO == 2 ~ "2 - nao"
  ),
  
  agressao_ameaca = case_when(
    AG_AMEACA == 1 ~ "1 - sim",
    AG_AMEACA == 2 ~ "2 - nao"
  ),
  
  agressao_outros = case_when(
    AG_OUTROS == 1 ~ "1 - sim",
    AG_OUTROS == 2 ~  "2 - nao"
  ),
    
  sex_assedio = case_when(
    SEX_ASSEDI == 1 ~ "1 - sim",
    SEX_ASSEDI == 2 ~ "2 - nao"
  ),
  
  sex_estupro = case_when(
    SEX_ESTUPR == 1 ~ "1 - sim",
    SEX_ESTUPR == 2 ~ "2 - nao"
  ),
  
  sex_pornog_inf = case_when(
    SEX_PORNO == 1 ~ "1 - sim",
    SEX_PORNO == 2 ~ "2 - nao"
  ),
  
  sex_exploracao = case_when(
    SEX_EXPLO == 1 ~ "1 - sim",
    SEX_EXPLO == 2 ~ "2 - nao"
  ),
  
  sex_outros = case_when(
    SEX_OUTRO == 1 ~ "1 - sim",
    SEX_OUTRO == 2 ~ "2 - nao"
  ),
    
  relacao_pai = case_when(
    REL_PAI == 1 ~ "1 - sim",
    REL_PAI == 2 ~ "2 - nao"
  ),
  
  relacao_mae = case_when(
    REL_MAE == 1 ~ "1 - sim",
    REL_MAE == 2 ~ "2 - nao"
  ),
  
  relacao_padrasto = case_when(
    REL_PAD == 1 ~ "1 - sim",
    REL_PAD == 2 ~ "2 - nao"
  ),
  
  relacao_madrasta = case_when(
    REL_MAD == 1 ~ "1 - sim",
    REL_MAD == 2 ~ "2 - nao"
  ),
  
  relacao_conjuge = case_when(
    REL_CONJ == 1 ~ "1 - sim",
    REL_CONJ == 2 ~ "2 - nao"
  ),
  
  relacao_exconj = case_when(
    REL_EXCON == 1 ~ "1 - sim",
    REL_EXCON == 2 ~ "2 - nao"
  ),
  
  relacao_namorado = case_when(
    REL_NAMO == 1 ~ "1 - sim",
    REL_NAMO == 2 ~ "2 - nao"
  ),
  
  relacao_exnamo = case_when(
    REL_EXNAM == 1 ~ "1 - sim",
    REL_EXNAM == 2 ~ "2 - nao"
  ),
  
  relacao_filho = case_when(
    REL_FILHO == 1 ~ "1 - sim",
    REL_FILHO == 2 ~ "2 - nao"
  ),
  
  relacao_irmao = case_when(
    REL_IRMAO == 1 ~ "1 - sim",
    REL_IRMAO == 2 ~ "2 - nao" 
  ),
  
  relacao_outros_fam = case_when(
    REL_OUTROS == 1 ~ "1 - sim",
    REL_OUTROS == 2 ~ "2 - nao"
  ),
  
  sexo_autor = case_when(
    AUTOR_SEXO == 1 ~ "1 - masculino",
    AUTOR_SEXO == 2 ~ "2 - feminino",
    AUTOR_SEXO == 3 ~ "3 - ambos"
  ),
  
  uso_alcool = case_when(
    AUTOR_ALCO == 1 ~ "1 - sim",
    AUTOR_ALCO == 2 ~ "2 - nao"
  ),
  
  enc_rede_atend_mulh = case_when(
    ATEND_MULH == 1 ~ "1 - sim, para servico especializado",
    ATEND_MULH == 2 ~ "2 - nao, para servico especializado"
  ),
  
  enc_deleg_mulh = case_when(
    DELEG_MULH == 1 ~ "1 - sim, para servico especializado",
    DELEG_MULH == 2 ~ "2 - nao, para servico especializado"
  ),
  
  enc_rede_saude = case_when(
    REDE_SAU == 1 ~ "1 - sim, para outros servicos",
    REDE_SAU == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_assist_social = case_when(
    ASSIST_SOC == 1 ~ "1 - sim, para outros servicos",
    ASSIST_SOC == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_rede_educacao = case_when(
    REDE_EDUCA == 1 ~ "1 - sim, para outros servicos",
    REDE_EDUCA == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_cons_tutelar = case_when(
    CONS_TUTEL == 1 ~ "1 - sim, para outros servicos",
    CONS_TUTEL == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_cons_idoso = case_when(
    CONS_IDO == 1 ~ "1 - sim, para outros servicos",
    CONS_IDO == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_deleg_idoso = case_when(
    DELEG_IDOS == 1 ~ "1 - sim, para outros servicos",
    DELEG_IDOS == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_dir_humanos = case_when(
    DIR_HUMAN == 1 ~ "1 - sim para outros servicos",
    DIR_HUMAN == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_mpu = case_when(
    MPU == 1 ~ "1 - sim, para outros servicos",
    MPU == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_deleg_crianca = case_when(
    DELEG_CRIA == 1 ~ "1 - sim, para outros servicos",
    DELEG_CRIA == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_outras_deleg = case_when(
    DELEG == 1 ~ "1 - sim, para outros servicos",
    DELEG == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_infan_juven = case_when(
    INFAN_JUV == 1 ~ "1 - sim, para outros servicos",
    INFAN_JUV == 2 ~ "2 - nao, para outros servicos"
  ),
  
  enc_defen_publica = case_when(
    DEFEN_PUBL == 1 ~ "1 - sim, para outros servicos",
    DEFEN_PUBL == 2 ~ "2 - nao, para outros servicos"
  ),
  
  motivacao = case_when(
    VIOL_MOTIV == 01 ~ "1 - sexismo",
    VIOL_MOTIV == 02 ~ "2 - homofobia",
    VIOL_MOTIV == 03 ~ "3 - racismo",
    VIOL_MOTIV == 04 ~ "4 - religiao",
    VIOL_MOTIV == 05 ~ "5 - xenofobia",
    VIOL_MOTIV == 06 ~ "6 - geracional",
    VIOL_MOTIV == 07 ~ "7 - situacao de rua",
    VIOL_MOTIV == 08 ~ "8 - deficiencia",
    VIOL_MOTIV == 09 ~ "9 - outros"
  )
  
) # endmutate


table(dados_mutate$idade,dados_mutate$regiao,useNA = "always")
table(dados_mutate$raca,dados_mutate$regiao,useNA = "always")
table(dados_mutate$escolaridade,dados_mutate$regiao,useNA = "always")
table(dados_mutate$sit_conj,dados_mutate$regiao,useNA = "always")
table(dados_mutate$gestante,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_repet,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_fisica,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_psicologica,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_tortura,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_sexual,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_trafico,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_financeira,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_negligencia,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_trab_infan,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_interv_legal,dados_mutate$regiao,useNA = "always")
table(dados_mutate$viol_outros,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_forca,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_enforcamento,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_obj_cont,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_obj_corte,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_quente,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_envenen,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_fogo,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_ameaca,dados_mutate$regiao,useNA = "always")
table(dados_mutate$agressao_outros,dados_mutate$regiao,useNA = "always")
table(dados_mutate$sex_assedio,dados_mutate$regiao,useNA = "always")
table(dados_mutate$sex_estupro,dados_mutate$regiao,useNA = "always")
table(dados_mutate$sex_pornog_inf,dados_mutate$regiao,useNA = "always")
table(dados_mutate$sex_exploracao,dados_mutate$regiao,useNA = "always")
table(dados_mutate$sex_outros,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_pai,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_mae,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_padrasto,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_madrasta,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_conjuge,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_exconj,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_namorado,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_exnamo,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_filho,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_irmao,dados_mutate$regiao,useNA = "always")
table(dados_mutate$relacao_outros_fam,dados_mutate$regiao,useNA = "always")
table(dados_mutate$sexo_autor,dados_mutate$regiao,useNA = "always")
table(dados_mutate$uso_alcool,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_rede_atend_mulh,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_deleg_mulh,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_rede_saude,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_assist_social,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_rede_educacao,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_cons_tutelar,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_cons_idoso,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_deleg_idoso,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_dir_humanos,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_mpu,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_deleg_crianca,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_outras_deleg,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_infan_juven,dados_mutate$regiao,useNA = "always")
table(dados_mutate$enc_defen_publica,dados_mutate$regiao,useNA = "always")
table(dados_mutate$motivacao,dados_mutate$regiao,useNA = "always")



#Proporção / percentual da coluna
prop.table(table(),2)


# para arredondar proporções - nº final é o total de casas após a ,
round(prop.table(table(dados_mutate$idade,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$raca,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$escolaridade,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$sit_conj,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$gestante,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_repet,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_fisica,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_psicologica,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_tortura,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_sexual,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_trafico,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_financeira,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_negligencia,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_trab_infan,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_interv_legal,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$viol_outros,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$agressao_forca,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$agressao_enforcamento,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$agressao_obj_cont,
                      dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$agressao_obj_corte,
                 dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$agressao_quente,
                       dados_mutate$regiao,useNA = "always"),2)*100,1) 
round(prop.table(table(dados_mutate$agressao_envenen,
                 dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$agressao_fogo,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$agressao_ameaca,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$agressao_outros,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$sex_assedio,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$sex_estupro,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$sex_pornog_inf,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$sex_exploracao,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$sex_outros,
                 dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_pai,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_mae,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_padrasto,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_madrasta,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_conjuge,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_exconj,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_namorado,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_exnamo,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_filho,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_irmao,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$relacao_outros_fam,
                      dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$sexo_autor,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$uso_alcool,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)  
round(prop.table(table(dados_mutate$enc_rede_atend_mulh,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_deleg_mulh
                       ,dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_rede_saude,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_assist_social,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_rede_educacao,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_cons_tutelar,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_cons_idoso,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_deleg_idoso,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_dir_humanos,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_mpu,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_deleg_crianca,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_outras_deleg,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_infan_juven,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$enc_defen_publica,
                       dados_mutate$regiao,useNA = "always"),2)*100,1)
round(prop.table(table(dados_mutate$motivacao,
                       dados_mutate$regiao,useNA = "always"),2)*100,2)




install.packages("gtsummary")
library(gtsummary)
install.packages("tidyverse")
library(tidyverse)



tabela1 <-
  dados_mutate %>%
  dplyr::select(idade, raca, escolaridade, sit_conj, gestante, viol_repet,
                viol_fisica, viol_psicologica, viol_tortura, viol_sexual,
                viol_trafico, viol_financeira, viol_negligencia, 
                viol_trab_infan, viol_interv_legal, viol_outros, agressao_forca,
                agressao_enforcamento, agressao_obj_cont, agressao_obj_corte,
                agressao_quente, agressao_envenen, agressao_fogo, agressao_ameaca,
                agressao_outros, sex_assedio, sex_estupro, sex_pornog_inf,
                sex_exploracao, sex_outros, relacao_pai, relacao_mae,
                relacao_padrasto, relacao_madrasta, relacao_conjuge, 
                relacao_exconj, relacao_namorado, relacao_exnamo, relacao_filho,
                relacao_irmao, relacao_outros_fam, sexo_autor, uso_alcool, 
                enc_rede_atend_mulh, enc_deleg_mulh, enc_rede_saude,
                enc_assist_social, enc_rede_educacao, enc_cons_tutelar,
                enc_cons_idoso, enc_deleg_idoso, enc_dir_humanos, enc_mpu,
                enc_deleg_crianca, enc_outras_deleg, enc_infan_juven,
                enc_defen_publica, motivacao, regiao)


#--------- código para tabela pronta ------------------------------

library("gt")

tabela1 %>% 
  dplyr::mutate(regiao=factor(regiao) %>% 
                  forcats::fct_explicit_na(), 
                idade=factor(idade) %>% fct_explicit_na(),
                raca=factor(raca) %>% fct_explicit_na(),
                escolaridade=factor(escolaridade) %>% fct_explicit_na(),
                sit_conj=factor(sit_conj) %>% fct_explicit_na(), 
                gestante=factor(gestante) %>% fct_explicit_na(),
                viol_repet=factor(viol_repet) %>% fct_explicit_na(), 
                viol_fisica=factor(viol_fisica) %>% fct_explicit_na(),
                viol_psicologica=factor(viol_psicologica) %>% fct_explicit_na(), 
                viol_tortura=factor(viol_tortura) %>% fct_explicit_na(),
                viol_sexual=factor(viol_sexual) %>% fct_explicit_na(),
                viol_trafico=factor(viol_trafico) %>% fct_explicit_na(),
                viol_financeira=factor(viol_financeira) %>% fct_explicit_na(),
                viol_negligencia=factor(viol_negligencia) %>% fct_explicit_na(), 
                viol_trab_infan=factor(viol_trab_infan) %>% fct_explicit_na(),
                viol_interv_legal=factor(viol_interv_legal) %>% fct_explicit_na(),
                viol_outros=factor(viol_outros) %>% fct_explicit_na(),
                agressao_forca=factor(agressao_forca) %>% fct_explicit_na(), 
                agressao_enforcamento=factor(agressao_enforcamento) %>% fct_explicit_na(),
                agressao_obj_cont=factor(agressao_obj_cont) %>% fct_explicit_na(),
                agressao_obj_corte=factor(agressao_obj_corte) %>% fct_explicit_na(),
                agressao_quente=factor(agressao_quente) %>% fct_explicit_na(), 
                agressao_envenen=factor(agressao_envenen) %>% fct_explicit_na(),
                agressao_fogo=factor(agressao_fogo) %>% fct_explicit_na(),
                agressao_ameaca=factor(agressao_ameaca) %>% fct_explicit_na(),
                agressao_outros=factor(agressao_outros) %>% fct_explicit_na(),
                sex_assedio=factor(sex_assedio) %>% fct_explicit_na(),
                sex_estupro=factor(sex_estupro) %>% fct_explicit_na(), 
                sex_pornog_inf=factor(sex_pornog_inf) %>% fct_explicit_na(),
                sex_exploracao=factor(sex_exploracao) %>% fct_explicit_na(),
                sex_outros=factor(sex_outros) %>% fct_explicit_na(),
                relacao_pai=factor(relacao_pai) %>% fct_explicit_na(),
                relacao_mae=factor(relacao_mae) %>% fct_explicit_na(),
                relacao_padrasto=factor(relacao_padrasto) %>% fct_explicit_na(), 
                relacao_madrasta=factor(relacao_madrasta) %>% fct_explicit_na(),
                relacao_conjuge=factor(relacao_conjuge) %>% fct_explicit_na(),
                relacao_exconj=factor(relacao_exconj) %>% fct_explicit_na(),
                relacao_namorado=factor(relacao_namorado) %>% fct_explicit_na(), 
                relacao_exnamo=factor(relacao_exnamo) %>% fct_explicit_na(),
                relacao_filho=factor(relacao_filho) %>% fct_explicit_na(),
                relacao_irmao=factor(relacao_irmao) %>% fct_explicit_na(),
                relacao_outros_fam=factor(relacao_outros_fam) %>% fct_explicit_na(),
                sexo_autor=factor(sexo_autor) %>% fct_explicit_na(),
                uso_alcool=factor(uso_alcool) %>% fct_explicit_na(),
                enc_rede_atend_mulh=factor(enc_rede_atend_mulh) %>% fct_explicit_na(),
                enc_deleg_mulh=factor(enc_deleg_mulh) %>% fct_explicit_na(),
                enc_rede_saude=factor(enc_rede_saude) %>% fct_explicit_na(),
                enc_assist_social=factor(enc_assist_social) %>% fct_explicit_na(),
                enc_rede_educacao=factor(enc_rede_educacao) %>% fct_explicit_na(),
                enc_cons_tutelar=factor(enc_cons_tutelar) %>% fct_explicit_na(),
                enc_cons_idoso=factor(enc_cons_idoso) %>% fct_explicit_na(),
                enc_deleg_idoso=factor(enc_deleg_idoso) %>% fct_explicit_na(), 
                enc_dir_humanos=factor(enc_dir_humanos) %>% fct_explicit_na(),
                enc_mpu=factor(enc_mpu) %>% fct_explicit_na(),
                enc_deleg_crianca=factor(enc_deleg_crianca) %>% fct_explicit_na(),
                enc_outras_deleg=factor(enc_outras_deleg) %>% fct_explicit_na(), 
                enc_infan_juven=factor(enc_infan_juven) %>% fct_explicit_na(),
                enc_defen_publica=factor(enc_defen_publica) %>% fct_explicit_na(),
                motivacao=factor(motivacao) %>% fct_explicit_na()
  ) %>%
  tbl_summary(by = regiao,
              digits = list(all_categorical() ~ 2),
              missing = "ifany",
              missing_text = "(não preenchido)",
              percent = "column") %>%
  add_n(statistic = "{n} / {N}") 

write.csv2(tabela1,"tabela1.csv")



