#####Analise de dados#####
#####Aluna Tatiane Albuquerque#####

library(tidyverse)
library(GGally)
library(ffbase)
library(readr)
library(readxl)
library(rlang) 

####Pergunta 1####

#Link do repositorio: "https://github.com/tatialb/Lista_4_Tatiane_Albuquerque/blob/master/Lista_4_Tatiane_Albuquerque.R"

####Pergunta 2####

#Realização da análise de correlação entre a quantidade de alunos por docente e o IDH de cada municipio - Dados do PNUD Censo Escolar de 2016#
#Baixando base de dados#

setwd("C:/Users/tatia/Desktop/Mestrado UFPE/ANALISE DE DADOS/Listas")
load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")

#Transformação da base de dados#
pnud <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet = 2)

#Reconhecendo a base de dados:
head(pnud)
unique(pnud$ANO)

#Seleção dos dados referentes ao Estade de PE#
pnud_pe_2010 <- pnud %>% filter(ANO == 2010 & UF == 26)


#Fase de processamento#

#Requerindo base de turmas#
require(tidyverse)

turmas_pe_sel <- turmas_pe %>% group_by(CO_MUNICIPIO) %>% summarise(n_turmas = n(), 
                                                                    turmas_disc_prof = sum(IN_DISC_PROFISSIONALIZANTE, na.rm = T), turmas_disc_inf = sum(IN_DISC_INFORMATICA_COMPUTACAO, na.rm = T), turmas_disc_mat = sum(IN_DISC_MATEMATICA, na.rm = T), turmas_disc_pt = sum(IN_DISC_LINGUA_PORTUGUESA, na.rm = T), turmas_disc_en = sum(IN_DISC_LINGUA_INGLES, na.rm = T))


dim(turmas_pe_sel)[1] == length(unique(turmas_pe$CO_MUNICIPIO))

summary(turmas_pe_sel)

#Base de dados de Escolas

escolas_pe_sel <- escolas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_escolas = n(), 
            n_escolas_priv = sum(TP_DEPENDENCIA == 4, na.rm = T),
            escolas_func = sum(TP_SITUACAO_FUNCIONAMENTO == 1, na.rm = T),
            escolas_agua_inex = sum(IN_AGUA_INEXISTENTE, na.rm = T),
            escolas_energia_inex = sum(IN_ENERGIA_INEXISTENTE, na.rm = T),
            escolas_esgoto_inex = sum(IN_ESGOTO_INEXISTENTE, na.rm = T),
            escolas_internet = sum(IN_INTERNET, na.rm = T),
            escolas_alimentacao = sum(IN_ALIMENTACAO, na.rm = T))

#Verificando#

dim(escolas_pe_sel)[1] == length(unique(escolas_pe$CO_MUNICIPIO))

#Resumo geral#
summary(escolas_pe_sel)

library(tidyverse)

#Parte dos Docentes#

install.packages(summ)
docentes_pe_sel <- docentes_pe %>% group_by(CO_MUNICIPIO) %>% summarise(n_docentes = n(), docentes_media_idade = mean(NU_IDADE), docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T), docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T), docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T))

summary(docentes_pe_sel)

#Parte das Matriculas#

matriculas_pe_sel <- matricula_pe %>% group_by(CO_MUNICIPIO) %>% summarise(n_matriculas = n(), alunos_media_idade = mean(NU_IDADE), alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T), alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T), alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T), alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T), matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T), matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T), matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T))


#Verificando todo o procedimento realizado#

dim(matriculas_pe_sel)[1] == length(unique(matricula_pe$CO_MUNICIPIO))

summary(matriculas_pe_sel)

#Parte referente a união da base de dados do censo e PNUD#  

#Matriculas#

censo_pnud_pe_sel <- pnud_pe_2010 %>% full_join(matriculas_pe_sel, 
                                                by = c("Codmun7" = "CO_MUNICIPIO"))
dim(pnud_pe_2010)

dim(matriculas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

#Escolas#

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(escolas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))

dim(escolas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

#Turmas#

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(turmas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))

dim(turmas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

#Docentes#
censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(docentes_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))


dim(docentes_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

#Salvar a nova base de dados#

getwd()

dir()

save(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.RData")

write.csv2(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.csv",
           row.names = F)
  
#Limpar area de trabalho

rm(list = ls())

#Abrindo o novo banco de dados#

getwd()

#Abrir nova base de dados#

load("2016_censo_pnud_pe_sel.RData")

#Verificação de algumas caracteristicas da base de dados#

dim(censo_pnud_pe_sel)

summary(censo_pnud_pe_sel)

head(censo_pnud_pe_sel)

#Nao deve haver docente com mais de 70 anos ou com menos de 18 anos:

summary(censo_pnud_pe_sel)

summary(censo_pnud_pe_sel$alunos_media_idade)

setwd("C:/Users/tatia/Desktop/Mestrado UFPE/ANALISE DE DADOS/Listas")

load("docentes_pe_censo_escolar_2016.RData")

docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70)

dim(docentes_pe_selecao)


#Nao deve haver aluno com mais de 25 anos ou com menos de 1 ano#

load("matricula_pe_censo_escolar_2016.RData")

matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE > 1, NU_IDADE < 25)

dim(matricula_pe_selecao)

summary(matricula_pe_selecao$NU_IDADE)


#Apresentação de estatasticas descritivas do numero de alunos por docente por municipio do Estado de PE#

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

DocentesAlunos <- censo_pnud_pe_sel$n_matriculas/censo_pnud_pe_sel$n_docentes


#Estatistica Descritiva de Docentes por alunos:

DocentesAlunos

summary(DocentesAlunos)

plot(DocentesAlunos)

ggplot(censo_pnud_pe_sel, aes(DocentesAlunos))+geom_histogram()


#Apresentação do municipio com maior numero de alunos por docente e seu IDH-M#

names(censo_pnud_pe_sel)

summary(DocentesAlunos)

DocentesAlunos

#União das variaveis#

censo_pnud_pe_sel_docentesalunos <- censo_pnud_pe_sel %>%  mutate(DocentesAlunos)

View(censo_pnud_pe_sel_docentesalunos)

censo_pnud_pe_sel_docentesalunos["177", ]

#>>>A cidade com maior numero de alunos por docente e Tupanatinga#


#Teste do coeficiente de correlacao linear de pearson#

cor(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

cor.test(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

#Segundo apresentado pelos dados, a correlacao e de "-0,47", sendo assim negativa, ou seja, nao existe correlacao entre o numero de alunos e docentes e o IDH dos municipios# 


####Questao 3####

#Grafico de dispersao no ggplot#

ggplot(censo_pnud_pe_sel_docentesalunos, aes(DocentesAlunos, IDHM, color = IDHM))+geom_point()



