library(shiny)
library(shinydashboard)
library(shinyWidgets)

#pacotes do tidyverse
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)

#dados ajustado
dta.sen <- read.csv(
  '/home/matheus/Documentos/UNB/Laboratório 1 - Shiny/Projeto/Scripts/Ajuste dos dados.csv',
  header = T,sep = ',')

#perguntas do questionário
democracia <- c('Em geral, qual o seu nível de satisfação com a Democracia no Brasil?',
                'Como você avalia o trabalho do Senado para atender às necessidades da população?',
                'Na sua opinião, o quanto o Senado Federal e a Câmara dos Deputados são importantes para a fiscalização do governo federal?',
                'Como você avalia seu interesse por política?',
                'Com que frequência você acompanha notícias sobre o que está sendo debatido no Senado',
                'Na política se fala em esquerda, direita e centro. Você se considera mais de: ')

fatores.voto <- c("Você votou nas eleições municipais?",
                  'Na eleição para prefeito, o que foi mais importante para a decisão do seu voto?',
                  'E na eleição para vereador, o que foi mais importante para a decisão do seu voto?')

auxilios.econ <- c('Na sua opinião, programas de auxílio financeiro do governo para pessoas de baixa renda devem existir:',
                   'As pessoas procuram ajuda financeira do governo porque não querem trabalhar')

eleicao.voto <- c('Você confia no resultado das urnas eletrônicas em eleições',
                  'Algumas pessoas só votam em determinados partidos porque têm menos estudo',
                  'Em algumas situações, o voto de pessoas como você deveria valer mais do que o voto de outras pessoas')

politica <- c('Na hora de fazer leis, os políticos devem levar em conta o que diz a tradição das religiões')

seguranca <- c('Antes de ser punido, um criminoso deve ter o direito de se defender na justiça',
               'Facilitar a posse de armas vai aumentar a segurança no Brasil',
               'Deveria existir pena de morte no Brasil')

preconceito <- c('No Brasil, homossexuais sofrem muita discriminação')

raca <- c('O sistema de cotas para negros em universidade é justo')

mulher <- c('As mulheres devem ter o direito de interromper a gravidez com segurança, caso elas queiram')

meio.ambiente <- c('De forma geral, a natureza e o meio-ambiente são bem protegidos no Brasil',
                   'O aquecimento global é preocupante')

maconha <- c('A maconha para uso medicinal deve ser legalizada',
             'A maconha para uso recreativo de ser legalizada')


#categoria das variáveis
sexo <- c("Feminino", "Masculino")
faixaet<- c("De 16 a 29 anos", "De 30 a 39 anos", "De 40 a 49 anos", "De 50 a 59 anos", "60 anos ou mais")
ocupacao<- c("Ocupado", "Desocupado", "Fora da força de trabalho", "Não sei ou prefiro não responder")
escolaridade <- c("Até ensino fundamental incompleto", "Ensino fundamental completo",
                  "Ensino médio completo", "Ensino superior incompleto ou mais")

regiao <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
cor <- c("Branca", "Preta", "Parda", "Amarela/Indígena")
religiao <- c("Católica", "Evangélica", "Espírita", "Outra/Sem religião", "Prefiro não responder")
renda <- c("Até 2", "De 2 a 5", "Mais de 5", "Não sei/Prefiro não responder")




#interface
ui <- dashboardPage(
  dashboardHeader(title = "DataSenado"),
  
  dashboardSidebar(
    width = 285,
    sidebarMenu(id = "menu",
                menuItem("Perfil dos Respondentes", tabName = "op-1" ,icon = icon("table")
                ),
                menuItem("Democracia e Avaliação do Congresso",tabName = "op-2" ,icon = icon("table")
                ),
                menuItem("Fatores Decisivos para voto municipal",tabName = "op-3", icon = icon("table")
                ),
                menuItem("Perfil Psicométrico", icon = icon("table"),
                         menuSubItem("Auxílios Econômicos", tabName = "op-4a",icon = icon("angle-right")),
                         menuSubItem("Eleições e voto", tabName = "op-4b",icon = icon("angle-right")),
                         menuSubItem("Política", tabName = "op-4c", icon = icon("angle-right")),
                         menuSubItem("Segurança Pública", tabName = "op-4d", icon = icon("angle-right")),
                         menuSubItem("Preconceito", tabName = "op-4e", icon = icon("angle-right")),
                         menuSubItem("Raça", tabName = "op-4f", icon = icon("angle-right")),
                         menuSubItem("Mulher", tabName = "op-4g", icon = icon("angle-right")),
                         menuSubItem("Meio Ambiente", tabName = "op-4h", icon = icon("angle-right")),
                         menuSubItem("Legalização da Maconha", tabName = "op-4i", icon = icon("angle-right"))
                ),
                conditionalPanel(condition = 'input.menu != "op-1" && input.menu2 != "about" ',
                                 selectInput('filtros1','Escolha um filtro',
                                             choices = c('Nenhum filtro selecionado','Sexo','Faixa Etária','Ocupação',
                                                         'Escolaridade','Região','Cor/Raça',
                                                         'Religião/Crença','Renda em Salários Mínimos'),
                                             selected = 'Nenhum filtro selecionado')),
                
                conditionalPanel(condition = 'input.menu != "op-1" && input.menu2 != "about" && input.filtros1 != "Nenhum filtro selecionado"',
                                 selectInput('filtros2','Escolha uma outro filtro',
                                             choices = c('Nenhum filtro selecionado','Sexo','Faixa Etária','Ocupação',
                                                         'Escolaridade','Região','Cor/Raça',
                                                         'Religião/Crença','Renda em Salários Mínimos'),
                                             selected = 'Nenhum filtro selecionado'))
    ),
    hr(),
    sidebarMenu(id = 'menu2', #POSSIVELMENT NÃO É O MAIS ADEQUADO PARA ESCREVER UM TEXTO.
                menuItem('Sobre',tabName = 'about',icon = icon('exclamation-circle'),
                         menuSubItem(p('ttttttttttttttttttttttttttttttttttttttttttttttttt',br(),
                                       'ttttttttttttttttttttttttttttttttttttttttttttttttt',br(),
                                       'teste',br(),
                                       'ttttttttttttttttttttttttttttttttttttttttttttttttt')))
    )
  ),
  #Separando cada Tema em uma "página" diferente:
  dashboardBody(
    tabItems(
      tabItem(tabName = "op-1",#para o perfil dos respondendes
              selectInput("perfil1","Perfil dos Respondentes de acordo com a característica selecionada abaixo",
                          choices = c('Sexo','Faixa Etária','Ocupação',
                                      'Escolaridade','Região','Cor/Raça',
                                      'Religião/Crença','Renda em Salários Mínimos')),
              
              fluidRow(column(width = 6,box(width = NULL,plotOutput('Graf1')))),
              fluidRow(column(width = 6,box(width = NULL,tableOutput('Tabela1'))))
              
      ),
      tabItem(tabName="op-2",selectInput("pergunta2","Questionamentos em relação à Democracia e a Avaliação do Congresso",
                                         choices=c(democracia)),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico2.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico2.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico2.3'))
              )),
              fluidRow(box(tableOutput("Tabela2")))
      ),
      tabItem(tabName="op-3",selectInput("pergunta3","Questionamentos em relação aos fatores decisivos para o voto municipal",
                                         choices = fatores.voto),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico3.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico3.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico3.3'))
              )),
              fluidRow(box(tableOutput("Tabela3")))
      ),
      tabItem(tabName="op-4a",selectInput("pergunta4A","Questionamentos em relação a auxílios econômicos",
                                          choices=c(auxilios.econ)),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4A.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4A.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4A.3'))
              )),
              fluidRow(box(tableOutput("Tabela4A")))
      ),
      tabItem(tabName="op-4b",selectInput("pergunta4B","Questionamentos em relação à eleições e voto",choices = eleicao.voto),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4B.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4B.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4B.3'))
              )),
              fluidRow(box(tableOutput("Tabela4B")))
      ),
      tabItem(tabName="op-4c",selectInput("pergunta4C","Questionamentos em relação à política",choices = politica),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4C.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4C.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4C.3'))
              )),
              fluidRow(box(tableOutput("Tabela4C")))
      ),
      tabItem(tabName="op-4d",selectInput("pergunta4D","Questionamentos em relação à segurança pública",choices= seguranca),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4D.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4D.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4D.3'))
              )),
              fluidRow(box(tableOutput("Tabela4D")))
      ),
      tabItem(tabName="op-4e",selectInput("pergunta4E","Questionamentos em relação a preconteito",choices = preconceito),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4E.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4E.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4E.3'))
              )),
              fluidRow(box(tableOutput("Tabela4E")))
      ),
      tabItem(tabName="op-4f",selectInput("pergunta4F","Questionamentos em relação à raça",choices = raca),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4F.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4F.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4F.3'))
              )),
              fluidRow(box(tableOutput("Tabela4F")))
      ),
      tabItem(tabName="op-4g",selectInput("pergunta4G","Questionamentos em relação à mulher",choices = mulher),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4G.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4G.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4G.3'))
              )),
              fluidRow(box(tableOutput("Tabela4G")))
      ),
      tabItem(tabName="op-4h",selectInput("pergunta4H","Questionamentos em relação ao meio ambiente",choices= meio.ambiente),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4H.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4H.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4H.3'))
              )),
              fluidRow(box(tableOutput("Tabela4H")))
      ),
      tabItem(tabName="op-4i",selectInput("pergunta4I","Questionamentos em relação à legalização da maconha",choices = maconha),
              conditionalPanel(condition = 'input.filtros2 == "Sexo" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.sexo','Selecione uma categoria',choices = c(sexo),selected = sexo[1])),
              conditionalPanel(condition = 'input.filtros2 == "Faixa Etária" && input.filtros1 != "Nenhum filtro selecionado" ',selectInput('cat.idade','Selecione uma categoria',choices = c(faixaet),selected = faixaet[1])),
              conditionalPanel(condition = 'input.filtros2 == "Ocupação" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.ocup','Selecione uma categoria',choices = c(ocupacao),selected = ocupacao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Escolaridade" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.escolaridade','Selecione uma categoria',choices = c(escolaridade),selected = escolaridade[1])),
              conditionalPanel(condition = 'input.filtros2 == "Região" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.regiao','Selecione uma categoria',choices = c(regiao),selected = regiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Cor/Raça" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.cor','Selecione uma categoria',choices = c(cor),selected = cor[1])),
              conditionalPanel(condition = 'input.filtros2 == "Religião/Crença" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.religiao','Selecione uma categoria',choices = c(religiao),selected = religiao[1])),
              conditionalPanel(condition = 'input.filtros2 == "Renda em Salários Mínimos" && input.filtros1 != "Nenhum filtro selecionado"',selectInput('cat.renda','Selecione uma categoria',choices = c(renda),selected = renda[1])),
              
              fluidRow(tabsetPanel(type = 'tabs',
                                   tabPanel(title = icon('chart-pie'),plotOutput('Grafico4I.1')),
                                   tabPanel(title = icon('chart-bar fa-rotate-90'),plotOutput('Grafico4I.2')),
                                   tabPanel(title = icon('bars'),plotOutput('Grafico4I.3'))
              )),
              fluidRow(box(tableOutput("Tabela4I")))
              
      )
    )  
  )
)


server <- function(input, output) {
  
  df.senado <- reactive({
    dta.sen
  })
  
  ## Gráficos e Tabelas (servidor)
  ##########################################################################################################################################3
  #Gráfico Tema 1 (Perfil dos Respondentes)
  output$Graf1 <- renderPlot({
    if(input$perfil1 == 'Sexo'){
      pie(table(df.senado()$V05))
    }else if(input$perfil1 == 'Faixa Etária'){
      barplot(table(df.senado()$V05))
    } else if(input$perfil1 == 'Ocupação'){
      barplot(table(df.senado()$V10_A))
    } else if(input$perfil1 == 'Escolaridade'){
      barplot(table(df.senado()$V08))
    } else if(input$perfil1 == 'Região'){
      barplot(table(df.senado()$P01_A))
    } else if(input$perfil1 == 'Cor/Raça'){
      barplot(table(df.senado()$V07))
    }else if(input$perfil1 == 'Religião/Crença'){
      barplot(table(df.senado()$V11))
    } else if (input$perfil1 =='Renda em Salários Mínimos'){
      barplot(table(df.senado()$V12))
    }
  })
  
  output$Tabela1 <- renderTable({
    if(input$perfil1 == 'Sexo'){
      table(df.senado()$V06)
    }else if(input$perfil1 == 'Faixa Etária'){
      table(df.senado()$V05)
    } else if(input$perfil1 == 'Ocupação'){
      table(df.senado()$V10_A)
    } else if(input$perfil1 == 'Escolaridade'){
      table(df.senado()$V08)
    } else if(input$perfil1 == 'Região'){
      table(df.senado()$P01_A)
    } else if(input$perfil1 == 'Cor/Raça'){
      table(df.senado()$V07)
    }else if(input$perfil1 == 'Religião/Crença'){
      table(df.senado()$V11)
    } else if (input$perfil1 =='Renda em Salários Mínimos'){
      table(df.senado()$V12)
    }
  })
  
  #################################################################################################################
  #Gráfico Tema 2 (Democracia e Avaliação do Congresso)
  output$Grafico2.1 <- renderPlot({
    ###PERGUNTA 1 DE DEMOCRACIA
    if(input$pergunta2 == democracia[1]){ 
      if(input$filtros1 == 'Nenhum filtro selecionado'){ #caso geral, tds os respondentes!
        pie(table(df.senado()$V05))
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Nenhum filtro selecionado'){#somente sexo, sem cruzamento de variáveis!!
        pie(table(df.senado()$V06))
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Sexo'){
        if(input$cat.sexo == sexo[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.sexo == sexo[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Faixa Etária'){
        if(input$cat.idade == faixaet[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.idade == faixaet[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.idade == faixaet[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.idade == faixaet[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.idade == faixaet[5]){
          pie(table(df.senado()$V06),col = 'red')
        }
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Ocupação'){
        if(input$cat.ocup == ocupacao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.ocup == ocupacao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.ocup == ocupacao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.ocup == ocupacao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Escolaridade'){
        if(input$cat.escolaridade == escolaridade[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.escolaridade == escolaridade[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.escolaridade == escolaridade[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.escolaridade == escolaridade[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Região'){
        if(input$cat.regiao == regiao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.regiao == regiao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.regiao == regiao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.regiao == regiao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.regiao == regiao[5]){
          pie(table(df.senado()$V06),col = 'red')
        }
        
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Cor/Raça'){
        if(input$cat.cor == cor[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.cor == cor[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.cor == cor[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.cor == cor[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Religião/Crença'){
        if(input$cat.religiao == religiao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.religiao == religiao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.religiao == religiao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.religiao == religiao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.religiao == religiao[5]){
          pie(table(df.senado()$V06),col = 'purple')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Renda em Salários Mínimos'){
        if(input$cat.renda == renda[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.renda == renda[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.renda == renda[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.renda == renda[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
      }
    }
  })
  
  
  output$Grafico2.2 <- renderPlot({
    if(input$pergunta2 == democracia[1]){ 
      if(input$filtros1 == 'Nenhum filtro selecionado'){ #caso geral, tds os respondentes!
        pie(table(df.senado()$V05))
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Nenhum filtro selecionado'){#somente sexo, sem cruzamento de variáveis!!
        pie(table(df.senado()$V06))
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Sexo'){
        if(input$cat.sexo == sexo[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.sexo == sexo[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Faixa Etária'){
        if(input$cat.idade == faixaet[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.idade == faixaet[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.idade == faixaet[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.idade == faixaet[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.idade == faixaet[5]){
          pie(table(df.senado()$V06),col = 'red')
        }
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Ocupação'){
        if(input$cat.ocup == ocupacao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.ocup == ocupacao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.ocup == ocupacao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.ocup == ocupacao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Escolaridade'){
        if(input$cat.escolaridade == escolaridade[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.escolaridade == escolaridade[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.escolaridade == escolaridade[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.escolaridade == escolaridade[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Região'){
        if(input$cat.regiao == regiao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.regiao == regiao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.regiao == regiao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.regiao == regiao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.regiao == regiao[5]){
          pie(table(df.senado()$V06),col = 'red')
        }
        
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Cor/Raça'){
        if(input$cat.cor == cor[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.cor == cor[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.cor == cor[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.cor == cor[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Religião/Crença'){
        if(input$cat.religiao == religiao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.religiao == religiao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.religiao == religiao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.religiao == religiao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.religiao == religiao[5]){
          pie(table(df.senado()$V06),col = 'purple')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Renda em Salários Mínimos'){
        if(input$cat.renda == renda[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.renda == renda[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.renda == renda[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.renda == renda[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
      }
    }
  })
  
  output$Grafico2.3 <- renderPlot({
    if(input$pergunta2 == democracia[1]){ 
      if(input$filtros1 == 'Nenhum filtro selecionado'){ #caso geral, tds os respondentes!
        pie(table(df.senado()$V05))
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Nenhum filtro selecionado'){#somente sexo, sem cruzamento de variáveis!!
        pie(table(df.senado()$V06))
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Sexo'){
        if(input$cat.sexo == sexo[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.sexo == sexo[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Faixa Etária'){
        if(input$cat.idade == faixaet[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.idade == faixaet[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.idade == faixaet[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.idade == faixaet[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.idade == faixaet[5]){
          pie(table(df.senado()$V06),col = 'red')
        }
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Ocupação'){
        if(input$cat.ocup == ocupacao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.ocup == ocupacao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.ocup == ocupacao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.ocup == ocupacao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Escolaridade'){
        if(input$cat.escolaridade == escolaridade[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.escolaridade == escolaridade[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.escolaridade == escolaridade[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.escolaridade == escolaridade[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Região'){
        if(input$cat.regiao == regiao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.regiao == regiao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.regiao == regiao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.regiao == regiao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.regiao == regiao[5]){
          pie(table(df.senado()$V06),col = 'red')
        }
        
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Cor/Raça'){
        if(input$cat.cor == cor[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.cor == cor[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.cor == cor[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.cor == cor[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
        
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Religião/Crença'){
        if(input$cat.religiao == religiao[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.religiao == religiao[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.religiao == religiao[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.religiao == religiao[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }else if(input$cat.religiao == religiao[5]){
          pie(table(df.senado()$V06),col = 'purple')
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Renda em Salários Mínimos'){
        if(input$cat.renda == renda[1]){
          pie(table(df.senado()$V06),col = 'pink')
        }else if(input$cat.renda == renda[2]){
          pie(table(df.senado()$V06),col = 'yellow')
        }else if(input$cat.renda == renda[3]){
          pie(table(df.senado()$V06),col = 'green')
        }else if(input$cat.renda == renda[4]){
          pie(table(df.senado()$V06),col = 'orange')
        }
      }
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  output$Tabela2 <- renderTable({
    if(input$pergunta2 == democracia[1]){ 
      if(input$filtros1 == 'Nenhum filtro selecionado'){
        table(df.senado()$V08)
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Nenhum filtro selecionado'){
        table(df.senado()$V06)
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Sexo'){
        if(input$cat.sexo == sexo[1]){
          table(df.senado()$V07)
        }else if(input$cat.sexo == sexo[2]){
          table(df.senado()$V05)
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Faixa Etária'){
        if(input$cat.idade == faixaet[1]){
          table(df.senado()$V06)
        }else if(input$cat.idade == faixaet[2]){
          table(df.senado()$V05)
        }else if(input$cat.idade == faixaet[3]){
          table(df.senado()$V07)
        }else if(input$cat.idade == faixaet[4]){
          table(df.senado()$V08)
        }else if(input$cat.idade == faixaet[5]){
          table(df.senado()$V06)
        }
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Ocupação'){
        if(input$cat.ocup == ocupacao[1]){
          table(df.senado()$V06)
        }else if(input$cat.ocup == ocupacao[2]){
          table(df.senado()$V05)
        }else if(input$cat.ocup == ocupacao[3]){
          table(df.senado()$V09)
        }else if(input$cat.ocup == ocupacao[4]){
          table(df.senado()$V08)
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Escolaridade'){
        if(input$cat.escolaridade == escolaridade[1]){
          table(df.senado()$V06)
        }else if(input$cat.escolaridade == escolaridade[2]){
          table(df.senado()$V05)
        }else if(input$cat.escolaridade == escolaridade[3]){
          table(df.senado()$V08)
        }else if(input$cat.escolaridade == escolaridade[4]){
          table(df.senado()$V07)
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Região'){
        if(input$cat.regiao == regiao[1]){
          table(df.senado()$V05)
        }else if(input$cat.regiao == regiao[2]){
          table(df.senado()$V07)
        }else if(input$cat.regiao == regiao[3]){
          table(df.senado()$V06)
        }else if(input$cat.regiao == regiao[4]){
          table(df.senado()$V05)
        }else if(input$cat.regiao == regiao[5]){
          table(df.senado()$V06)
        }
        
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Cor/Raça'){
        if(input$cat.cor == cor[1]){
          table(df.senado()$V06)
        }else if(input$cat.cor == cor[2]){
          table(df.senado()$V05)
        }else if(input$cat.cor == cor[3]){
          table(df.senado()$V08)
        }else if(input$cat.cor == cor[4]){
          table(df.senado()$V06)
        }
        
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Religião/Crença'){
        if(input$cat.religiao == religiao[1]){
          table(df.senado()$V06)
        }else if(input$cat.religiao == religiao[2]){
          table(df.senado()$V05)
        }else if(input$cat.religiao == religiao[3]){
          table(df.senado()$V08)
        }else if(input$cat.religiao == religiao[4]){
          table(df.senado()$V05)
        }else if(input$cat.religiao == religiao[5]){
          table(df.senado()$V06)
        }
        
        
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Renda em Salários Mínimos'){
        if(input$cat.renda == renda[1]){
          table(df.senado()$V06)
        }else if(input$cat.renda == renda[2]){
          table(df.senado()$V05)
        }else if(input$cat.renda == renda[3]){
          table(df.senado()$V07)
        }else if(input$cat.renda == renda[4]){
          table(df.senado()$V06)
        }
      }
    }
  })
  
  
  
  ######################################################################################################################################################3  
  #Gráfico Tema 3 (Fatores Decisivos para voto municipal)
  output$Graf3 <- renderPlot({
    df
  })
  
  #Gráfico Tema 4 (Perfil Psicométrico)
  output$Graf4A <- renderPlot({
    df
  })
}

shinyApp(ui, server)




