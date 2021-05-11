
### Ajuste dos Dados ###
dados_senado$I02 <- factor(dados_senado$I02, label = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo", "Goiás",
                                                       "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco",
                                                       "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina",
                                                       "São Paulo", "Sergipe", "Tocantins"), levels = c(12,27,13,16,29,23,53,32,52,21,51,50,31,15,25,41,26,22,33,24,43,11,
                                                                                                        14,42,35,28,17))
dados_senado$P01 <- factor(dados_senado$P01, label = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo", "Goiás",
                                                       "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco",
                                                       "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina",
                                                       "São Paulo", "Sergipe", "Tocantins"), levels = c(12,27,13,16,29,23,53,32,52,21,51,50,31,15,25,41,26,22,33,24,43,11,
                                                                                                        14,42,35,28,17))
dados_senado$P01_A <- factor(dados_senado$P01_A, label = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
                             levels = c(1:5))
dados_senado$P02 <- factor(dados_senado$P02, label = c("Melhorou", "Permaneceu igual", "Piorou", "Não sei ou prefiro não responder"),
                           levels = c(1:3, 97))
dados_senado$P03 <- factor(dados_senado$P03, label = c("Emprego", "Educação", "Custo de vida", "Corrupção", "Saúde", "Segurança Pública",
                                                       "Outro", "Não sei ou prefiro não responder"), 
                           levels = c(1:7, 97))
dados_senado$P04 <- factor(dados_senado$P04, label = c("Alto", "Médio", "Baixo", "Nenhum", "Não sei ou prefiro não responder"),
                           levels = c(1:4, 97))
dados_senado$P05 <- factor(dados_senado$P05, label = c("TV", "Páginas de notícias na Internet", "Jornais e revistas Impressos", "Facebook",
                                                       "Whatsapp", "YouTube","Outros meios. Quais?", "Não sei ou prefiro não responder"),
                           levels = c(1:7, 97))
dados_senado$P06 <- factor(dados_senado$P06, label = c("Com muita frequencia", "Com pouca frequencia", "Nunca", "Não sei ou prefiro não responder"),
                           levels = c(1:3, 97))
dados_senado$P07_1 <- factor(dados_senado$P07_1, label = c("Sim", "Não", "Não sei ou prefiro não responder"),
                             levels = c(1, 2, 97))
dados_senado$P07_2 <- factor(dados_senado$P07_2, label = c("Sim", "Não", "Não se aplica ou prefiro não responder"),
                             levels = c(1, 2, 97))
dados_senado$P08 <- factor(dados_senado$P08, label = c("Ótimo", "Bom", "Regular", "Ruim", "Péssimo", "Não sei ou prefiro não responder"),
                           levels = c(1:5, 97))
dados_senado$P09 <- factor(dados_senado$P09, label = c("Muito importantes", "Pouco importantes", "Nada importantes", "Não sei ou prefiro não responder"),
                           levels = c(1:3, 97))
dados_senado$P10 <- factor(dados_senado$P10, label = c("Muito importantes", "Pouco importantes", "Nada importantes", "Não sei ou prefiro não responder"),
                           levels = c(1:3, 97))
dados_senado$P11 <- factor(dados_senado$P11, label = c("A democracia é sempre a melhor forma de governo", "Em algumas situações, um governo autoritário é melhor",
                                                       "Para você tanto faz ter um governo democrático ou governo autoritário",
                                                       "Não sei ou prefiro não responder"), levels = c(1:3, 97))
dados_senado$P12 <- factor(dados_senado$P12, label = c("Muito satisfeito(a)", "Pouco satisfeito(a)", "Nada satisfeito(a)", "Não sei ou prefiro não responder"),
                           levels = c(1:3, 97))
dados_senado$P13 <- factor(dados_senado$P13, label = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo", "Goiás",
                                                       "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco",
                                                       "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina",
                                                       "São Paulo", "Sergipe", "Tocantins"), levels = c(12,27,13,16,29,23,53,32,52,21,51,50,31,15,25,41,26,22,33,24,43,11,
                                                                                                        14,42,35,28,17))
dados_senado$P14 <- factor(dados_senado$P14, label = c("Sim", "Não", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P15 <- factor(dados_senado$P15, label = c("Combater a corrupção", "Renovar a política", "Cuidar bem da cidade", "Ter experiência política",
                                                       "Ter pensamento parecido com o seu", "Combater a pandemia", " Outro", "Não sei ou prefiro não responder"), 
                           levels = c(1:7, 97))
dados_senado$P15_A <- factor(dados_senado$P15_A, label = c("Combater a corrupção", "Renovar a política", "Cuidar bem da cidade", "Ter experiência política",
                                                           "Ter pensamento parecido com o seu", "Combater a pandemia", " Outro", "Não sei ou prefiro não responder",
                                                           "Prefiro não responder"), levels = c(1:7, 97, 99))

dados_senado$P16_1 <- factor(dados_senado$P16_1, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P16_2 <- factor(dados_senado$P16_2, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P16_3 <- factor(dados_senado$P16_3, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P16_4 <- factor(dados_senado$P16_4, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P17_1 <- factor(dados_senado$P17_1, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P17_2 <- factor(dados_senado$P17_2, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P17_3 <- factor(dados_senado$P17_3, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P17_4 <- factor(dados_senado$P17_4, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P17_5 <- factor(dados_senado$P17_5, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P18_1 <- factor(dados_senado$P18_1, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P18_2 <- factor(dados_senado$P18_2, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P18_3 <- factor(dados_senado$P18_3, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P18_4 <- factor(dados_senado$P18_4, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P18_5 <- factor(dados_senado$P18_5, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P18_6 <- factor(dados_senado$P18_6, label = c("Concorda", "Discorda", "Não sei ou prefiro não responder"), levels = c(1, 2, 97))

dados_senado$P19 <- factor(dados_senado$P19, label = c("Muito", "Pouco", "Nada", "Não sei ou prefiro não responder"), levels = c(1:3, 97))

dados_senado$P20 <- factor(dados_senado$P20, label = c("Sempre", "Apenas em situações de crise", "Nunca", "Não sei ou prefiro não responder"),
                           levels = c(1:3, 97))
dados_senado$P21 <- factor(dados_senado$P21, label = c("Esquerda", "Direita", "Centro", "Não sei ou prefiro não responder"),
                           levels = c(1:3, 97))
dados_senado$V02 <- factor(dados_senado$V02, label = c("Sim", "Não"), levels = c(1, 2))

dados_senado$V03 <- factor(dados_senado$V03, label = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10 ou mais",
                                                       "Não sei ou prefiro não responder"), levels = c(1:10, 97))
dados_senado$V05 <- factor(dados_senado$V05, label = c("Feminino", "Masculino"), levels = c(1, 2))

dados_senado$V06 <- factor(dados_senado$V06, label = c("De 16 a 29 anos", "De 30 a 39 anos", "De 40 a 49 anos", "De 50 a 59 anos", "60 anos ou mais"),
                           levels = c(1:5))
dados_senado$V07 <- factor(dados_senado$V07, label = c("Branca", "Preta", "Amarela/Indígena", "Parda"), levels = c(1:4))

dados_senado$V08 <- factor(dados_senado$V08, label = c("Até ensino fundamental incompleto", "Ensino fundamental completo",
                                                       "Ensino médio completo", "Ensino superior incompleto ou mais"), levels = c(1:4))
dados_senado$V09 <- factor(dados_senado$V09, label = c("Sim", "Não", "Prefiro não responder"), levels = c(1, 2, 99))

dados_senado$V10 <- factor(dados_senado$V10, label = c("Ocupado", "Desocupado", "Fora da força de trabalho", "Não sei ou prefiro não responder"),
                           levels = c(1:3, 97))
dados_senado$V10_A <- factor(dados_senado$V10_A, label = c("Assalariado no setor privado", "Assalariado no setor público",
                                                           "Trabalhador doméstico", "Empresário", "Trabalhador autônomo",
                                                           "Estagiário", "Outra", "Prefiro não responder"), levels = c(1:7, 99))
dados_senado$V11 <- factor(dados_senado$V11, label = c("Católica", "Evangélica", "Espírita", "Outra/Sem religião", "Prefiro não respodner"),
                           levels = c(1:3, 96, 97))

dados_senado$V12 <- factor(dados_senado$V12, label = c("Até 2", "De 2 a 5", "Mais de 5", "Não sei/Prefiro não responder"),
                           levels = c(1, 3, 5, 97))

dados_senado$V13 <- factor(dados_senado$V13, label = c("Responsável pelo domicílio", "Outra condição no domicílio", "Prefiro não responder "),
                           levels = c(1, 2, 99))
dados_senado$V13_A <- factor(dados_senado$V13_A, label = c("Cônjuge ", "Filho(a) ou enteado(a)", "Genro ou nora ", "Pai, mãe, padrasto ou madrasta",
                                                           "Sogro(a)", "Neto(a)", "Bisneto(a)", "Irmão/Irmã", "Avô ou avó", "Outro parente",
                                                           "Não é parente (Agregado)", "Empregado doméstico que mora no domicilio",
                                                           "Parente do empregado doméstico que mora no domicílio", "Prefiro não responder"),
                             levels = c(1:13, 99))

dados_senado$porte.wgts <- factor(dados_senado$porte.wgts, label = c("Pequeno", "Médio", "Grande"), levels = c(1:3))



