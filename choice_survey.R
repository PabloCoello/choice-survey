library(support.CEs)


design = Lma.design(
          nalternatives = 3,
          attribute.names = list(vidaHumana = c('si', 'no'),
                                 medioambiental = c('med1', 'med2', 'med3', 'med4', 'med5'),
                                 social = c('soc1', 'soc2', 'soc3', 'soc4', 'soc5'),
                                 economico = c('econ1', 'econ2', 'econ3', 'econ4', 'econ5')
          ),
          nblocks = 1
)
summary(design)
design
questionnaire(design)
