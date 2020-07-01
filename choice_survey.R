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
design
questionnaire(design, common = list(vidaHumana = 'si',
                                    medioambiental = 'med1',
                                    social = 'soc1',
                                    economico = 'econ4'
))

rotation <- rotation.design(
  nalternatives = 3,
  attribute.names = list(vidaHumana = c('si', 'no'),
                         medioambiental = c('med1', 'med2', 'med3', 'med4', 'med5'),
                         social = c('soc1', 'soc2', 'soc3', 'soc4', 'soc5'),
                         economico = c('econ1', 'econ2', 'econ3', 'econ4', 'econ5')
  ),
  nblocks = 1
)

questionnaire(rotation)

design <- sb.design(operation = "construct",
                    nattributes = 4, 
                    nalternatives = 4,
                    nlevels = c(2, 5, 5, 5),
                    attribute.names = list(vidaHumana = c('si', 'no'),
                                        medioambiental = c('med1', 'med2', 'med3', 'med4', 'med5'),
                                        social = c('soc1', 'soc2', 'soc3', 'soc4', 'soc5'),
                                        economico = c('econ1', 'econ2', 'econ3', 'econ4', 'econ5')),
                    design = NULL, 
                    generators = c(1,1,1,1,1,1,1,1,1,1,1,1), 
                    effect = "main", 
                    interactions = NULL, 
                    determinant = NULL,
                    nblocks = 1, 
                    seed = NULL)