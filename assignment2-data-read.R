## Assignment 2 Data Read in


# Read Data in
hlm::set_filedir()
probsolv <- read.table('probsolv.txt',
                      col.names = c('condition', 'age', 'female',
                                    'hispanic', 'lunch', 'esollevel',
                                    'stanmath', 'stanread', 'learndis',
                                    'ability1', 'ability2', 'ability3',
                                    'ability4', 'mathse')
                      )
