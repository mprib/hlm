#---------------------------------------------------#
# EDP 380C 16: HLM
# Regression Review -- Write Data for Blimp Studio
#
# Copyright Brian T. Keller 2020 all rights reserved
#---------------------------------------------------#

# Set working directory
hlm::set_filedir()

# Subset dropout data
dropout <- na.omit(hlm::dropout[,c('math7','math8','math10','mothed')])

# Write Data
write.table(dropout,file='dropout.dat',
            row.names = F, col.names = F,
            na = '-999')