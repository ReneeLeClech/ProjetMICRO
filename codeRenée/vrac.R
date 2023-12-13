lignes_vides_1 <- merge1RH[which(is.na(merge1RH$RH)),]
lignes_vides_2 <- merge2RH[which(is.na(merge2RH$RH)),]
lignes_vides_3 <- merge3RH[which(is.na(merge3RH$RH)),]
lignes_vides_4 <- merge4RH[which(is.na(merge4RH$RH)),]


indices_communs <- Reduce(intersect, 
                          list(which(is.na(merge1RH$RH)), 
                               which(is.na(merge2RH$RH)), 
                               which(is.na(merge3RH$RH)), 
                               which(is.na(merge4RH$RH))))

comunes <- merge1RH[indices_communs, ]


###------------

lignes_vides_1 <- merge1LUX[which(is.na(merge1LUX$LUX)),]
lignes_vides_2 <- merge2LUX[which(is.na(merge2LUX$LUX)),]
lignes_vides_3 <- merge3LUX[which(is.na(merge3LUX$LUX)),]
lignes_vides_4 <- merge4LUX[which(is.na(merge4LUX$LUX)),]


indices_communs <- Reduce(intersect, 
                          list(which(is.na(merge1LUX$LUX)), 
                               which(is.na(merge2LUX$LUX)), 
                               which(is.na(merge3LUX$LUX)), 
                               which(is.na(merge4LUX$LUX))))

luxcomunes <- merge1LUX[indices_communs, ]
