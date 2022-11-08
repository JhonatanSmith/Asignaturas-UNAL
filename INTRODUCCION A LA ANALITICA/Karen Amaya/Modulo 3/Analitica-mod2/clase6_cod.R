# UN EJEMPLO DE CÓMO FUNCIONAN LAS COMPONENTES PRINCIPALES:

# La página web donde se encuentran los datos es:
# https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.2685

Datos1<-read.table("Bases_Clase6/ColombianAnuranMorphology_individuals.txt",header=TRUE, sep="\t")
names(Datos1)

?princomp

Datos2<-na.omit(Datos1[,20:26])

summary(pc.cr <- princomp(Datos2, cor = TRUE))
loadings(pc.cr) 
plot(pc.cr)
biplot(pc.cr)
