tamannoMuesta <- function(Nh, s2h, wh, D){
    a <- (Nh^2) * s2h / wh
    numerador <- sum(a)
    denom <- sum(Nh*s2h)
    N <- sum(Nh)
    n <- numerador/((N^2)*D + denom )
    return(n)
}

mediaEstratificada <- function(Nh, yh){
    N <- sum(Nh)
    yest <- sum(Nh*yh)/N
    return(yest)
}

varianzaMediasEnCadaEstrato <- function(Nh, nh, s2h){
    Varhaty <- (1- nh/Nh)*s2h/nh
    return(Varhaty)
}

varianzaPropEnCadaEstrato <- function(Nh, nh, ph){
    qh <- 1-ph
    Var <- (1 - nh/Nh)*ph*qh/(nh-1)
    return(Var)
}

varianzaEstimadorMediaEstrarificada <- function(Nh, Varhaty){
    N <- sum(Nh)
    sum(Nh^2 * Varhaty)/(N^2)
}

afijacionMuestra <- function(Nh = 1, sh = 1, Ch = 1, H = 1){
    # Solo se debe pasar el parámetro 'H' 
    # cuando es el único que se conoce.
    
    num <- Nh*sh/sqrt(Ch)
    denom <- H* sum(Nh*sh/sqrt(Ch))
    wh <- num/denom
    return(wh)
}