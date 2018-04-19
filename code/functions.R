### gaussian kernel ###
gaussian_kernel = function(z){
    out = (1/sqrt(2*pi))*exp(-(1/2)*z^2)
    return(out)
}

### kernel density estimation ###
# x is data we estimate (n by 1 matrix)
# h is a bandwidth (scalar)
# cx is points we estimate
# n is number of the data set

kernel_density = function(y, h, cy){
    out = rep(0, length = length(cx))
    for(i in 1:length(cy)){
        out[i] = (1/(n*h))*sum(gaussian_kernel((y - cy[i])/h))
    }
    return(out)
}

### kernel regression ###
# y is explained variables (n by 1 matrix)
# x is explanatory variables (n by 1 matrix)
# h is a bandwidth (scalar)
# cx is points we estimate
# n is number of the data set

kernel_regression = function(y, x, h, cx){
    out = rep(0, length = length(cx))
    j = 1
    for(i in cx){
        Kz = gaussian_kernel((x - i)/h)
        sumZ = sum(Kz)
        w = Kz/sumZ
        out_z = sum(w*y)
        out[j] = ifelse(is.na(out_z), 0, out_z)
        j = j + 1
    }
    return(out)
}
