msm = function(Seed, n) {
    a = nchar(Seed)
    b = nchar(Seed)/2
    empty.vec = NULL
    for (i in 1:n) {
        Seedseed  = Seed^2
        Seed      = (Seedseed%/%10^b)%%10^a
        empty.vec = c(empty.vec, Seed)
    }
    return(empty.vec)
}


mcg.lehmer = function(n, a = 48271, m = 2147483647) {
    Rnum      = as.numeric(Sys.time())
    Rnumb.out = vector(length = n)
    for (i in 1:n) {
        Rnum         = (a * Rnum)%%m
        Rnumb.out[i] = Rnum/m
    }
    return(Rnumb.out)
}


LFG = function(n, j = 7, k = 10, m = 2147483647, Seed = c(4, 8, 2, 8, 3, 9, 1, 8, 7, 1)) {
    gn.out = NULL
    for (i in 0:n) {
        Output    = (Seed[j + i] + Seed[k + i])%%m
        i         = i + 1
        Seed      = append(Seed, Output)
        gn.out[i] = Output
    }
    return(gn.out)
}


# Figure 1:
plot(msm(123456, 300), main = "msm(123456, 300)", xlab = "Generated random numbers", ylab = "Interval [0, 1000000]")


# Figure 2:
plot(msm(4373, 300), main = "msm(4373, 300)", xlab = "Generated random numbers", ylab = "Interval [0, 10000]")


# Figure 3:
nmbrs = c(100, 1000, 5000, 10000, 20000, 50000, 75000, 1e+05)
timecheck.PRNG = function(input = nmbrs, n = length(input)) {
    a1 <<- vector()
    b1 <<- vector()
    c1 <<- vector()
    d1 <<- vector()
    for (i in 1:n) {
        a = system.time(msm(123456, input[i]))[1]
        b = system.time(mcg.lehmer(input[i]))[1]
        c = system.time(LFG(input[i]))[1]
        d = system.time(runif(input[i], 0, 1))[1]
        a1[i] <<- a
        b1[i] <<- b
        c1[i] <<- c
        d1[i] <<- d
    }
}

timecheck.PRNG()

plot(a1, type = "b", main = "Performance check", col = "red", xlab = "Generated random numbers", ylab = "Time (in seconds)", 
    xaxt = "n")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
lines(b1, col = "green", type = "b")
lines(c1, col = "blue", type = "b")
lines(d1, col = "brown", type = "b")
axis(1, at = 1:length(nmbrs), labels = nmbrs)
legend("topleft", legend = c("LFG", "MSM", "MCG", "runif"), lwd = c(2, 2, 2, 2), col = c("blue", "red", "green", "brown"), 
    lty = c(1, 1, 1, 1))


# Figure 4:
nmbrs = c(100, 1000, 5000, 10000, 20000, 50000, 75000, 1e+05, 2e+05, 5e+05, 750000, 1e+06)
timecheck.PRNG = function(input = nmbrs, n = length(input)) {
    b1 <<- vector()
    d1 <<- vector()
    for (i in 1:n) {
        b = system.time(mcg.lehmer(input[i]))[1]
        d = system.time(runif(input[i], 0, 1))[1]
        b1[i] <<- b
        d1[i] <<- d
    }
}

timecheck.PRNG()

plot(b1, type = "b", main = "Performance check", col = "red", xlab = "Generated random numbers", ylab = "Time (in seconds)", 
    xaxt = "n")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
lines(d1, col = "green", type = "b")
axis(1, at = 1:length(nmbrs), labels = nmbrs)
legend("topleft", legend = c("MCG", "runif"), lwd = c(2, 2), col = c("red", "green"), lty = c(1, 1))


# Figure 5:
plot(msm(654321, 1000), main = "msm(654321, 1000)", xlab = "Random numbers", ylab = "Interval")


# Figure 6:
plot(mcg.lehmer(1000), main = "mcg.lehmer(1000)", xlab = "Random numbers", ylab = "Interval")


# Figure 7:
plot(LFG(1000), main = "LFG(1000)", xlab = "Random numbers", ylab = "Interval")


# Figure 8:
plot(runif(1000), main = "runif(1000, 0, 1)", xlab = "Random numbers", ylab = "Interval")


E = 10000
# Result (7)
O1 = table(cut(runif(20000), seq(0, 1, by = 0.5)))
sum(((O1 - E)^2)/E)


# Result (8)
O2 = table(cut(mcg.lehmer(20000), seq(0, 1, by = 0.5)))
sum(((O2 - E)^2)/E)


# Result (9)
O3 = table(cut(LFG(20000), seq(0, 2147483647, by = (2147483647/2))))
sum(((O3 - E)^2)/E)
