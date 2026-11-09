#task1 multivariate statistics assignement 1
set.seed(123)

#-------------------------------------------
#1. load necessary packages
#-------------------------------------------

library(paran) #package for Horn's procedure


#-------------------------------------------
#2. load input data
#-------------------------------------------

load("data/agdata.Rdata")

#-------------------------------------------
#3. recoding variables
#-------------------------------------------

agdata$freehms <- with(agdata,max(freehms)-freehms) #recode such that max score is a tolerant view on gay/lesbian
agdata$hmsacld <- with(agdata, max(hmsacld)-hmsacld)


agdata$rlgatnd <- with(agdata, max(rlgatnd)-rlgatnd) #recode such that max score is more religious 
agdata$pray <- with(agdata, max(pray)-pray)

#-------------------------------------------
#4. standardizing variables
#-------------------------------------------

zdata <- scale(agdata[,1:16], center = TRUE, scale = TRUE)

#-------------------------------------------
#5. applying pca to standardized variables
#-------------------------------------------

agpca <- prcomp(zdata)
summary(agpca)
round(agpca$sdev^2,2)

#--------------------------------------------
#6. select number of components using procedure of Horn
#--------------------------------------------

paran(zdata,iterations=5000,graph=TRUE,cfa=FALSE,centile=0) #retain only eigenvalues for which the observed eigenvalue > mean of bootstrap

paran(zdata, iterations = 5000, graph = TRUE, cfa = FALSE, centile = 95) #eigenvalue > 95th percentile bootstrap


#--------------------------------------------
#7. compute component loadings
#--------------------------------------------

loadingzdata<-agpca$rotation%*%diag(agpca$sd)
round(loadingzdata,2)


#--------------------------------------------
#8. standardize component scores
#--------------------------------------------

zstandard <- zdata%*%agpca$rotation%*%diag(1/agpca$sdev)

round(zstandard[,1:2],2) #only look at scores for PC1 and PC2



#--------------------------------------------
#9. biplot (written by ai to create a clearer code)
#--------------------------------------------


S  <- zstandard[, 1:2]              # standardized scores z*_i (countries)
A2 <- loadingzdata[, 1:2]           # loadings a_j (items)
k  <- 2                        # arrow stretch factor (angles unchanged)
ve <- round(100 * agpca$sdev^2 / sum(agpca$sdev^2), 1)
item2concept <- c(ppltrst = "social trust",  pplfair = "social trust",  pplhlp = "social trust",
                  freehms = "gays/lesbians", hmsfmlsh = "gays/lesbians", hmsacld = "gays/lesbians",
                  imbgeco = "immigrants",    imueclt = "immigrants",    imwbcnt = "immigrants",
                  rlgdgr = "religiousness",  rlgatnd = "religiousness", pray = "religiousness",
                  eqwrkbg = "gender equality", eqpolbg = "gender equality",
                  eqmgmbg = "gender equality", eqpaybg = "gender equality")
concept <- item2concept[colnames(zdata)]
cols <- c("social trust"    = "#E00000",
          "gays/lesbians"   = "#0040FF",
          "immigrants"      = "#00A000",
          "religiousness"   = "#B000D0",
          "gender equality" = "#FF8000")
ltys <- c("social trust" = 1, "gays/lesbians" = 1, "immigrants" = 2,
          "religiousness" = 1, "gender equality" = 1)

m     <- ceiling(2 * max(abs(c(S, k * A2)))) / 2   # largest value in the plot, rounded up to 0.5
lim   <- c(-m, m)
major <- seq(-m, m, by = 0.5)    # labelled ticks and darker grid lines
minor <- seq(-m, m, by = 0.25)   # unlabelled ticks and light grid lines


# two panels: plot on the left, legend on the right
layout(matrix(1:2, nrow = 1), widths = c(4, 1.4))
# ---- panel 1: biplot ----
par(pty = "s", mar = c(4.5, 4.5, 3, 0.5))
plot(NA, xlim = lim, ylim = lim, asp = 1, axes = FALSE,
     xlab = paste0("PC1 (", ve[1], "% of variance)"),
     ylab = paste0("PC2 (", ve[2], "% of variance)"),
     main = "Biplot of countries and items", cex.main = 1.2)
# grid in the background
abline(v = minor, h = minor, col = "grey93", lwd = 1)
abline(v = major, h = major, col = "grey82", lwd = 1)
abline(h = 0, v = 0, col = "grey45", lwd = 1.5)       # the axes through the origin
# precise axes: labelled ticks every 0.5, small ticks every 0.25
axis(1, at = major, labels = major, cex.axis = 0.8)
axis(2, at = major, labels = major, cex.axis = 0.8, las = 1)
axis(1, at = minor, labels = FALSE, tcl = -0.25)
axis(2, at = minor, labels = FALSE, tcl = -0.25)
box()
# countries first (grey), arrows on top
text(S[, 1], S[, 2], labels = rownames(zdata), col = "grey30", cex = 0.9)
arrows(0, 0, k * A2[, 1], k * A2[, 2], length = 0.12, lwd = 3,
       col = cols[concept], lty = ltys[concept])
# ---- panel 2: legend outside the plot ----
par(pty = "m", mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = names(cols), col = cols, lty = ltys, lwd = 3,
       title = "Concept\n(arrows = items)", bty = "n", cex = 1.1, y.intersp = 1.6)
mtext(paste0("Arrows stretched\nby k = ", k), side = 1, line = -3, cex = 0.8)
layout(1)                           # reset the plotting layout afterwards

