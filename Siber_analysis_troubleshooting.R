Fold=TRUE  ##run this line for easier navigation

library(SIBER)
library(rjags)

##Use the next line to load the needed files
#load(file = "data/wrangeld_for_isotopes")

##Fold all - then unfold from top to bottom as you go along to see notes.

If (fold){
  
  ## I interpret the following analysis as showing that there are no significant differences amongst the two communities.
  ## Based stronly on the graphical representations shown in the density plots
  
If (fold){
##Compile and review the data setup - this should compare all three isotopes by group (Liv, Mus, or mucuS ) and community (artificial or natural)
temp_l= isotope[ ,c("c13l", "n15l", "sizecat", "location_type")]; colnames(temp_l); dim(temp_l)
temp_l=temp_l[!is.na(temp_l$sizecat), ]; dim(temp_l)  ##this takes out the prey items that were tested
temp_l$group=1 ##Group 1 is liver
temp_l$location_type=ifelse((temp_l$location_type) == "A", "1", "2") ##turning location into a numeric value artificial = 1, nat=2
names(temp_l)=c("iso1", "iso2", "sizecat", "community","group")  ##location_type renamed to community
temp_l= temp_l[ ,c("iso1", "iso2", "group", "community")]; summary(temp_l) ##size dropped for now
temp_l$community=as.numeric(temp_l$community)
temp_l=na.omit(temp_l); summary(temp_l); dim(temp_l)


temp_m= isotope[ ,c("c13m", "n15m", "sizecat", "location_type")]; colnames(temp_m); dim(temp_l)
temp_m=temp_m[!is.na(temp_m$sizecat), ]; dim(temp_m)##this takes out the prey items that were tested
temp_m$group=2 ##becasue this will denote muscle
temp_m$location_type=ifelse((temp_m$location_type) == "A", "1", "2") ##turning location into a numeric value artificial = 1, nat=2
names(temp_m)=c("iso1", "iso2", "sizecat", "community","group")  ##location_type renamed to community
temp_m= temp_m[ ,c("iso1", "iso2", "group", "community")]; summary(temp_m) ##size dropped for now
temp_m$community=as.numeric(temp_m$community)
temp_m=na.omit(temp_m); summary(temp_m); dim(temp_m)

temp_s= isotope[ ,c("c13s", "n15s", "sizecat", "location_type")]; colnames(temp_s)
temp_s=temp_s[!is.na(temp_s$sizecat), ]; dim(temp_s)##this takes out the prey items that were tested
temp_s$group=3 ##becasue this will denote ,mucus
temp_s$location_type=ifelse((temp_s$location_type) == "A", "1", "2") ##turning location into a numeric value artificial = 1, nat=2
names(temp_s)=c("iso1", "iso2", "sizecat", "community","group")  ##location_type renamed to community
temp_s= temp_s[ ,c("iso1", "iso2", "group", "community")]; summary(temp_s) ##size dropped for now
temp_s$community=as.numeric(temp_s$community)
temp_s=na.omit(temp_s); summary(temp_s); dim(temp_s)

alldata= merge(temp_l, temp_m, all=TRUE)

alldata= merge(alldata, temp_s, all=TRUE)

##review the dataset- made 3 groups (where 1=Liv, 2=Mus, & 3=mucuS)  and 2 communities (where 1 = artificial & 2 = natural).
summary(alldata); dim(alldata)
#View(alldata)

} ##Fold data wrangling for isotope at locaiton type comparison



If (fold){

# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hulls.args     <- list(lty = 2, col = "grey20")


siber.example <- createSiberObject(alldata)

par(mfrow=c(1,1))
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)


# community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
# group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
# group.hull.args      <- list(lty = 2, col = "grey20")
# 
# # this time we will make the points a bit smaller by 
# # cex = 0.5
# plotSiberObject(siber.example,
#                 ax.pad = 2, 
#                 hulls = F, community.hulls.args, 
#                 ellipses = F, group.ellipses.args,
#                 group.hulls = F, group.hull.args,
#                 bty = "L",
#                 iso.order = c(1,2),
#                 xlab=expression({delta}^13*C~'\u2030'),
#                 ylab=expression({delta}^15*N~'\u2030'),
#                 cex = 0.5
# )



# Calculate summary statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber.example)
print(group.ML)
# 1.2       1.1       2.2       2.1
# TA   8.389450 19.860700 11.443450 17.215650
# SEA  1.630509  2.613049  2.039266  2.745728
# SEAc 1.651413  2.636804  2.055982  2.778415


# You can add more ellipses by directly calling plot.group.ellipses()
# Add an additional p.interval % prediction ellilpse
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  lty = 1, lwd = 2)

# or you can add the XX% confidence interval around the bivariate means
# by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95, ci.mean = T,
                  lty = 1, lwd = 2)

# A second plot provides information more suitable to comparing
# the two communities based on the community-level Layman metrics

# this time we will make the points a bit smaller by 
# cex = 0.5
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = T, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5
)

# or you can add the XX% confidence interval around the bivariate means
# by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  ci.mean = T, lty = 1, lwd = 2) 

# Calculate the various Layman metrics on each of the communities.
community.ML <- communityMetricsML(siber.example) 
print(community.ML)

# 1         2
# dY_range 1.3338106 1.3190256
# dX_range 1.5303210 2.0602064
# TA       0.6029143 0.5021562
# CD       0.8271787 0.9800907
# MNND     1.1288067 1.1011090
# SDNND    0.1755286 0.6853317

# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber.example, parms, priors)

# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.
SEA.B <- siberEllipses(ellipses.posterior)

siberDensityPlot(SEA.B, xticklabels = colnames(group.ML), 
                 xlab = c("Community (site type)| Group (size)"),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses on each group"
)

# Add red x's for the ML estimated SEA-c
points(1:ncol(SEA.B), group.ML[3,], col="red", pch = "x", lwd = 2)

# Calculate some credible intervals 
cr.p <- c(0.95, 0.99) # vector of quantiles

# call to hdrcde:hdr using lapply()
SEA.B.credibles <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$hdr},
  prob = cr.p)

# do similar to get the modes, taking care to pick up multimodal posterior
# distributions if present
SEA.B.modes <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$mode},
  prob = cr.p, all.modes=T)

# extract the posterior means
mu.post <- extractPosteriorMeans(siber.example, ellipses.posterior)

# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)



# --------------------------------------
# Visualise the first community
# --------------------------------------
siberDensityPlot(layman.B[[1]], xticklabels = colnames(layman.B[[1]]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates (if you want). Extract the correct means 
# from the appropriate array held within the overall array of means.
comm1.layman.ml <- laymanMetrics(siber.example$ML.mu[[1]][1,1,],
                                 siber.example$ML.mu[[1]][1,2,]
)
points(1:6, comm1.layman.ml$metrics, col = "red", pch = "x", lwd = 2)

# --------------------------------------
# Visualise the second community
# --------------------------------------
siberDensityPlot(layman.B[[2]], xticklabels = colnames(layman.B[[2]]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates. (if you want) Extract the correct means 
# from the appropriate array held within the overall array of means.
comm2.layman.ml <- laymanMetrics(siber.example$ML.mu[[2]][1,1,],
                                 siber.example$ML.mu[[2]][1,2,]
)
points(1:6, comm2.layman.ml$metrics, col = "red", pch = "x", lwd = 2)


# --------------------------------------
# Alternatively, pull out TA from both and aggregate them into a 
# single matrix using cbind() and plot them together on one graph.
# --------------------------------------

# go back to a 1x1 panel plot
par(mfrow=c(1,1))

siberDensityPlot(cbind(layman.B[[1]][,"TA"], layman.B[[2]][,"TA"]),
                 xticklabels = c("Community 1", "Community 2"), 
                 bty="L", ylim = c(0,10),
                 las = 1,
                 ylab = "TA - Convex Hull Area",
                 xlab = "")

##these two communities dont differ in standard area


##Do they overlap?
ellipse1 <- "1.1"
ellipse2 <- "1.2"
ellipse3 <- "1.3"
ellipse4 <- "2.1"
ellipse5 <- "2.2"
ellipse6 <- "2.3"

sea.overlap <- maxLikOverlap(ellipse1, ellipse2, siber.example, p.interval = 0.95, n=100)
ellipse95.overlap<- maxLikOverlap(ellipse1, ellipse2, siber.example, p.interval = 0.95, n=100)
ellipse.posterior <- siberMVN(siber.example, parms, priors)
bayes95.overlap <-bayesianOverlap(ellipse1, ellipse2, ellipses.posterior, draws = 100, p.interval = 0.95, n=100)
hist(bayes95.overlap[ ,3] ,10)

bayes.prop.95.over <- (bayes95.overlap[ , 3]/(bayes95.overlap[ , 2]+bayes95.overlap[ , 1]-bayes95.overlap[ , 3]))
hist(bayes.prop.95.over ,10)


} ## fold isotope at location type  SIBER analysis

} ## FOLD - this section compares isotope by location type 

If (fold) {

##I interpret this data to show that there are now significant differences b/w muscle and liver in small fish.  Only differece is in mucs.
##Also no difference in liver or mucus in med fish, only difference in muscle - again, based on density plots.
##Not sure how to interpret the last graph displaying TA of Convex hull Area.  It makes the two communities look the same.
  
If (fold) {  
##Compile and review the data setup - this should compare all three isotopes by group (Liv, Mus, or mucuS ) and community (size - small or med)
temp_l= isotope[ ,c("c13l", "n15l", "sizecat", "location_type")]; colnames(temp_l); dim(temp_l)
temp_l=temp_l[!is.na(temp_l$sizecat), ]; dim(temp_l)  ##this takes out the prey items that were tested
temp_l$group=1 ##Group 1 is liver
temp_l$sizecat=ifelse((temp_l$sizecat) == "S", "1", "2") ##turning size into a numeric value small = 1, med=2
names(temp_l)=c("iso1", "iso2", "community", "location_type","group")  ##size renamed to community
temp_l= temp_l[ ,c("iso1", "iso2", "group", "community")]; summary(temp_l) ##locaiton dropped 
temp_l$community=as.numeric(temp_l$community)
temp_l=na.omit(temp_l); summary(temp_l); dim(temp_l)


temp_m= isotope[ ,c("c13m", "n15m", "sizecat", "location_type")]; colnames(temp_m); dim(temp_l)
temp_m=temp_m[!is.na(temp_m$sizecat), ]; dim(temp_m)##this takes out the prey items that were tested
temp_m=temp_m[!is.na(temp_m$c13m), ]; dim(temp_m)##this takes out the na items that were tested
temp_m$group=2 ##becasue this will denote muscle
temp_m$sizecat=ifelse((temp_m$sizecat) == "S", "1", "2") ##turning size into a numeric value small = 1, med=2names(temp_m)=c("iso1", "iso2", "sizecat", "community","group")  ##location_type renamed to community
names(temp_m)=c("iso1", "iso2", "community", "location_type","group")  ##location_type renamed to community
temp_m= temp_m[ ,c("iso1", "iso2", "group", "community")]; summary(temp_m) ##locaiton dropped
temp_m$community=as.numeric(temp_m$community)
temp_m=na.omit(temp_m); summary(temp_m); dim(temp_m)

temp_s= isotope[ ,c("c13s", "n15s", "sizecat", "location_type")]; colnames(temp_s)
temp_s=temp_s[!is.na(temp_s$sizecat), ]; dim(temp_s)##this takes out the prey items that were tested
temp_s$group=3 ##becasue this will denote ,mucus
temp_s$sizecat=ifelse((temp_s$sizecat) == "S", "1", "2") ##turning size into a numeric value small = 1, med=2names(temp_m)=c("iso1", "iso2", "sizecat", "community","group")  ##location_type renamed to community
names(temp_s)=c("iso1", "iso2", "community", "location_type","group")  ##location_type renamed to community
temp_s= temp_s[ ,c("iso1", "iso2", "group", "community")]; summary(temp_s) ##locaiton dropped
temp_s$community=as.numeric(temp_s$community)
temp_s=na.omit(temp_s); summary(temp_s); dim(temp_s)

alldata= merge(temp_l, temp_m, all=TRUE)

alldata= merge(alldata, temp_s, all=TRUE)

##review the dataset- made 3 groups (where 1=Liv, 2=Mus, & 3=mucuS)  and 2 communities (where 1 = small & 2 = medium sized fish).
summary(alldata); dim(alldata)
#View(alldata)

} ##fold wrangling for isotope by size SIBER comparison
  
  If (fold){
    
    # Create lists of plotting arguments to be passed onwards to each 
    # of the three plotting functions.
    community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
    group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
    group.hulls.args     <- list(lty = 2, col = "grey20")
    
    
    siber.example <- createSiberObject(alldata)
    
    par(mfrow=c(1,1))
    plotSiberObject(siber.example,
                    ax.pad = 2, 
                    hulls = F, community.hulls.args = community.hulls.args, 
                    ellipses = T, group.ellipses.args = group.ellipses.args,
                    group.hulls = T, group.hulls.args = group.hulls.args,
                    bty = "L",
                    iso.order = c(1,2),
                    xlab = expression({delta}^13*C~'\u2030'),
                    ylab = expression({delta}^15*N~'\u2030')
    )
    
    
    community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
    group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
    group.hull.args      <- list(lty = 2, col = "grey20")
    
    # this time we will make the points a bit smaller by 
    # cex = 0.5
    plotSiberObject(siber.example,
                    ax.pad = 2, 
                    hulls = F, community.hulls.args, 
                    ellipses = F, group.ellipses.args,
                    group.hulls = F, group.hull.args,
                    bty = "L",
                    iso.order = c(1,2),
                    xlab=expression({delta}^13*C~'\u2030'),
                    ylab=expression({delta}^15*N~'\u2030'),
                    cex = 0.5
    )
    
    
    
    # Calculate summary statistics for each group: TA, SEA and SEAc
    group.ML <- groupMetricsML(siber.example)
    print(group.ML)
    # 1.1       1.2      1.3       2.1
    # TA   28.590500 28.147450 3.549150 13.972750
    # SEA   3.274496  3.443478 1.090814  2.279346
    # SEAc  3.291202  3.460695 1.122897  2.290629
    # 2.2       2.3
    # TA   13.559000 2.9627500
    # SEA   1.110340 0.8094862
    # SEAc  1.115864 0.8297234
    
    
    # You can add more ellipses by directly calling plot.group.ellipses()
    # Add an additional p.interval % prediction ellilpse
    plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                      lty = 1, lwd = 2)
    
    # or you can add the XX% confidence interval around the bivariate means
    # by specifying ci.mean = T along with whatever p.interval you want.
    plotGroupEllipses(siber.example, n = 100, p.interval = 0.95, ci.mean = T,
                      lty = 1, lwd = 2)
    
    # A second plot provides information more suitable to comparing
    # the two communities based on the community-level Layman metrics
    
    # this time we will make the points a bit smaller by 
    # cex = 0.5
    plotSiberObject(siber.example,
                    ax.pad = 2, 
                    hulls = T, community.hulls.args, 
                    ellipses = F, group.ellipses.args,
                    group.hulls = F, group.hull.args,
                    bty = "L",
                    iso.order = c(1,2),
                    xlab=expression({delta}^13*C~'\u2030'),
                    ylab=expression({delta}^15*N~'\u2030'),
                    cex = 0.5
    )
    
    # or you can add the XX% confidence interval around the bivariate means
    # by specifying ci.mean = T along with whatever p.interval you want.
    plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                      ci.mean = T, lty = 1, lwd = 2) 
    
    # Calculate the various Layman metrics on each of the communities.
    community.ML <- communityMetricsML(siber.example) 
    print(community.ML)
    
    # dY_range 1.41564056 1.3518697
    # dX_range 1.61359686 2.0062422
    # TA       0.49085616 0.6400910
    # CD       0.82458133 0.9612781
    # MNND     1.15695445 1.1481142
    # SDNND    0.03385127 0.5754062
    
    # options for running jags
    parms <- list()
    parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
    parms$n.burnin <- 1 * 10^3 # discard the first set of values
    parms$n.thin <- 10     # thin the posterior by this many
    parms$n.chains <- 2        # run this many chains
    
    # define the priors
    priors <- list()
    priors$R <- 1 * diag(2)
    priors$k <- 2
    priors$tau.mu <- 1.0E-3
    
    # fit the ellipses which uses an Inverse Wishart prior
    # on the covariance matrix Sigma, and a vague normal prior on the 
    # means. Fitting is via the JAGS method.
    ellipses.posterior <- siberMVN(siber.example, parms, priors)
    
    # The posterior estimates of the ellipses for each group can be used to
    # calculate the SEA.B for each group.
    SEA.B <- siberEllipses(ellipses.posterior)
    
    siberDensityPlot(SEA.B, xticklabels = colnames(group.ML), 
                     xlab = c("Community (site type)| Group (size)"),
                     ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                     bty = "L",
                     las = 1,
                     main = "SIBER ellipses on each group"
    )
    
    # Add red x's for the ML estimated SEA-c
    points(1:ncol(SEA.B), group.ML[3,], col="red", pch = "x", lwd = 2)
    
    # Calculate some credible intervals 
    cr.p <- c(0.95, 0.99) # vector of quantiles
    
    # call to hdrcde:hdr using lapply()
    SEA.B.credibles <- lapply(
      as.data.frame(SEA.B), 
      function(x,...){tmp<-hdrcde::hdr(x)$hdr},
      prob = cr.p)
    
    # do similar to get the modes, taking care to pick up multimodal posterior
    # distributions if present
    SEA.B.modes <- lapply(
      as.data.frame(SEA.B), 
      function(x,...){tmp<-hdrcde::hdr(x)$mode},
      prob = cr.p, all.modes=T)
    
    # extract the posterior means
    mu.post <- extractPosteriorMeans(siber.example, ellipses.posterior)
    
    # calculate the corresponding distribution of layman metrics
    layman.B <- bayesianLayman(mu.post)
    
    
    
    # --------------------------------------
    # Visualise the first community
    # --------------------------------------
    siberDensityPlot(layman.B[[1]], xticklabels = colnames(layman.B[[1]]), 
                     bty="L", ylim = c(0,20))
    
    # add the ML estimates (if you want). Extract the correct means 
    # from the appropriate array held within the overall array of means.
    comm1.layman.ml <- laymanMetrics(siber.example$ML.mu[[1]][1,1,],
                                     siber.example$ML.mu[[1]][1,2,]
    )
    points(1:6, comm1.layman.ml$metrics, col = "red", pch = "x", lwd = 2)
    
    # --------------------------------------
    # Visualise the second community
    # --------------------------------------
    siberDensityPlot(layman.B[[2]], xticklabels = colnames(layman.B[[2]]), 
                     bty="L", ylim = c(0,20))
    
    # add the ML estimates. (if you want) Extract the correct means 
    # from the appropriate array held within the overall array of means.
    comm2.layman.ml <- laymanMetrics(siber.example$ML.mu[[2]][1,1,],
                                     siber.example$ML.mu[[2]][1,2,]
    )
    points(1:6, comm2.layman.ml$metrics, col = "red", pch = "x", lwd = 2)
    
    
    # --------------------------------------
    # Alternatively, pull out TA from both and aggregate them into a 
    # single matrix using cbind() and plot them together on one graph.
    # --------------------------------------
    
    # go back to a 1x1 panel plot
    par(mfrow=c(1,1))
    
    siberDensityPlot(cbind(layman.B[[1]][,"TA"], layman.B[[2]][,"TA"]),
                     xticklabels = c("Community 1", "Community 2"), 
                     bty="L", ylim = c(0,20),
                     las = 1,
                     ylab = "TA - Convex Hull Area",
                     xlab = "")
    
  } ## fold isotope by size SIBER comparison


} ##FOLD - this section folds SIBER isotope comparision by size class