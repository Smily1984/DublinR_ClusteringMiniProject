# 04_featureselection.r
# :purpose: Reduce dataset to fewer features preserving information
# :author: jon.sedar@applied.ai
# :date: Mon 04 Nov 2013


p1 <- ggplot(cst[seq(1,nrow(cst),10)]) +
    geom_point(aes(x=prodctpb_max,y=popcat_nbaskD))
plot(p1)


### could try:
#   use fewer rows
#   use fewer dims
#   use fewer model types
#   use only the most basic model types
#   use fewer components
#   reduce dims using pca
#   reduce dims by selecting ones with greatest variance


# Dimension reduction using Z-score 
cstZi <- data.table(scale(cst[,which(!colnames(cst) %in% c("cust_id")),with=F]))
cstZall <- data.table(cst[,list(cust_id)],cstZi)

### IMPORTANT: DATA LOSS - REMOVING OUTLIERS
### (OVER 6 SD, keep at least 97% Chebyshev, and if normal dist along dim, then keep 99.999999)
sel <- apply(cstZall[,colnames(cstZi),with=F],1,function(x){if(max(abs(x))>6){T}else{F}})

# which rows contain outliers?
cstZoutliers <- cstZall[sel]
nrow(cstZoutliers)

# now reduce down to the good rows
cstZ <- copy(cstZall[!sel])
setkey(cstZ,cust_id)



# # make sure all is well
# means <- cstZ[,lapply(.SD,function(x){mean(x)}),.SDcols=colnames(cstZi)]
# meds <- cstZ[,lapply(.SD,function(x){median(x)}),.SDcols=colnames(cstZi)]
# 
# 
# # possible to excute gmm on just Zscored data? 
# maxes <- cstZ[,lapply(.SD,function(x){max(x)}),.SDcols=colnames(cstZi)]
# mins <- cstZ[,lapply(.SD,function(x){min(x)}),.SDcols=colnames(cstZi)]
# deltas <- data.table(maxes-mins)
# 
# nms <- data.table(row.names(t(deltas)))
# dt_deltas <- data.table(t(deltas),nms)
# setnames(dt_deltas,c("variance","col"))
# 
# #  take top 3 dimensions by variance
# hivarcols <- dt_deltas[order(-variance)][1:5,col]
# 
# cstZclst <- Mclust(cstZ[seq(1,nrow(cstZ),20),which(colnames(cstZ) %in% hivarcols),with=F],G=(1:6))
# summary(cstZclst,parameters=T)
# plot(cstZclst)



# 3. Dimension reduction using PCA 
cstPCAi <- prcomp(cstZ[,which(!colnames(cstZ) %in% c("cust_id")),with=F])
cstPCA <- data.table(cstZ[,list(cust_id)],cstPCAi$x)
cstPCAi$sdev             # sqrt of eigenvalues
cstPCAi$rotation         # loadings
cstPCAi$x                # PCs (aka scores)
summary(cstPCAi)         # view component details: first 3 PCs contain 87% of total variance


### NOTE: lets understand the composition of these PCs: ----------------
# correlations btwn feats and PCs
corrs <- data.table(featname=colnames(cstZi),cor(cstZ[,which(!colnames(cstZ) %in% c("cust_id")),with=F],cstPCAi$x))

###  plotting aids: arrows and a big circle
### http://gastonsanchez.wordpress.com/2012/06/17/principal-components-analysis-in-r-part-1/
arrows <- data.table(x1=rep(0,nrow(corrs)),y1=rep(0,nrow(corrs)),x2=corrs$PC1,y2=corrs$PC2)
arrows[,len:=sqrt((y2-y1)^2+(x2-x1)^2)]
corcir <- get_circle(c(0,0),npoints=100) 

# add arrows onto corrs
corrsarw <- data.table(corrs,arrows)

# All
p1 <- ggplot(corrsarw) +
    geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2,color=len)) +
    geom_text(aes(x=PC1,y=PC2,label=seq(1,nrow(corrsarw),1)),alpha=0.8,color="gray20") +
    geom_path(data=corcir,aes(x=x,y=y),color="gray65") +
    geom_hline(yintercept=0,color="gray65") +
    geom_vline(xintercept=0,color="gray65") +
    labs(list(title="Constituent Features of Selected Principal Components",x="PC1",y="PC2")) +
    scale_color_gradient(low="yellow",high="red") +
    guides(color=F) +
    theme(axis.ticks=element_blank()
        ,axis.text.x=element_text(angle=0,size=12,vjust=0)
        ,axis.text.y=element_text(angle=0,size=12,hjust=0)
        ,axis.title=element_text(angle=0,size=12,vjust=0)
        ,plot.title=element_text(face='bold',size=14,vjust=2)
        ,plot.margin=unit(c(0.08,0.05,0.05,0.05), "snpc"))
plot(p1)


# take topN of each PC
selectfeats <- c(corrsarw[order(-abs(PC1))][1:3,featname]
                 ,corrsarw[order(-abs(PC2))][1:3,featname])

p2 <- ggplot(corrsarw[featname %in% selectfeats]) +
    geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2,color=len)) +
    geom_text(aes(x=PC1,y=PC2,label=featname),alpha=0.8,color="gray20") +
    geom_path(data=corcir,aes(x=x,y=y),color="gray65") +
    geom_hline(yintercept=0,color="gray65") +
    geom_vline(xintercept=0,color="gray65") +
    labs(list(title="Top 3 Constituent Features of Selected Principal Components",x="PC1",y="PC2")) +
    scale_color_gradient(low="yellow",high="red") +
    guides(color=F) +
    theme(axis.ticks=element_blank()
        ,axis.text.x=element_text(angle=0,size=12,vjust=0)
        ,axis.text.y=element_text(angle=0,size=12,hjust=0)
        ,axis.title=element_text(angle=0,size=12,vjust=0)
        ,plot.title=element_text(face='bold',size=14,vjust=2)
        ,plot.margin=unit(c(0.08,0.05,0.05,0.05), "snpc"))
plot(p2)


# Observe top10 feats of each top5 PC
write.table(corrsarw[order(-abs(PC1))][1:5,list(featname,PC1)],sep=",",quote=F,row.names=F)
write.table(corrsarw[order(-abs(PC2))][1:5,list(featname,PC2)],sep=",",quote=F,row.names=F)
write.table(corrsarw[order(-abs(PC3))][1:5,list(featname,PC3)],sep=",",quote=F,row.names=F)
