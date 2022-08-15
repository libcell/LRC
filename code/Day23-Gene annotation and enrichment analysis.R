
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 28th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 20: Primary drawing in R.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# 1) For windows, work directory. 

pri.dir <- getwd()

if (!dir.exists("asthma")) dir.create("asthma")

setwd("asthma")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. GO Enrichment Analysis. 

library(clusterProfiler)
library(org.Hs.eg.db)
library(topGO)

data(geneList, package="DOSE")
gene <- names(geneList)[abs(geneList) > 2]

# Entrez gene ID
head(gene)

# (1) Over-Representation Analysis (Fisher exactly test)

ego <- enrichGO(
  gene          = gene,
  keyType = "ENTREZID", # 默认为ENTREZID, 该参数的取值可以参考keytypes(org.Hs.eg.db)的结果
  OrgDb         = org.Hs.eg.db, # 指定该物种对应的org包的名字
  ont           = "CC", # 代表GO的3大类别，BP, CC, MF
  pAdjustMethod = "BH", # 指定多重假设检验矫正的方法
  pvalueCutoff  = 0.01, # 指定对应的阈值
  qvalueCutoff  = 0.05,
  readable      = TRUE) # 代表将基因ID转换为gene symbol

head(ego)

gene.df <- bitr(gene, fromType = "ENTREZID",
                toType = c("ENSEMBL", "SYMBOL"),
                OrgDb = org.Hs.eg.db)

ego2 <- enrichGO(gene         = gene.df$ENSEMBL,
                 OrgDb         = org.Hs.eg.db,
                 keyType       = 'ENSEMBL',
                 ont           = "CC",
                 pAdjustMethod = "BH",
                 pvalueCutoff  = 0.01,
                 qvalueCutoff  = 0.05)
head(ego2, 3)                

# (2) Gene Set Enrichment Analysis

ego3 <- gseGO(
  geneList     = geneList,
  OrgDb        = org.Hs.eg.db,
  ont          = "CC",
  nPerm        = 1000,
  minGSSize    = 100,
  maxGSSize    = 500,
  pvalueCutoff = 0.05,
  verbose      = FALSE)

barplot(ego, showCategory = 10)
goplot(ego)
plotGOgraph(ego)

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. GO Enrichment Analysis. 

library(clusterProfiler)
search_kegg_organism('ece', by='kegg_code')

ecoli <- search_kegg_organism('Escherichia coli', by='scientific_name')
dim(ecoli)

head(ecoli)

# (1) ORA

data(geneList, package="DOSE")
gene <- names(geneList)[abs(geneList) > 2]

kk <- enrichKEGG(gene         = gene,
                 organism     = 'hsa',
                 pvalueCutoff = 0.05)
head(kk)

# (2) GSEA

kk2 <- gseKEGG(geneList     = geneList,
               organism     = 'hsa',
               minGSSize    = 120,
               pvalueCutoff = 0.05,
               verbose      = FALSE)
head(kk2)

# (3) KEGG Module ORA

mkk <- enrichMKEGG(gene = gene,
                   organism = 'hsa',
                   pvalueCutoff = 1,
                   qvalueCutoff = 1)
head(mkk)                   

# (4) KEGG Module GSEA

mkk2 <- gseMKEGG(geneList = geneList,
                 organism = 'hsa',
                 pvalueCutoff = 1)
head(mkk2)

# (5) Visualization

browseKEGG(kk, 'hsa04110')

library("pathview")
hsa04110 <- pathview(gene.data  = geneList,
                     pathway.id = "hsa04110",
                     species    = "hsa",
                     limit      = list(gene=max(abs(geneList)), cpd=1))



