library(dplyr)
library(Seurat)
library(patchwork)
pbmc.data <- Read10X(data.dir = "/brahms/mollag/practice/filtered_gene_bc_matrices/hg19/")

