library(Seurat)
library(ggplot2)
library(tidyverse)
library(gridExtra)
path = '/Users/neuropromotion/Desktop/singlecell/gbm_rad/data'
dirs <- list.dirs(path = path, 
                  recursive = FALSE, full.names = FALSE)
folders = c('0days', '2days')


### Now perform integrating through naive samples (0 day of radiotherapy)

#let's create seurat object through our data samples
for (x in dirs){
  name = gsub('_filtered_feature_bc_matrix', '', x)
  cts <- ReadMtx(mtx = paste0(path, '/', x, '/', folders[2], '/matrix.mtx'),
          features = paste0(path, '/', x, '/', folders[2], '/genes.tsv'),
          cells = paste0(path, '/', x, '/', folders[2], '/barcodes.tsv'))
  assign(name, CreateSeuratObject(counts = cts))
}
ls()
#merge samples
merged_naive <- merge(Sample1, y = c(Sample2, Sample3),
      add.cell.id  = ls()[6:8],
      project = 'Naive cells')
#take a look on our data

#View(merged_naive@meta.data)

#let's add new colums with barcodes and sample name
merged_naive$sample <- rownames(merged_naive@meta.data)

#now we separate previous share colums into sample name and barcode 
merged_naive@meta.data <- separate(merged_naive@meta.data, col = 'sample', into = c('Sample', 'Barcode'),
         sep = '_')

#check 
unique(merged_naive$Sample)

#calculate percentage of mito genes
merged_naive$mitoPercent <- PercentageFeatureSet(merged_naive, pattern = '^MT-')

#explore QC
VlnPlot(merged_naive, features = c("nFeature_RNA", "nCount_RNA", "mitoPercent"), ncol = 3)
plot1 <- FeatureScatter(merged_naive, feature1 = 'nCount_RNA', feature2 = 'nFeature_RNA')
plot2 <- FeatureScatter(merged_naive, feature1 = 'nCount_RNA', feature2 = "mitoPercent")
plot1 + plot2
#filtering
merged_naive_filtered <- subset(merged_naive, subset = nFeature_RNA > 500 & nFeature_RNA < 4000 & mitoPercent < 15)
#print differences in sizes
merged_naive_filtered
merged_naive

###################Dim reduction and clustering:

#Normalize
merged_naive_filtered = NormalizeData(merged_naive_filtered)
#Find hightly varible genes
merged_naive_filtered <- FindVariableFeatures(object = merged_naive_filtered)
# Identify the 10 most highly variable genes
top10 <- head(VariableFeatures(merged_naive_filtered), 10)

# plot variable features with and without labels
plot1 <- VariableFeaturePlot(merged_naive_filtered)
plot2 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)
plot2

#Scale data
#all.genes <- rownames(merged_naive)
#merged_naive <- ScaleData(merged_naive, features = all.genes)
merged_naive_filtered <- ScaleData(object = merged_naive_filtered)
#PCA
merged_naive_filtered <- RunPCA(object = merged_naive_filtered)
ElbowPlot(merged_naive_filtered)


merged_naive_filtered <- FindNeighbors(object = merged_naive_filtered, dims = 1:20)
merged_naive_filtered <- FindClusters(object = merged_naive_filtered)
merged_naive_filtered <- RunUMAP(object = merged_naive_filtered, dims = 1:20, resolution = 0.25)

d1 <- DimPlot(merged_naive_filtered, reduction = 'umap', group.by = 'Sample')
d2 <- DimPlot(merged_naive_filtered, reduction = 'umap')
grid.arrange(d1, d2, ncol=2, nrow=2)




