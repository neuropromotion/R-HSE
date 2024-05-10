library(dplyr)
library(Seurat)
library(patchwork)
#############################################################################
#################################NAIVE#######################################
#############################################################################
# Load the gmb 0 days dataset
path.naive  = '/Users/neuropromotion/Desktop/singlecell/gbm_rad/0days'
gbm.naive.data <- Read10X(data.dir = path.naive)
# Initialize the Seurat object with the raw (non-normalized data).
gbm.naive <- CreateSeuratObject(counts = gbm.naive.data, project = "naive", min.cells = 3, min.features = 200)
gbm.naive
gbm.naive[["percent.mt"]] <- PercentageFeatureSet(gbm.naive, pattern = "^MT-")

# Visualize QC metrics as a violin plot
VlnPlot(gbm.naive, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)
plot1 <- FeatureScatter(gbm.naive, feature1 = 'nCount_RNA', feature2 = 'nFeature_RNA')
plot2 <- FeatureScatter(gbm.naive, feature1 = 'nCount_RNA', feature2 = "percent.mt")
plot1 + plot2

# filter
gbm.naive <- subset(gbm.naive, subset = nFeature_RNA > 200 & nFeature_RNA < 6000 & percent.mt < 15)
VlnPlot(gbm.naive, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)

#Normalize
gbm.naive = NormalizeData(gbm.naive)

#Find hightly varible genes
gbm.naive <- FindVariableFeatures(gbm.naive, selection.method = "vst", nfeatures = 2000)

# Identify the 20 most highly variable genes
top10 <- head(VariableFeatures(gbm.naive), 10)

# plot variable features with and without labels
plot1 <- VariableFeaturePlot(gbm.naive)
plot2 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)
plot2

#Scale data
all.genes.naive <- rownames(gbm.naive)
gbm.naive <- ScaleData(gbm.naive, features = all.genes)

#PCA
gbm.naive <- RunPCA(gbm.naive, features = VariableFeatures(object = gbm.naive))
# Examine and visualize PCA results a few different ways
print(gbm.naive[["pca"]], dims = 1:5, nfeatures = 5)

VizDimLoadings(gbm.naive, dims = 1:2, reduction = "pca")

DimPlot(gbm.naive, reduction = "pca") + NoLegend()
DimHeatmap(gbm.naive, dims = 1, cells = 500, balanced = TRUE)
ElbowPlot(pbmc)


#UMAP
gbm.naive <- FindNeighbors(gbm.naive, dims = 1:10)
gbm.naive <- FindClusters(gbm.naive, resolution = 0.5)

gbm.naive <- RunUMAP(gbm.naive, dims = 1:10)
# note that you can set `label = TRUE` or use the LabelClusters function to help label
# individual clusters
DimPlot(gbm.naive, reduction = "umap")

#############################################################################
#################################3-DAYS#######################################
#############################################################################
# Load the gmb 0 days dataset
path.3days.path  = '/Users/neuropromotion/Desktop/singlecell/gbm_rad/3days'
gbm.3days.data <- Read10X(data.dir = path.3days.path)
# Initialize the Seurat object with the raw (non-normalized data).
gbm.3days <- CreateSeuratObject(counts = gbm.3days.data, project = "naive", min.cells = 3, min.features = 200)

gbm.3days[["percent.mt"]] <- PercentageFeatureSet(gbm.3days, pattern = "^MT-")

# Visualize QC metrics as a violin plot
VlnPlot(gbm.3days, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)

plot1 <- FeatureScatter(gbm.3days, feature1 = 'nCount_RNA', feature2 = 'nFeature_RNA')
plot2 <- FeatureScatter(gbm.3days, feature1 = 'nCount_RNA', feature2 = "percent.mt")
plot1 + plot2

# filter
gbm.3days <- subset(gbm.3days, subset = nFeature_RNA > 200 & nFeature_RNA < 4500 & percent.mt < 15)
VlnPlot(gbm.3days, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)

#Normalize
gbm.3days = NormalizeData(gbm.3days)

#Find hightly varible genes
gbm.3days <- FindVariableFeatures(gbm.3days, selection.method = "vst", nfeatures = 2000)

# Identify the 20 most highly variable genes
top20.3days <- head(VariableFeatures(gbm.3days), 20)

# plot variable features with and without labels
plot1 <- VariableFeaturePlot(gbm.3days)
plot2 <- LabelPoints(plot = plot1, points = top20.3days, repel = TRUE)
plot2

#Scale data
all.genes.3days <- rownames(gbm.3days)
gbm.3days <- ScaleData(gbm.3days, features = all.genes)

#PCA
gbm.3days <- RunPCA(gbm.3days, features = VariableFeatures(object = gbm.3days))
# Examine and visualize PCA results a few different ways
print(gbm.3days[["pca"]], dims = 1:5, nfeatures = 5)

VizDimLoadings(gbm.3days, dims = 1:2, reduction = "pca")

DimPlot(gbm.3days, reduction = "pca") + NoLegend()
DimHeatmap(gbm.3days, dims = 1, cells = 500, balanced = TRUE)
ElbowPlot(gbm.3days)


#UMAP
gbm.3days <- FindNeighbors(gbm.3days, dims = 1:10)
gbm.3days <- FindClusters(gbm.3days, resolution = 0.5)

gbm.3days <- RunUMAP(gbm.3days, dims = 1:10)
# note that you can set `label = TRUE` or use the LabelClusters function to help label
# individual clusters
DimPlot(gbm.3days, reduction = "umap")

gbm.3days@meta.data
