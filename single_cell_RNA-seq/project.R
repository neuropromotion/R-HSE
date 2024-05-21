path = '/Users/neuropromotion/Desktop/singlecell/gmb_for_integration'
dirs <- list.dirs(path = path, 
                  recursive = FALSE, full.names = FALSE)
#dirs <- dirs[1:4]


for (x in dirs) {
  file_path <- paste0(path, '/', x, '/data.txt')
  expression_matrix <- read.delim(file_path, row.names = 1)
  assign(x, CreateSeuratObject(counts = expression_matrix))
}


merged <- merge(sample1, y = c(sample2, sample3, sample4, sample5, sample6, sample7),
                      add.cell.id  = dirs,
                      project = 'GBM')

View(merged@meta.data)

#let's add new colums with barcodes and sample name
merged$sample <- rownames(merged@meta.data)

#now we separate previous share colums into sample name and barcode 
merged@meta.data <- separate(merged@meta.data, col = 'sample', into = c('Sample', 'Barcode'),
                                   sep = '_')

#check 
unique(merged$Sample)

#calculate percentage of mito genes
merged$mitoPercent <- PercentageFeatureSet(merged, pattern = '^MT-')

vln_plots <- VlnPlot(merged, features = c("nFeature_RNA", "nCount_RNA", "mitoPercent"), ncol = 3)
vln_plots[[1]] <- vln_plots[[1]] + geom_hline(yintercept = 4500, linetype = "dashed", color = "red")
vln_plots[[1]] <- vln_plots[[1]] + geom_hline(yintercept = 600, linetype = "dashed", color = "red")
vln_plots[[3]] <- vln_plots[[3]] + geom_hline(yintercept = 15, linetype = "dashed", color = "red")
vln_plots

merged_filtered <- subset(merged, subset = nFeature_RNA > 600 & nFeature_RNA < 4500 & mitoPercent < 15)
#print differences in sizes
merged_filtered
merged


merged_filtered = NormalizeData(merged_filtered)
#Find hightly varible genes
merged_filtered <- FindVariableFeatures(object = merged_filtered)
# Identify the 15 most highly variable genes
top15 <- head(VariableFeatures(merged_filtered), 15)

# plot variable features with and without labels
plot1 <- VariableFeaturePlot(merged_filtered)
plot2 <- LabelPoints(plot = plot1, points = top15, repel = TRUE)
plot2

#Scale data
#all.genes <- rownames(merged_naive)
#merged_naive <- ScaleData(merged_naive, features = all.genes)
merged_filtered <- ScaleData(object = merged_filtered)
#PCA
merged_filtered <- RunPCA(object = merged_filtered)
print(merged_filtered[["pca"]], dims = 1:5, nfeatures = 5)
DimHeatmap(merged_filtered, dims = 1, cells = 200, balanced = TRUE)
VizDimLoadings(merged_filtered, dims = 1:3, reduction = "pca")
DimPlot(merged_filtered, reduction = "pca") + NoLegend()
ElbowPlot(merged_filtered)


merged_filtered <- FindNeighbors(object = merged_filtered, dims = 1:20)
merged_filtered <- FindClusters(object = merged_filtered, resolution = 0.05)
merged_filtered <- RunUMAP(object = merged_filtered, dims = 1:20)

d1 <- DimPlot(merged_filtered, reduction = 'umap', group.by = 'Sample')
d2 <- DimPlot(merged_filtered, reduction = 'umap', label = TRUE)
grid.arrange(d1, d2, ncol=2, nrow=1)

DimPlot(merged_filtered, reduction = 'umap', label = TRUE)
View(merged_filtered@meta.data)




marker_genes <- list(
  "Cancer" = c("EGFR", "PDGFRA", "MDM2", "CDK4", "TP53"),
  "T-cells" = c("CD3D", "CD3E", "CD4", "CD8A"),
  "B-cells" = c("CD19", "MS4A1"),
  "Macrophages" = c("CD68", "CD163"),
  "NK-cells" = c("NCAM1", "KLRD1")
)

M1_genes <- c("TNF", "IL1B", "IL6", "NOS2", "CXCL9", "CXCL10", "CXCL11", "CD80", "CD86")
M2_genes <- c("ARG1", "CD163", "CD206", "IL10", "TGFB", "CCL17", "CCL18", "CCL22")

protooncogenes <- c("KRAS", "HRAS", "NRAS", "BRAF", "MYC", "ERBB2", "EGFR", "PIK3CA", 
                    "AKT1", "ALK", "RET", "MET", "NOTCH1", "SRC", "CCND1")


FeaturePlot(merged_filtered, features = marker_genes$`B-cells`, reduction = "umap")


FeaturePlot(merged_filtered, features = c('EGFR', 'GFAP', 'AQP4', 'TP53'), reduction = "umap")

x <- DimPlot(merged_filtered, reduction = 'umap', label = TRUE)
y <- FeaturePlot(merged_filtered, features = 'CD163', reduction = "umap")
grid.arrange(x, y, ncol=2, nrow=1)

FeaturePlot(merged_filtered, features = c('LYZ', 'HLA-DRB1', 'CD163', 'CD86', "HLA-DQB1", 'HLA-B'), reduction = "umap")













