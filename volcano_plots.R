volcano_df <- dataDEGs_CHOL
volcano_df$Component <- ifelse(volcano_df$gene_name %in% XBP1sign, "XBP1",
                               ifelse(volcano_df$gene_name %in% RIDDsign, "RIDD",
                                      ifelse(volcano_df$gene_name == "ERN1", "ERN1", NA)))

volcano_df$Diff.Expr <- ifelse(volcano_df$logFC > 0.5 & volcano_df$PValue < 0.05, "UP",
                               ifelse(volcano_df$logFC < -0.5 & volcano_df$PValue < 0.05, "DOWN", "NO"))

component_cols <- c("ERN1" = "#F98400", "RIDD" = "#046C9A", "XBP1" = "#6c5099")

# volcano plot containing most genes
volcano <- ggplot(data = volcano_df, aes(x = logFC, y = -log10(PValue), col = Diff.Expr)) +
  theme_pubr() +
  geom_vline(xintercept = c(0.5, -0.5), col = 'gray', linetype = 'dashed') +
  geom_hline(yintercept = c(-log10(0.05)), col = 'gray', linetype = 'dashed') +
  geom_point(size = 1) +
  scale_color_manual(values = c("#7696d6", "#c2c3c4", "#d67676"), 
                     labels = c("Downregulated", "Not significant", "Upregulated")) +
  ggtitle("Volcano plot of CHOL DEGs") +
  theme(plot.title = element_text(hjust=0.5, face="bold"), legend.position = "right") +
  guides(color = guide_legend(title = "Differential Expression"))
  
# print pdf
pdf("volcano_CHOL.pdf", width = 6.76, height = 4.54)
print(volcano, newpage = F)
dev.off()


# volcano plot containing only the IRE1 sign. genes
volcano_df_IRE1 <- volcano_df %>% drop_na(Component)
volcano_IRE1 <- ggplot(data = volcano_df_IRE1, aes(x = logFC, y = -log10(PValue), col = Component)) +
  theme_pubr() +
  geom_vline(xintercept = c(0.5, -0.5), col = 'gray', linetype = 'dashed') +
  geom_hline(yintercept = c(-log10(0.05)), col = 'gray', linetype = 'dashed') +
  geom_point(aes(col = Component), size = 2) +
  scale_color_manual(values = c("#fa9b2f", "#3cc1fa", "#ae82f5"), 
                     labels = c("ERN1", "RIDD", "XBP1")) +
  ggtitle("Volcano plot of IRE1 sign. genes in CHOL") +
  theme(plot.title = element_text(hjust=0.5, face="bold"), legend.position = "right") +
  guides(color = guide_legend(title = "Component")) +
  geom_text(label = volcano_df_IRE1$gene_name,
            #nudge_x=0.8,
            nudge_y=1,
            nudge_x = -0.2,
            color = "#363535",
            size = 3,
            check_overlap = T)

# print pdf
pdf("volcano_CHOL_IRE1.pdf", width = 6.76, height = 4.54)
print(volcano_IRE1, newpage = F)
dev.off()
