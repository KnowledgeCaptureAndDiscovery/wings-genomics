## Instructions for using the Docker image.

Docker image is available in DockerHub under kcapd/wings-genomics:latest

Alternatively, an archived image is available in Zenodo: http://doi.org/10.5281/zenodo.4612080


### Download and run the Docker image:

- Download and install docker from: https://www.docker.com/

- Start Docker

- Download the image: docker pull kcapd/wings-genomics:latest. If you use this archived image, you will have to execute: docker load -i $path where $path is the path to the downloaded image.

- Run the image: docker run -p 8080:8080 -d kcapd/wings-genomics-full:latest

- In a browser, go to: http://localhost:8080/wings-portal/

- Username: admin, password: 4dm1n!23

- On WINGS portal, go to “Analysis > Run Workflows”



### Executing multiomic workflows on Wings:

- MultiProteomic_SpectralLibSearch - Stage3 reanalysis workflow

- SingleProteogenomicSearch - Proteogenomic reanalysis workflow

- SingleProteomicSearch_ITRAQ - Proteomic workflow for iTRAQ data

- SingleProteogenomicSearch_ITRAQ - Proteogenomic workflow for iTRAQ data



Note: Refer to Tables 1 and 2 below from the “DockerWorkflowFilesDescription” document for input and output files descriptions.



#### MultiProteomic_SpectralLibSearch: This workflow closely resembles the steps from the original proteomic analysis and was used for stage 3 reanalysis. Uses Myrimatch and MSGPlus for peptide searches and Pepitome for spectral library based peptide searches. Results from the three mentioned search engines are merged together to assemble proteins.



#### Input files:

- ProtDataZip: SampleData1_mzML.zip

- sampleIDs: SampleIDs.txt

- ref_proteome_fasta: humanRefSeq_Version54_with_tryp_DECOY.fasta

- spectralLibFile: human_2011_05_26_it_decoy.sptxt

- pepSearchParams: ZhangMM_Exact.params, ZhangMSGFPlus_Exact.params (shift select both)

- pepSpectralSearchParams: ZhangPepitome_Exact.params

- pepConvertParams: toPepXML.params

- idpQonvertParams: idpqonvert.params

- idpQonvertGeneParams: idpqonvert_gene.params

- condFile: NOplex.condition.xml

- protAssemblyParams: idpassemble_0.001.params

- protAssemblyParams_redone: idpassemble_0.01.params

- protConvertParams: idpQueryProt.Params

- protConvertParams_withGene: idpQueryGene.Params



#### Output files (Required):

- ref_protConvertSpectra: Protein groups results file

- ref_protConvertSpectraWithGene: Gene groups results file

    - SingleProteogenomicSearch: This workflow replicates the steps of the proteogenomics analysis from the original analysis and was generates single amino acid variants (SAAVs) and variant peptides.


#### Input files:

- ProtData: SampleData1_mzML.zip

- RNAData: SampleData1_fastq.zip

- sampleIDs: SampleIDs.txt

- RefFastaAnnot: hg19_cpdb_bundle.zip

- cufflinksParams: cufflinks.params

- decoyParams: decoyFASTA.params

- fastqToBamParams: tophat2.params

- genesAnnotFile: hg19_genes.gtf

- genomicsFASTA: hg19.fa (or test.fa)

- refFastaBundle: hg19_tux_bundle.zip

- refFastaBundleRSEM: hg19_RSEM_bundle.zip

- rsemParams: rsem_SE.params

- pepSearchParams: ZhangMM_Exact.params

- pepConvertParams: toPepXML.params

- idpQonvertParams: idpqonvert.params

- idpQonvertGeneParams: idpqonvert_gene.params

- condFile: NOplex.condition.xml

- protAssemblyParams: idpassemble_0.001.params

- protAssemblyParams_redone: idpassemble_0.01.params

- protConvertParams: idpQueryProt.Params

- protConvertParams_withGene: idpQueryGene.Params

- protConvertParams_pepLevel: idpQueryPep.Params



#### Output files (Required):

- custom_protConvertSpectra_redone: Protein groups results file

- custom_protConvertSpectraWithGene_redone: Gene groups results file

- custom_protConvertSpectra_pepData: Peptide results file



#### SingleProteomicSearch_ITRAQ: This generic workflow processes proteomics iTRAQ data using MSGFPlus search engine.



#### Input files:

- ProtDataZip: SampleData_iTRAQ_mzML.zip

- sampleIDs: SampleIDs_iTRAQ.txt

- ref_proteome_fasta: humanRefSeq_Version54_with_tryp_DECOY.fasta

- pepSearchParams: MSGFPlus_iTRAQ4plex.params

- pepConvertParams: toPepXML.params

- idpQonvertParams: idpqonvert.params

- idpQonvertGeneParams: idpqonvert_gene.params

- idpQonvertParams_Quant: idpqonvert_iTRAQ4plex.params

- condFile: NOplex.condition.xml

- condFile_withQuantData: iTRAQ4plex_idpicker.params

- protAssemblyParams: idpassemble_0.01.params

- protAssemblyParams_redone: idpassemble_0.01.params

- protAssemblyParams_withQuantData: idpassemble_0.01_iTRAQ4plex.params

- protConvertParams: iTRAQ4plex_idpQueryProt.params

- protConvertParams_withGene: iTRAQ4plex_idpQueryGene.params



#### Output files (Required):

- ref_protConvertSpectra: Protein groups results file

- ref_protConvertSpectraWithGene: Gene groups results file



#### SingleProteogenomicSearch_ITRAQ: This generic workflow processes proteogenomics iTRAQ data using MSGFPlus search engine.



#### Input files:

- ProtData: SampleData_iTRAQ_mzML.zip

- RNAData: SampleData1_fastq.zip

- sampleIDs: SampleIDs_iTRAQ.txt

- RefFastaAnnot: hg19_cpdb_bundle.zip

- cufflinksParams: cufflinks.params

- decoyParams: decoyFASTA.params

- fastqToBamParams: tophat2.params

- genesAnnotFile: hg19_genes.gtf

- genomicsFASTA: hg19.fa (or test.fa)

- refFastaBundle: hg19_tux_bundle.zip

- refFastaBundleRSEM: hg19_RSEM_bundle.zip

- rsemParams: rsem_SE.params

- pepSearchParams: ZhangMM_Exact.params

- pepConvertParams: toPepXML.params

- idpQonvertParams: idpqonvert.params

- idpQonvertGeneParams: idpqonvert_gene.params

- idpQonvertParams_Quant: idpqonvert_iTRAQ4plex.params

- condFile: NOplex.condition.xml

- condFile_withQuantData: iTRAQ4plex_idpicker.params

- protAssemblyParams: idpassemble_0.01.params

- protAssemblyParams_redone: idpassemble_0.01.params

- protAssemblyParams_withQuantData: idpassemble_0.01_iTRAQ4plex.params

- protConvertParams: iTRAQ4plex_idpQueryProt.params

- protConvertParams_withGene: iTRAQ4plex_idpQueryGene.params

- protConvertParams_pepLevel: iTRAQ4plex_idpQueryPep.params



#### Output files (Required):

- custom_protConvertSpectra_redone: Protein groups results file

- custom_protConvertSpectraWithGene_redone: Gene groups results file

- custom_protConvertSpectra_pepData: Peptide results file





### Executing metaworkflows on WINGS to reproduce figures from the original paper:

Metaworkflows typically accept “Run ID(s)” from the multiomic workflows as input. Below are the metaworkflows necessary to reproduce the figures from the original paper

- Figure 2: MetaAnalysis_OriginalVsReanalysis

- Figure 3: MetaAnalysis_ParamSensitivity

- Figure 4: MetaAnalysis_AdditionalData

- Figure 5: MetaAnalysis_NewData



Note: Refer to Tables 3 and 5 below from the “DockerWorkflowFilesDescription” document for metaworkflow input file and output file descriptions. For metaworkflow parameter descriptions, refer to Table 1 from the “DockerWorkflowParametersDescription.docx” document.



### Metaworkflows with files as input for simplicity

- Figure 2: MetaAnalysis_OriginalVsReanalysisFiles



#### Input files:

- OriginalProtFile: ProteinFile_Deposited.tsv

- ReanalysisProtFile: ProteinFile_Stage3.tsv

- ORSEMNormWideFile: RSEMNormMatrix_Original.tsv

- ReRSEMNormWideFile: RSEMNormMatrix_Reanalysis.tsv

- SampleIDs: SampleIDs_CRC95.txt



#### Input Parameters:

- DatasetsStr: Original,Reanalysis

- ODatasetName: Original

- OProtRSEMCorrByGeneColname: Original_ProteinRNA_Corr_ByGene

- OProtRSEMCorrBySampleColname: Original_ProteinRNA_Corr_BySample

- ReProtRSEMCorrByGeneColname: Reanalysis_ProteinRNA_Corr_ByGene

- ReProtRSEMCorrBySampleColname: Reanalysis_ProteinRNA_Corr_BySample

- ReDatasetName: Reanalysis

- OReProtRSEMByGeneSplot: Original_Reanalysis_ProteinRNA_Corr_ByGene

- OReProtRSEMBySampleSplot: Original_Reanalysis_ProteinRNA_Corr_BySample

- OReProtCorrColname: Original_Reanalysis_ProteinCorrelation

- OReProtDPlotTitle: Original_Reanalysis_ProteinCorrelation

- OReRSEMCorrColname: Original_Reanalysis_RNACorrelation

- OReRSEMDPlotTitle: Original_Reanalysis_RNACorrelation

- vennTitle_Proteins: Original_Reanalysis_ProteinGroups



#### Output plots (required):

- OReProtDensityPlot: Density plot that illustrates the protein correlation between deposited and stage 3 reanalysis datasets

- OReProtRSEMByGeneSPlot: Scatter plot that illustrates mRNA-protein correlation per gene, between deposited and stage 3 reanalysis datasets

- OReProtRSEMBySampleSPlot: Scatter plot that illustrates mRNA-protein correlation per sample, between deposited and stage 3 reanalysis datasets

- OReProtsBarplot: Barplot showing protein count for each sample per dataset

- OReRSEMDensityPlot: Density plot illustrating correlation of RNA measures between deposited and stage 3 reanalysis datasets

- vennProtIDs: Venn diagram of protein groups in deposited and stage 3 reanalysis datasets


#### Figure 3: MetaAnalysis_ParamSensitivityFiles



##### Input files:

- OSAAVFile: SAAVs_Published.tsv

- ReSAAVFile: SAAVs_Stage3.tsv

- OVariantPepFile: VariantPeptides_Published.tsv

- ReVariantPepFile: VariantPeptides_Stage3.tsv

- RNAProtPaired_SampleIDs: ZhangPGPaired_91SampleIDs.txt

- ReCustomProtFile: ProteinFile_CustomDB.tsv

- SampleIDs: SampleIDs_CRC95.txt

- MyrimatchProtFile1: ProteinFile_Myrimatch1.tsv

- MyrimatchProtFile2: ProteinFile_Myrimatch2.tsv

- MyrimatchPepFile1: Peptide_Myrimatch1.tsv

- MyrimatchPepFile2: Peptide_Myrimatch2.tsv

- ProtFile1: ProteinFile_Search1.tsv

- ProtFile2: ProteinFile_Search2.tsv

- ProtFile3: ProteinFile_Search3.tsv

- ProtFile4: ProteinFile_Search4.tsv



#### Input Parameters:

- Myrimatch_DatasetsStr: Myrimatch1,Myrimatch2

- OReDatasetStr: Original,Reanalysis

- ReRefCustomMMDplot: Ref_Custom_ProteinCorrelation_Reanalysis

- SAAVColname: SAAV

- VariantPeptideColname: peptide_sequence

- datasetName1: Search1

- datasetName2: Search2

- datasetName3: Search3

- datasetName4: Search4

- datasetNameMyrimatch1: Myrimatch1

- datasetNameMyrimatch2: Myrimatch2

- datasetNameOProteogenomic: Original

- datasetNameReProteogenomic: Reanalysis

- pepseqColname: Sequence

- protAccessionsColname: Accessions

- vennTitle_PepMyrimatch: Peptides

- vennTitle_ProtMyrimatch: Protein_Groups

- vennTitle_SAAVs: SAAVs

- vennTitle_VariantPeptides: Variant_Peptides



#### Output plots (required):

- densityPlot: Plot showing the density distribution of protein correlations for overlapping proteins between reference and custom protein sequence database searches

- protsPerSearchBarplot: Bar plot depicting number of protein identifications across search engines

- vennOReSAAVs: Venn diagram of SAAVs in the deposited and stage3 analysis datasets.

Note: The SAAV Venn diagram needs manual intervention to produce accurate plot. For example, same reference protein IDs and SAAV IDs. The deposited and stage3 reanalysis differ in the protein IDs.

- vennOReVariantPeptides: Venn diagram of variant peptide sequences in the deposited and stage3 analysis datasets.

- vennPepMyrimatch: Venn diagram of peptide sequences in two Myrimatch searches.

- vennProtMyrimatch: Venn diagram of protein groups in two Myrimatch searches.



#### Figure 4: MetaAnalysis_AdditionalDataFiles



#### Input files:

- ProtFile1: ProteinFile1_95Samples.tsv

- ProtFile2: ProteinFile2_100Samples.tsv

- ProtFile3: ProteinFile3_105Samples.tsv

- ProtFile4: ProteinFile4_110Samples.tsv

- ProtFile5: ProteinFile5_115Samples.tsv

- SampleIDs1: SampleIDs_CRC95.txt

- SampleIDs2: SampleIDs_CRC95_CO_5.txt

- SampleIDs3: SampleIDs_CRC95_CO_10.txt

- SampleIDs4: SampleIDs_CRC95_CO_15.txt

- SampleIDs5: SampleIDs_CRC95_CO_20.txt

- sampleDupAnn1: CRC_sampleAnn.txt

- sampleDupAnn2: CRC_sampleAnn100.txt

- sampleDupAnn3: CRC_sampleAnn105.txt

- sampleDupAnn4: CRC_sampleAnn110.txt

- sampleDupAnn5: CRC_sampleAnn115.txt

- batchesFile: CRC_BatchAnn115.txt

- dataSubtypeAnn: CRC_subtypeAnn.txt



#### Input Parameters:

- datasetName1: CRC95

- datasetName2: CRC100

- datasetName3: CRC105

- datasetName4: CRC110

- datasetName5: CRC115



#### Output plots (required):

- upsetPlot: UpSet Plot showing protein intersections and counts within and across datasets.



#### Figure 5: MetaAnalysis_NewDataFiles



#### Input files:

- ProtFile1: ProteinFile1_95Samples.tsv

- ProtFile2: ProteinFile_NCI60.tsv

- ProtFile3: ProteinFile_OV.tsv

- ProtFile4: ProteinFile_BRCA.tsv

- SAAVFile1: SAAVs_Stage3.tsv

- SAAVFile2: SAAVs_OV.tsv

- SAAVFile3: SAAVs_BRCA.tsv

- SampleIDs1: SampleIDs_CRC95.txt

- SampleIDs2: SampleIDs_NCI60.txt

- SampleIDs3: SampleIDs_OV.txt

- SampleIDs4: SampleIDS_BRCA.txt



#### Input Parameters:

- DatasetsStr: CRC95,NCI60,OV,BRCA

- SAAVColname: SAAV

- datasetName1: CRC95

- datasetName2: NCI60

- datasetName3: OV

- datasetName4: BRCA



#### Output plots:

- barplotProtsPerDataset: Barplot depicting protein counts for CRC95,NCI60,OV and BRCA datasets

- barplotSAAVsPerDataset: Barplot depicting SAAV counts for CRC95,OV and BRCA datasets

- vennSAAVs: Venn diagram of SAAVs in CRC95,OV,BRCA datasets



Note: Refer to Tables 3, 4 and 5 from the “DockerWorkflowFilesDescription” document for input and output files description.



In addition to the above-mentioned meta workflows, the docker also contains few workflows starting with “MetaAnalysisCompare”. These “MetaAnalysisCompare” workflows are subsets of the “MetaAnalysis” workflows listed above. These are available independently to ease individual comparison between datasets.
