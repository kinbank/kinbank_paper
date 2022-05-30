KINBANK_REPO=https://github.com/SimonGreenhill/kinbank.git
KINBANK=../kinbank

clean: 
	rm -rf results processed_data figures

help:
	@echo 1. make collect_data : this gets kinbank data and checks out the right version. This may have already been done in the mother-father analyses.
	@echo 2. make clean_data : cleans data into format for Bayestraits, which is then manually coded. creates and prunes phylogenies. 
	@echo 3. make analyse : runs brms model for cross-cousin phylogenetic analyses. creates figure.

$(KINBANK):
	mkdir -p ../kinbank
	git clone $(KINBANK_REPO) ../kinbank

collect_data: $(KINBANK)
	cd $(KINBANK) && git checkout 18ecc23fdb80ac1681904c87fc1995497cb956e8$

clean_data:
	mkdir -p processed_data/
	Rscript processing/get-data.R
	@echo After creating bantu data, it is manually coded for bifurcate merging terminology
	Rscript processing/create_bt_data.R
	
analyse:
	mkdir -p results/
	@echo First, get_tags creates an output which reconstructs the ancestral state at all internal nodes of the phylogeny. This is copied into the BT analysis file manually
	RScript analysis/get_tags.R
	analysis/all.sh
	analysis/make_tree.R