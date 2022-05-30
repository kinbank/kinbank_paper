KINBANK_REPO=https://github.com/kinbank/kinbank.git
KINBANK=../kinbank

clean:
	rm -rf ../kinbank processed_data results figures

help:
	@echo 1. make collect_data : downloads correct version of KINBANK
	@echo 2. make clean_data : cleans data into format for mother - father analyses, which is then manually coded. creates & prunes phylogenies. 
	@echo 3. make analyse : runs brms model for mother-father analyses. creates figure.
	
$(KINBANK):
	mkdir -p ../kinbank
	git clone $(KINBANK_REPO) ../kinbank

collect_data: $(KINBANK)
	cd $(KINBANK) && git checkout 45ae459b755174781b133844cde96da995269e50
	
# 18ecc23fdb80ac1681904c87fc1995497cb956e8$
	
clean_data: 
	mkdir processed_data/
	RScript processing/get-data.R
	@echo After processing kinbank data, it is manually coded for the first syllable sound. 
	RScript processing/prune_jaeger.R
	
analyse:
	mkdir results/
	mkdir figures/
	Rscript brms_models_jaeger.R
	@echo Calculate interreliability statistics
	RScript get_interreliabilitydata.R
	RScript irr.R
