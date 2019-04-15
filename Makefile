SHELL := /bin/bash
data_dir := /mnt/lappy2
soundcloud_dir := $(data_dir)/soundcloud
itunes_dir := $(data_dir)/itunes

$(soundcloud_dir)/soundcloud_graph.tsv.gz:
	find $(soundcloud_dir) -name '*.jsons.xz' |\
		parallel -P 60% ./soundcloud_pairs.sh |\
		hashuniq |\
		gzip -c > $(soundcloud_dir)/soundcloud_graph.tsv.gz

$(itunes_dir)/itunes_graph.tsv.gz:
	find $(itunes_dir) -name '*.jsons.xz' |\
		parallel -P60% ./itunes_pairs.sh |\
		hashuniq |\
		gzip -c > $(itunes_dir)/itunes_graph.tsv.gz

$(itunes_dir)/urls.txt:
	find $(itunes_dir) -name '*.jsons.xz' |\
		parallel -P60% ./itunes_urls.sh |\
		hashuniq |\
		sort -t' ' -k 2,2 \
		> $(itunes_dir)/urls.txt

$(soundcloud_dir)/urls.txt:
	find $(soundcloud_dir) -name '*.jsons.xz' |\
		parallel -P60% ./soundcloud_get_links.sh |\
		hashuniq |\
		sort -t' ' -k 2,2 \
		> $(soundcloud_dir)/urls.txt

$(data_dir)/url_join.txt: $(soundcloud_dir)/urls.txt $(itunes_dir)/urls.txt
	join -t ' ' -1 2 -2 2 $(soundcloud_dir)/urls.txt $(itunes_dir)/urls.txt > $(data_dir)/url_join.txt

$(data_dir)/combined_graph.tsv.gz: $(data_dir)/url_join.txt $(itunes_dir)/itunes_graph.tsv.gz $(soundcloud_dir)/soundcloud_graph.tsv.gz
	gunzip -c $(itunes_dir)/itunes_graph.tsv.gz | ./combine_ids.py  $(data_dir)/url_join.txt | gzip -c | cat $(soundcloud_dir)/soundcloud_graph.tsv.gz - > $(data_dir)/combined_graph.tsv.gz

$(soundcloud_dir)/soundcloud_svd_128.npy: $(data_dir)/combined_graph.tsv.gz 
	./soundcloud_mat.py $(soundcloud_dir) $(data_dir)/combined_graph.tsv.gz

all: $(soundcloud_dir)/soundcloud_svd_128.npy
.PHONY: all
