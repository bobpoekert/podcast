SHELL := /bin/bash
data_dir := /mnt/lappy2/soundcloud

$(data_dir)/soundcloud_graph.tsv.gz:
	find $(data_dir) -name '*.jsons.xz' |\
		parallel ./soundcloud_pairs.sh |\
		hashuniq |\
		gzip -c > $(data_dir)/soundcloud_graph.tsv.gz

$(data_dir)/soundcloud_svd_128.npy: $(data_dir)/soundcloud_graph.tsv.gz 
	./soundcloud_mat.py $(data_dir)

all: $(data_dir)/soundcloud_svd_128.npy
.PHONY: all
