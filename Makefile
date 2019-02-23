SHELL := /bin/bash
data_dir := /mnt/exthdd/soundcloud

$(data_dir)/soundcloud_graph.tsv.gz:
	./soundcloud_pairs.sh `find $(data_dir) -name '*.jsons.xz'` | gzip -c > $(data_dir)/soundcloud_graph.tsv.gz

$(data_dir)/soundcloud_svd_128.npy: $(data_dir)/soundcloud_graph.tsv.gz 
	./soundcloud_mat.py $(data_dir)

all: $(data_dir)/soundcloud_svd_128.npy
.PHONY: all
