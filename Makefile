SHELL := /bin/bash
data_dir := /mnt/ssd/soundcloud

soundcloud_graph.tsv.gz:
	./soundcloud_decode `ls $(data_dir)`

soundcloud_svd_128.npy: soundcloud_graph.tsv.gz 
	python soundcloud_mat.py $(data_dir)
