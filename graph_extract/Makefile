
graph_extract.exe:
	dune build bin/graph_extract.exe --profile release
	cp _build/default/bin/graph_extract.exe .

rss_extract.exe:
	dune build bin/rss_extract.exe --profile release
	cp _build/default/bin/rss_extract.exe .

word_map.exe:
	dune build bin/word_map.exe --profile release
	cp _build/default/bin/word_map.exe .

graph.bin: graph_extract.exe
	touch graph.bin
	./graph_extract.exe /mnt/lappy/itunes/ /mnt/lappy/soundcloud /mnt/lappy/podbean /mnt/tiny/podbean urls.txt graph.bin

cluster_ids.npy: graph.bin
	OPENBLAS_NUM_THREADS=1 python3 cluster.py

cluster_dists.bin pairwise_clusters.bin: cluster_ids.npy rss_extract.exe
	touch cluster_dists.bin
	./rss_extract.exe cluster_ids.npy cluster_dists.bin pairwise_clusters.bin cluster_vecs.bin `ls /mnt/lappy/rss_warc/*.warc.gz`

clusters_2d.bin: pairwise_clusters.bin
	python3 mds.py
 
word_map.bin: cluster_dists.bin clusters_2d.bin word_map.exe
	./word_map.exe cluster_dists.bin clusters_2d.bin 1200 1900 word_map.bin word_map_terms.txt
