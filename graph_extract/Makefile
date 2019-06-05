
graph_extract.exe:
	dune build bin/graph_extract.exe
	cp _build/default/bin/graph_extract.exe .

rss_extract.exe:
	dune build bin/rss_extract.exe
	cp _build/default/bin/rss_extract.exe .

word_map.exe:
	dune build bin/word_map.exe
	cp _build/default/bin/word_map.exe .

extract_words.exe:
	dune build bin/extract_words.exe
	cp _build/default/bin/extract_words.exe .

graph.bin: graph_extract.exe
	touch graph.bin
	./graph_extract.exe /mnt/lappy/itunes/ /mnt/lappy/soundcloud /mnt/lappy/podbean /mnt/tiny/podbean urls.txt graph.bin

cluster_ids.npy: graph.bin
	OPENBLAS_NUM_THREADS=1 python3 cluster.py

cluster_dists.bin pairwise_clusters.bin: cluster_ids.npy rss_extract.exe
	touch cluster_dists.bin
	./rss_extract.exe cluster_ids.npy cluster_dists.bin pairwise_clusters.bin bow_vecs.bin `ls /mnt/lappy/rss_warc/*.warc.gz`

bow_clusters.bin: cluster_dists.bin 
	sh -c 'python3 word_clusters.py `ls bow_vecs.bin*`'

clusters_2d.bin: pairwise_clusters.bin
	python3 mds.py
 
word_map.bin: cluster_dists.bin clusters_2d.bin word_map.exe
	./word_map.exe bow_clusters.bin clusters_2d.bin 768 1024 word_map.bin 

words.txt: cluster_dists.bin extract_words.exe
	sh -c './extract_words.exe cluster_dists.bin | hashuniq > words.txt'

map_words.txt: words.txt word_map.bin
	sh -c 'python3 map_words.py 768 1024 > map_words.txt'
