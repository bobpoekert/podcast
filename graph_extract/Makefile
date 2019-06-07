
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

doc_clusters.exe:
	dune build bin/doc_clusters.exe 
	cp _build/default/bin/doc_clusters.exe .

graph.bin: graph_extract.exe
	touch graph.bin
	./graph_extract.exe /mnt/lappy/itunes/ /mnt/lappy/soundcloud /mnt/lappy/podbean /mnt/tiny/podbean urls.txt graph.bin

cluster_ids.npy pairwise_clusters.bin: graph.bin
	OPENBLAS_NUM_THREADS=1 python3 cluster.py

rss_raw.txt: rss_extract.exe 
	./rss_extract.exe _rss_raw.txt `ls /mnt/lappy/rss_warc/*.warc.gz`
	cat _rss_raw.txt.* | hashuniq > rss_raw.txt 
	rm _rss_raw.txt.*
	sort -o rss_raw.txt rss_raw.txt

doc_vec.bin doc_cluster_dists.bin: rss_raw.txt 
	python3 docvec.py rss_raw.txt doc_vec.bin doc_cluster_dists.bin

clusters_2d.bin: pairwise_clusters.bin 
	python3 mds.py

word_map.bin: word_map.exe clusters_2d.bin doc_cluster_dists.bin
	./word_map.exe doc_cluster_dists.bin clusters_2d.bin word_map.bin 768 1024

