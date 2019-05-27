
graph_extract.exe:
	dune build bin/graph_extract.exe
	cp _build/default/bin/graph_extract.exe .

rss_extract.exe:
	dune build bin/rss_extract.exe
	cp _build/default/bin/rss_extract.exe .

graph.bin: graph_extract.exe
	touch graph.bin
	./graph_extract.exe /mnt/lappy/itunes/ /mnt/lappy/soundcloud /mnt/lappy/podbean /mnt/tiny/podbean urls.txt graph.bin

cluster_ids.npy: graph.bin
	OPENBLAS_NUM_THREADS=1 python3 cluster.py

cluster_dists.bin: cluster_ids.npy rss_extract.exe
	touch cluster_dists.bin
	./rss_extract.exe cluster_ids.npy cluster_dists.bin `ls /mnt/lappy/rss_warc/*.warc.gz`
