IMAGE_NAME=benchmarksgame-cvs-importer

info:
	$(info Targets)
	$(info -------------------------------------------------------------)
	$(info update            | update this repository with data from cvs)
	$(info -Other Targets ----------------------------------------------)
	$(info docker-image      | build the image used to run the csv import)
	$(info docker-run        | run the image interactively)
	$(info docker-cvs-import | run the import and update this repository)

.image.ok:
	cd etc/docker && docker build -t $(IMAGE_NAME) .
	touch $@

docker-image: .image.ok
docker-run: docker-image
	docker run -it $(IMAGE_NAME)

docker-cvs-import: docker-image
	docker run -v $$PWD:/repo -v $$PWD/.docker-home:/root -w /repo $(IMAGE_NAME) ./update-git-from-cvs-in-docker.sh
	./update-git-from-cvs.sh


update: docker-cvs-import

