DOCKER_USER:=agasson
DOCKER_IMAGE:=environment
DOCKER_IMAGE_TAG:=archlinux

image:
	docker build --no-cache -t "$(DOCKER_IMAGE):$(DOCKER_IMAGE_TAG)" .

push:
	docker login -u $(DOCKER_USER)
	docker push "$(DOCKER_IMAGE):$(DOCKER_IMAGE_TAG)"

.PHONY: image push
