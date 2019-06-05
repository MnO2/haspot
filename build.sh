VERSION=v1.0.26

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
