VERSION=v1.0.16

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
