VERSION=v1.0.10

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
