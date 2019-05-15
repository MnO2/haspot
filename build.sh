VERSION=v1.0.0

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
