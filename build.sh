VERSION=v1.0.1

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
